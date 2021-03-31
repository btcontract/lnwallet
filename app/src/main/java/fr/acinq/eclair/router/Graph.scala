/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.router

import fr.acinq.eclair._
import fr.acinq.eclair.router.Router._
import scala.collection.JavaConversions._
import fr.acinq.eclair.router.Graph.GraphStructure.{DirectedGraph, GraphEdge}
import fr.acinq.bitcoin.{Btc, MilliBtc}

import fr.acinq.bitcoin.Crypto.PublicKey
import scala.collection.mutable


object Graph {
  /**
   * The cumulative weight of a set of edges (path in the graph).
   *
   * @param costs   amount to send to the recipient + each edge's fees per hop
   * @param length number of edges in the path
   * @param cltv   sum of each edge's cltv
   */
  case class RichWeight(costs: List[MilliSatoshi], length: Int, cltv: CltvExpiryDelta, weight: Double) extends Ordered[RichWeight] {
    override def compare(that: RichWeight): Int = weight.compareTo(that.weight)
  }

  case class WeightedNode(key: PublicKey, weight: RichWeight)

  case class WeightedPath(path: Seq[GraphEdge], weight: RichWeight)

  /**
   * This comparator must be consistent with the "equals" behavior, thus for two weighted nodes with the same weight we distinguish them by their public key
   * See https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html
   */
  object NodeComparator extends Ordering[WeightedNode] {
    override def compare(x: WeightedNode, y: WeightedNode): Int = {
      val weightCmp = x.weight.compareTo(y.weight)
      if (weightCmp == 0) x.key.toString.compareTo(y.key.toString)
      else weightCmp
    }
  }

  implicit object PathComparator extends Ordering[WeightedPath] {
    override def compare(x: WeightedPath, y: WeightedPath): Int = y.weight.compare(x.weight)
  }

  /**
   * @param graph                         the graph on which will be performed the search
   * @param sourceNode                    the starting node of the path we're looking for (payer)
   * @param targetNode                    the destination node of the path (recipient)
   * @param amount                        amount to send to the last node
   * @param ignoredEdges                  channels that should be avoided
   * @param ignoredVertices               nodes that should be avoided
   * @param boundaries                    a predicate function that can be used to impose limits on the outcome of the search
   */
  def bestPath(graph: DirectedGraph,
               sourceNode: PublicKey, targetNode: PublicKey, amount: MilliSatoshi, ignoredEdges: Set[ChannelDesc],
               ignoredVertices: Set[PublicKey], boundaries: RichWeight => Boolean): Option[WeightedPath] = {

    val latestBlockExpectedStampMsecs = System.currentTimeMillis
    val targetWeight = RichWeight(List(amount), length = 0, CltvExpiryDelta(0), weight = 0)
    val shortestPath = dijkstraShortestPath(graph, sourceNode, targetNode, ignoredEdges, ignoredVertices, targetWeight, boundaries, latestBlockExpectedStampMsecs)

    if (shortestPath.nonEmpty) {
      val weight = shortestPath.foldRight(targetWeight) { case (edge, prev) =>
        addEdgeWeight(sourceNode, edge, prev, latestBlockExpectedStampMsecs)
      }

      val weightedPath = WeightedPath(shortestPath, weight)
      Some(weightedPath)
    } else None
  }

  /**
   * Finds the shortest path in the graph, uses a modified version of Dijkstra's algorithm that computes the shortest
   * path from the target to the source (this is because we want to calculate the weight of the edges correctly). The
   * graph @param g is optimized for querying the incoming edges given a vertex.
   *
   * @param g                             the graph on which will be performed the search
   * @param sourceNode                    the starting node of the path we're looking for
   * @param targetNode                    the destination node of the path
   * @param ignoredEdges                  channels that should be avoided
   * @param ignoredVertices               nodes that should be avoided
   * @param initialWeight                 weight that will be applied to the target node
   * @param boundaries                    a predicate function that can be used to impose limits on the outcome of the search
   */
  private def dijkstraShortestPath(g: DirectedGraph, sourceNode: PublicKey, targetNode: PublicKey,
                                   ignoredEdges: Set[ChannelDesc], ignoredVertices: Set[PublicKey], initialWeight: RichWeight,
                                   boundaries: RichWeight => Boolean, latestBlockExpectedStampMsecs: Long): Seq[GraphEdge] = {

    val sourceNotInGraph = !g.containsVertex(sourceNode)
    val targetNotInGraph = !g.containsVertex(targetNode)

    // the graph does not contain source/destination nodes
    if (sourceNotInGraph || targetNotInGraph) return Seq.empty

    // conservative estimation to avoid over-allocating memory
    val initialCapacity = 100

    val bestWeights = new DefaultHashMap[PublicKey, RichWeight](RichWeight(List(Long.MaxValue.msat), Int.MaxValue, CltvExpiryDelta(Int.MaxValue), Double.MaxValue), initialCapacity)
    val bestEdges = new java.util.HashMap[PublicKey, GraphEdge](initialCapacity)

    // NB: we want the elements with smallest weight first, hence the `reverse`.
    val toExplore = mutable.PriorityQueue.empty[WeightedNode](NodeComparator.reverse)
    val visitedNodes = mutable.HashSet.empty[PublicKey]
    var targetFound = false

    // initialize the queue and cost array with the initial weight
    toExplore.enqueue(WeightedNode(targetNode, initialWeight))
    bestWeights.put(targetNode, initialWeight)

    while (toExplore.nonEmpty && !targetFound) {
      // node with the smallest distance from the target

      val current = toExplore.dequeue
      targetFound = current.key == sourceNode

      if (!targetFound && !visitedNodes.contains(current.key)) {
        visitedNodes += current.key

        // build the neighbors with optional extra edges
        g.getIncomingEdgesOf(current.key).foreach { edge =>
          val currentWeight = current.weight

          val currentCost = currentWeight.costs.head

          val neighborWeight = addEdgeWeight(sourceNode, edge, currentWeight, latestBlockExpectedStampMsecs)

          val canRelayAmount = currentCost <= edge.updExt.capacity && currentCost >= edge.updExt.update.htlcMinimumMsat

          val neighbor = edge.desc.from

          if (canRelayAmount && boundaries(neighborWeight) && !ignoredEdges.contains(edge.desc) && !ignoredVertices.contains(neighbor)) {
            val previousNeighborWeight = bestWeights.getOrDefaultValue(neighbor)
            // if this path between neighbor and the target has a shorter distance than previously known, we select it
            if (neighborWeight.weight < previousNeighborWeight.weight) {
              // update the best edge for this vertex
              bestEdges.put(neighbor, edge)
              // add this updated node to the list for further exploration
              toExplore.enqueue(WeightedNode(neighbor, neighborWeight)) // O(1)
              // update the minimum known distance array
              bestWeights.put(neighbor, neighborWeight)
            }
          }
        }
      } else {
        targetFound = true
      }
    }

    if (targetFound) {
      val edgePath = new mutable.ArrayBuffer[GraphEdge](RouteCalculation.ROUTE_MAX_LENGTH)
      var current = bestEdges.get(sourceNode)
      while (null != current) {
        edgePath += current
        current = bestEdges.get(current.desc.to)
      }
      edgePath
    } else {
      Seq.empty
    }
  }

  /**
   * Add the given edge to the path and compute the new weight.
   *
   * @param sender                        node sending the payment
   * @param edge                          the edge we want to cross
   * @param prev                          weight of the rest of the path
   * @param latestBlockExpectedStampMsecs expected timestamp of latest block
   */
  private def addEdgeWeight(sender: PublicKey, edge: GraphEdge, prev: RichWeight, latestBlockExpectedStampMsecs: Long): RichWeight = {
    import RoutingHeuristics._

    // Every edge is weighted by its routing success score, higher score adds less weight
    val successFactor = 1 - normalize(edge.updExt.score, SCORE_LOW, SCORE_HIGH)

    val factor = if (edge.updExt.useHeuristics) {
      val blockNum = ShortChannelId.blockHeight(edge.desc.shortChannelId)
      val chanStampMsecs = BLOCK_300K_STAMP_MSEC + (blockNum - BLOCK_300K) * AVG_BLOCK_INTERVAL_MSEC
      val ageFactor = normalize(chanStampMsecs, BLOCK_300K_STAMP_MSEC, latestBlockExpectedStampMsecs)

      // Every edge is weighted by channel capacity, larger channels add less weight
      val capFactor = 1 - normalize(edge.updExt.capacity.toLong, CAPACITY_CHANNEL_LOW.toLong, CAPACITY_CHANNEL_HIGH.toLong)

      // Every edge is weighted by its cltv-delta value, normalized
      val cltvFactor = normalize(edge.updExt.update.cltvExpiryDelta.toInt, CLTV_LOW, CLTV_HIGH)

      ageFactor + capFactor + cltvFactor + successFactor
    } else {
      // Max out all heuristics except success rate on assisted and hosted channels
      // this makes these channels less likely to be used for routing at first
      1 + 1 + 1 + successFactor
    }

    val totalCost = if (edge.desc.from == sender) prev.costs else addEdgeFees(edge, prev.costs.head) +: prev.costs

    val totalCltv = if (edge.desc.from == sender) prev.cltv else prev.cltv + edge.updExt.update.cltvExpiryDelta

    // Every heuristic adds 0 - 100 imgainary SAT to edge weight (which is based on fee cost in msat), the better heuristic is the less SAT it adds
    val totalWeight = if (edge.desc.from == sender) prev.weight else prev.weight + totalCost.head.toLong + factor * 100000L

    RichWeight(totalCost, prev.length + 1, totalCltv, totalWeight)
  }

  /**
   * Calculate the minimum amount that the start node needs to receive to be able to forward @amountWithFees to the end node.
   *
   * @param edge            the edge we want to cross
   * @param amountToForward the value that this edge will have to carry along
   * @return the new amount updated with the necessary fees for this edge
   */
  private def addEdgeFees(edge: GraphEdge, amountToForward: MilliSatoshi): MilliSatoshi = {
    amountToForward + nodeFee(edge.updExt.update.feeBaseMsat, edge.updExt.update.feeProportionalMillionths, amountToForward)
  }

  object RoutingHeuristics {
    val BLOCK_300K: Int = 300000

    val BLOCK_300K_STAMP_MSEC: Long = 1399680000000L

    val AVG_BLOCK_INTERVAL_MSEC: Long = 10 * 60 * 1000L

    val CAPACITY_CHANNEL_LOW: MilliSatoshi = MilliBtc(10).toMilliSatoshi

    val CAPACITY_CHANNEL_HIGH: MilliSatoshi = Btc(10).toMilliSatoshi

    val CLTV_LOW = 9

    val CLTV_HIGH = 2016

    val SCORE_LOW = 1

    val SCORE_HIGH = 1000

    /**
     * Normalize the given value between (0, 1). If the @param value is outside the min/max window we flatten it to something very close to the
     * extremes but always bigger than zero so it's guaranteed to never return zero
     */
    def normalize(value: Double, min: Double, max: Double): Double =
      if (value <= min) 0D else if (value >= max) 1D else (value - min) / (max - min)
  }

  object GraphStructure {
    case class DescAndCapacity(desc: ChannelDesc, capacity: MilliSatoshi)

    case class GraphEdge(desc: ChannelDesc, updExt: ChannelUpdateExt) {
      def toDescAndCapacity: DescAndCapacity = DescAndCapacity(desc, updExt.capacity)
    }

    /** A graph data structure that uses an adjacency list, stores the incoming edges of the neighbors */
    case class DirectedGraph(vertices: Map[PublicKey, List[GraphEdge]]) {
      /**
       * Adds an edge to the graph. If one of the two vertices is not found it will be created.
       *
       * @param edge the edge that is going to be added to the graph
       * @return a new graph containing this edge
       */
      def addEdge(edge: GraphEdge, checkIfContains: Boolean = true): DirectedGraph = {
        val vertexIn = edge.desc.from
        val vertexOut = edge.desc.to
        // the graph is allowed to have multiple edges between the same vertices but only one per channel
        if (checkIfContains && containsEdge(edge.desc)) {
          removeEdge(edge.desc).addEdge(edge) // the recursive call will have the original params
        } else {
          val withVertices = addVertex(vertexIn).addVertex(vertexOut)
          val vertices1 = edge +: withVertices.vertices(vertexOut)
          DirectedGraph(withVertices.vertices.updated(vertexOut, vertices1))
        }
      }

      /**
       * Removes the edge corresponding to the given pair channel-desc/channel-update,
       * NB: this operation does NOT remove any vertex
       *
       * @param desc the channel description associated to the edge that will be removed
       * @return a new graph without this edge
       */
      def removeEdge(desc: ChannelDesc): DirectedGraph =
        if (containsEdge(desc)) {
          val v1 = vertices(desc.to).filterNot(_.desc == desc)
          DirectedGraph(vertices.updated(desc.to, v1))
        } else {
          this
        }

      def removeEdges(descList: Iterable[ChannelDesc]): DirectedGraph = descList.foldLeft(this)(_ removeEdge _)

      def addEdges(edges: Iterable[GraphEdge]): DirectedGraph = edges.foldLeft(this)(_ addEdge _)

      /**
       * @param keyB the key associated with the target vertex
       * @return all edges incoming to that vertex
       */
      def getIncomingEdgesOf(keyB: PublicKey): Seq[GraphEdge] = vertices.getOrElse(keyB, List.empty)

      /**
       * Adds a new vertex to the graph, starting with no edges
       */
      def addVertex(key: PublicKey): DirectedGraph = vertices.get(key) match {
        case None => DirectedGraph(vertices.updated(key, List.empty))
        case _ => this
      }

      /**
       * @return true if this graph contain a vertex with this key, false otherwise
       */
      def containsVertex(key: PublicKey): Boolean = vertices.contains(key)

      /**
       * @return true if this edge desc is in the graph. For edges to be considered equal they must have the same in/out vertices AND same shortChannelId
       */
      def containsEdge(desc: ChannelDesc): Boolean = {
        vertices.get(desc.to) match {
          case None => false
          case Some(adj) => adj.exists(neighbor => neighbor.desc.shortChannelId == desc.shortChannelId && neighbor.desc.from == desc.from)
        }
      }
    }

    object DirectedGraph {
      def apply(): DirectedGraph = new DirectedGraph(Map.empty)

      def apply(key: PublicKey): DirectedGraph = new DirectedGraph(Map(key -> List.empty))

      def apply(edge: GraphEdge): DirectedGraph = DirectedGraph().addEdge(edge)

      def apply(edges: Seq[GraphEdge]): DirectedGraph = DirectedGraph().addEdges(edges)

      def makeGraph(channels: Map[ShortChannelId, PublicChannel] = Map.empty): DirectedGraph = {
        // initialize the map with the appropriate size to avoid resizing during the graph initialization
        val mutableMap = new DefaultHashMap[PublicKey, List[GraphEdge]](List.empty, channels.size + 1)

        // add all the vertices and edges in one go
        channels.values.foreach { channel =>
          channel.update1Opt.foreach { u1 =>
            val desc1 = Router.getDesc(u1.update, channel.ann)
            mutableMap.put(desc1.to, GraphEdge(desc1, u1) :: mutableMap.getOrDefaultValue(desc1.to))
          }

          channel.update2Opt.foreach { u2 =>
            val desc2 = Router.getDesc(u2.update, channel.ann)
            mutableMap.put(desc2.to, GraphEdge(desc2, u2) :: mutableMap.getOrDefaultValue(desc2.to))
          }
        }

        DirectedGraph(mutableMap.toMap)
      }
    }
  }
}
