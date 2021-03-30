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
import fr.acinq.eclair.wire._
import fr.acinq.eclair.router.Graph.GraphStructure._
import fr.acinq.eclair.router.Graph.RichWeight
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.utils.Denomination
import scodec.bits.ByteVector


object ChannelUpdateExt {
  def fromUpdate(update: ChannelUpdate): ChannelUpdateExt = ChannelUpdateExt(update, Sync.getChecksum(update), score = 1L, useHeuristics = false)
}

case class ChannelUpdateExt(update: ChannelUpdate, crc32: Long, score: Long, useHeuristics: Boolean) {
  def withNewUpdate(cu: ChannelUpdate): ChannelUpdateExt = copy(crc32 = Sync.getChecksum(cu), update = cu)
  lazy val capacity: MilliSatoshi = update.htlcMaximumMsat.get
}

object Router {
  case class ChannelDesc(shortChannelId: ShortChannelId, from: PublicKey, to: PublicKey)

  case class RouterConf(routeHopDistance: Int, maxCltvDelta: CltvExpiryDelta, mppMinPartAmount: MilliSatoshi, maxChannelFailures: Int, maxStrangeNodeFailures: Int, maxRemoteAttempts: Int)

  case class PublicChannel(update1Opt: Option[ChannelUpdateExt], update2Opt: Option[ChannelUpdateExt], ann: ChannelAnnouncement) {
    def getChannelUpdateSameSideAs(cu: ChannelUpdate): Option[ChannelUpdateExt] = if (cu.position == ChannelUpdate.POSITION1NODE) update1Opt else update2Opt
  }

  trait Hop {
    def nodeId: PublicKey
    def nextNodeId: PublicKey
    def asString(humanRouted: String): String
    def fee(amount: MilliSatoshi): MilliSatoshi
    def cltvExpiryDelta: CltvExpiryDelta
  }

  /**
   * A directed hop between two connected nodes using a specific channel.
   */
  case class ChannelHop(edge: GraphEdge) extends Hop {
    override def asString(humanRouted: String): String = s"${nodeId.value.toHex} ($humanRouted @ ${edge.desc.shortChannelId.toString})"
    override def fee(amount: MilliSatoshi): MilliSatoshi = nodeFee(edge.updExt.update.feeBaseMsat, edge.updExt.update.feeProportionalMillionths, amount)
    override val cltvExpiryDelta: CltvExpiryDelta = edge.updExt.update.cltvExpiryDelta
    override val nextNodeId: PublicKey = edge.desc.to
    override val nodeId: PublicKey = edge.desc.from
  }

  /**
   * A directed hop between two trampoline nodes.
   * These nodes need not be connected and we don't need to know a route between them.
   * The start node will compute the route to the end node itself when it receives our payment.
   *
   * @param nodeId          id of the start node.
   * @param nextNodeId      id of the end node.
   * @param cltvExpiryDelta cltv expiry delta.
   * @param fee             total fee for that hop.
   */
  case class NodeHop(nodeId: PublicKey, nextNodeId: PublicKey, cltvExpiryDelta: CltvExpiryDelta, fee: MilliSatoshi) extends Hop {
    override def asString(humanRouted: String): String = s"${nodeId.value.toHex} ($humanRouted @ Trampoline)"
    override def fee(amount: MilliSatoshi): MilliSatoshi = fee
  }

  case class RouteParams(feeReserve: MilliSatoshi, routeMaxLength: Int, routeMaxCltv: CltvExpiryDelta)

  case class RouteRequest(fullTag: FullPaymentTag, partId: ByteVector, source: PublicKey,
                          target: PublicKey, amount: MilliSatoshi, localEdge: GraphEdge, routeParams: RouteParams,
                          ignoreNodes: Set[PublicKey] = Set.empty, ignoreChannels: Set[ChannelDesc] = Set.empty)

  type RoutedPerHop = (MilliSatoshi, Hop)

  type RoutedPerChannelHop = (MilliSatoshi, ChannelHop)

  case class Route(hops: Seq[Hop], weight: RichWeight) {
    lazy val fee: MilliSatoshi = weight.costs.head - weight.costs.last

    lazy val routedPerHop: Seq[RoutedPerHop] = weight.costs.tail.zip(hops.tail)

    lazy val routedPerChannelHop: Seq[RoutedPerChannelHop] = routedPerHop.collect { case (amt, chanHop: ChannelHop) => amt -> chanHop }

    def getEdgeForNode(nodeId: PublicKey): Option[GraphEdge] = routedPerChannelHop.collectFirst { case (_, chanHop: ChannelHop) if nodeId == chanHop.nodeId => chanHop.edge }

    def asString(denom: Denomination): String = routedPerHop.collect { case (amt, hop) => hop.asString(denom asString amt).trim }.mkString("me -> peer ", " -> ", s" -> payee, route fee: ${denom asString fee}")

    require(hops.nonEmpty, "Route cannot be empty")
  }

  sealed trait RouteResponse { def fullTag: FullPaymentTag }

  case class NoRouteAvailable(fullTag: FullPaymentTag, partId: ByteVector) extends RouteResponse

  case class RouteFound(route: Route, fullTag: FullPaymentTag, partId: ByteVector) extends RouteResponse

  case class Data(channels: Map[ShortChannelId, PublicChannel], hostedChannels: Map[ShortChannelId, PublicChannel], graph: DirectedGraph)

  def getDesc(cu: ChannelUpdate, ann: ChannelAnnouncement): ChannelDesc = {
    if (Announcements isNode1 cu.channelFlags) ChannelDesc(cu.shortChannelId, ann.nodeId1, ann.nodeId2)
    else ChannelDesc(cu.shortChannelId, ann.nodeId2, ann.nodeId1)
  }
}