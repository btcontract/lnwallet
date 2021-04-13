package immortan

import immortan.PathFinder._
import immortan.crypto.Tools._
import scala.collection.JavaConverters._

import immortan.utils.{Rx, Statistics}
import java.util.concurrent.{Executors, TimeUnit}
import immortan.crypto.{CanBeRepliedTo, StateMachine}
import fr.acinq.eclair.{CltvExpiryDelta, MilliSatoshi}
import fr.acinq.eclair.wire.{ChannelUpdate, ShortIdAndPosition}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import fr.acinq.eclair.router.{Announcements, ChannelUpdateExt, Router}
import fr.acinq.eclair.router.Router.{Data, PublicChannel, RouteRequest}
import fr.acinq.eclair.router.Graph.GraphStructure.{DirectedGraph, GraphEdge}
import fr.acinq.eclair.router.RouteCalculation.handleRouteRequest
import com.google.common.cache.CacheBuilder
import scala.collection.mutable


object PathFinder {
  val WAITING = "path-finder-state-waiting"
  val OPERATIONAL = "path-finder-state-operational"
  val NotifyRejected = "path-finder-notify-rejected"
  val NotifyOperational = "path-finder-notify-operational"
  val CMDLoadGraph = "cmd-load-graph"
  val CMDResync = "cmd-resync"

  val RESYNC_PERIOD: Long = 1000L * 3600 * 24 * 2 // days in msecs
}

case class AvgHopParams(cltvExpiryDelta: CltvExpiryDelta, feeProportionalMillionths: MilliSatoshi, feeBaseMsat: MilliSatoshi, sampleSize: Long)

abstract class PathFinder(val normalBag: NetworkBag, val hostedBag: NetworkBag) extends StateMachine[Data] { me =>
  private val extraEdges = CacheBuilder.newBuilder.expireAfterWrite(1, TimeUnit.DAYS).maximumSize(5000).build[ShortIdAndPosition, GraphEdge]
  val extraEdgesMap: mutable.Map[ShortIdAndPosition, GraphEdge] = extraEdges.asMap.asScala
  var listeners: Set[CanBeRepliedTo] = Set.empty
  var debugMode: Boolean = false

  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)

  // We don't load routing data on every startup but when user (or system) actually needs it
  become(Data(channels = Map.empty, hostedChannels = Map.empty, graph = DirectedGraph.apply), WAITING)
  // Init resync with persistent delay on startup, then periodically resync every RESYNC_PERIOD days + 1 hour to trigger a full, not just PHC sync
  Rx.initDelay(Rx.repeat(Rx.ioQueue, Rx.incHour, 49 to Int.MaxValue by 49), getLastTotalResyncStamp, RESYNC_PERIOD).foreach(_ => me process CMDResync)

  def getLastTotalResyncStamp: Long
  def getLastNormalResyncStamp: Long

  def updateLastTotalResyncStamp(stamp: Long): Unit
  def updateLastNormalResyncStamp(stamp: Long): Unit

  def getPHCExtraNodes: Set[RemoteNodeInfo]
  def getExtraNodes: Set[RemoteNodeInfo]

  def doProcess(change: Any): Unit = (change, state) match {
    case (Tuple2(sender: CanBeRepliedTo, request: RouteRequest), OPERATIONAL) =>

      if (data.channels.isEmpty) {
        // Graph is loaded but it is empty
        // likey a first launch or synchronizing
        sender process NotifyRejected
      } else {
        // In OPERATIONAL state we instruct graph to search through the single pre-selected local channel
        // it is safe to not check for existance becase base graph never has our private outgoing edges
        val graph1 = data.graph.addEdge(edge = request.localEdge, checkIfContains = false)
        sender process handleRouteRequest(graph1, request)
      }

    case (Tuple2(sender: CanBeRepliedTo, _: RouteRequest), WAITING) =>

      if (debugMode) {
        // Do not proceed, just inform the sender
        sender process NotifyRejected
      } else {
        // We need a loaded routing data to search for path properly
        // load that data while notifying sender if it's absent
        sender process NotifyRejected
        me process CMDLoadGraph
      }

    case (CMDResync, WAITING) =>
      // We need a loaded routing data to sync properly
      // load that data before proceeding if it's absent
      me process CMDLoadGraph
      me process CMDResync

    case (CMDLoadGraph, WAITING) =>
      val normalShortIdToPubChan = normalBag.getRoutingData
      val hostedShortIdToPubChan = hostedBag.getRoutingData
      val searchGraph1 = DirectedGraph.makeGraph(normalShortIdToPubChan ++ hostedShortIdToPubChan).addEdges(extraEdgesMap.values)
      become(Data(normalShortIdToPubChan, hostedShortIdToPubChan, searchGraph1), OPERATIONAL)
      listeners.foreach(_ process NotifyOperational)

    case (CMDResync, OPERATIONAL) if System.currentTimeMillis - getLastNormalResyncStamp > RESYNC_PERIOD =>
      // Last normal sync has happened too long ago, start with normal sync, then proceed with PHC sync
      val setupData = SyncMasterShortIdData(LNParams.syncParams.syncNodes, getExtraNodes, Set.empty)

      new SyncMaster(normalBag.listExcludedChannels, data) { self =>
        def onChunkSyncComplete(pure: PureRoutingData): Unit = me process pure
        def onTotalSyncComplete: Unit = me process self
      } process setupData

    case (CMDResync, OPERATIONAL) =>
      // Normal resync has happened recently, but PHC resync is outdated (PHC failed last time due to running out of attempts)
      // in this case we skip normal sync and start directly with PHC sync to save time and increase PHC sync success chances
      startPHCSync

    case (pure: CompleteHostedRoutingData, OPERATIONAL) =>
      // First, completely replace PHC data with obtained one
      hostedBag.processCompleteHostedData(pure)

      // Then reconstruct graph with new PHC data
      val hostedShortIdToPubChan = hostedBag.getRoutingData
      val searchGraph1 = DirectedGraph.makeGraph(data.channels ++ hostedShortIdToPubChan)
      val searchGraph2 = searchGraph1.addEdges(extraEdgesMap.values)

      // Sync is considered fully done, there will be no new attempts for a while
      become(Data(data.channels, hostedShortIdToPubChan, searchGraph2), OPERATIONAL)
      updateLastTotalResyncStamp(System.currentTimeMillis)

    case (pure: PureRoutingData, OPERATIONAL) =>
      // Run in this thread to not overload SyncMaster
      normalBag.processPureData(pure)

    case (sync: SyncMaster, OPERATIONAL) =>
      // Get rid of channels which peers know nothing about
      val normalShortIdToPubChan = normalBag.getRoutingData
      val oneSideShortIds = normalBag.listChannelsWithOneUpdate
      val ghostIds = normalShortIdToPubChan.keySet.diff(sync.provenShortIds)
      val normalShortIdToPubChan1 = normalShortIdToPubChan -- ghostIds -- oneSideShortIds
      val searchGraph1 = DirectedGraph.makeGraph(normalShortIdToPubChan1 ++ data.hostedChannels)
      val searchGraph2 = searchGraph1.addEdges(extraEdgesMap.values)

      become(Data(normalShortIdToPubChan1, data.hostedChannels, searchGraph2), OPERATIONAL)
      // Perform database cleaning in a different thread since it's slow and we are operational now
      Rx.ioQueue.foreach(_ => normalBag.removeGhostChannels(ghostIds, oneSideShortIds), none)
      // Update normal checkpoint, if PHC sync fails this time we'll jump to it next time
      updateLastNormalResyncStamp(System.currentTimeMillis)
      listeners.foreach(_ process NotifyOperational)
      // Notify ASAP, then start PHC sync
      startPHCSync

    // We always accept and store disabled channels:
    // - to reduce subsequent sync traffic if channel remains disabled
    // - to account for the case when channel becomes enabled but we don't know
    // If we hit an updated channel while routing we save it to db and update in-memory graph
    // If disabled channel stays disabled for a long time it will be pruned by peers and then by us

    case (cu: ChannelUpdate, OPERATIONAL) if data.channels.contains(cu.shortChannelId) =>
      val data1 = resolve(data.channels(cu.shortChannelId), cu, normalBag)
      become(data1, OPERATIONAL)

    case (cu: ChannelUpdate, OPERATIONAL) if data.hostedChannels.contains(cu.shortChannelId) =>
      val data1 = resolve(data.hostedChannels(cu.shortChannelId), cu, hostedBag)
      become(data1, OPERATIONAL)

    case (cu: ChannelUpdate, OPERATIONAL) =>
      // Last chance: if it's not a known public update then maybe it's a private one
      Option(extraEdges getIfPresent cu.toShortIdAndPosition).foreach { extraEdge =>
        val edge1 = extraEdge.copy(updExt = extraEdge.updExt withNewUpdate cu)
        val data1 = resolveKnownDesc(edge1, storeOpt = None, isOld = false)
        become(data1, OPERATIONAL)
      }

    case (edge: GraphEdge, WAITING | OPERATIONAL) if !data.channels.contains(edge.desc.shortChannelId) =>
      // We add assisted routes to graph as if they are normal channels, also rememeber them to refill later if graph gets reloaded
      // these edges will be private most of the time, but they may be public and we may have them already so checkIfContains == true
      extraEdges.put(edge.updExt.update.toShortIdAndPosition, edge)
      val data1 = data.copy(graph = data.graph addEdge edge)
      become(data1, state)

    case _ =>
  }

  // Common resover for normal/hosted public channel updates
  def resolve(pubChan: PublicChannel, newUpdate: ChannelUpdate, store: NetworkBag): Data = {
    val currentUpdateExtOpt: Option[ChannelUpdateExt] = pubChan.getChannelUpdateSameSideAs(newUpdate)
    val newUpdateIsOlder: Boolean = currentUpdateExtOpt.exists(_.update.timestamp >= newUpdate.timestamp)
    val newUpdateExt = currentUpdateExtOpt.map(_ withNewUpdate newUpdate).getOrElse(ChannelUpdateExt fromUpdate newUpdate)
    resolveKnownDesc(GraphEdge(Router.getDesc(newUpdate, pubChan.ann), newUpdateExt), storeOpt = Some(store), isOld = newUpdateIsOlder)
  }

  // Resolves channel updates which we obtain from node errors while trying to route payments
  // store is optional to make sure private normal/hosted channel updates never make it to our database
  def resolveKnownDesc(edge: GraphEdge, storeOpt: Option[NetworkBag], isOld: Boolean): Data = {
    val isEnabled = Announcements.isEnabled(edge.updExt.update.channelFlags)

    storeOpt match {
      case Some(store) if edge.updExt.update.htlcMaximumMsat.isEmpty =>
        // Will be queried on next sync and will most likely be excluded
        store.removeChannelUpdate(edge.updExt.update.shortChannelId)
        data.copy(graph = data.graph removeEdge edge.desc)

      case _ if isOld =>
        // We have a newer one or this one is stale
        // retain db record since we have a more recent copy
        data.copy(graph = data.graph removeEdge edge.desc)

      case Some(store) if isEnabled =>
        // This is a legitimate public update, refresh everywhere
        store.addChannelUpdateByPosition(edge.updExt.update)
        data.copy(graph = data.graph addEdge edge)

      case Some(store) =>
        // Save in db because update is fresh
        store.addChannelUpdateByPosition(edge.updExt.update)
        // But remove from runtime graph because it's disabled
        data.copy(graph = data.graph removeEdge edge.desc)

      case None if isEnabled =>
        // This is a legitimate private/unknown-public update
        extraEdges.put(edge.updExt.update.toShortIdAndPosition, edge)
        // Don't save in DB but update runtime graph
        data.copy(graph = data.graph addEdge edge)

      case None =>
        // Disabled private/unknown-public update, remove from graph
        data.copy(graph = data.graph removeEdge edge.desc)
    }
  }

  def startPHCSync: Unit = {
    val phcSync = new PHCSyncMaster(data) { def onSyncComplete(pure: CompleteHostedRoutingData): Unit = me process pure }
    phcSync process SyncMasterPHCData(LNParams.syncParams.phcSyncNodes, getPHCExtraNodes, Set.empty)
  }

  def getAvgHopParams: AvgHopParams = {
    val sample = data.channels.values.toVector.flatMap(pc => pc.update1Opt ++ pc.update2Opt)
    val noFeeOutliers = Statistics.removeExtremeOutliers(sample)(_.update.feeProportionalMillionths)
    val cltvExpiryDelta = CltvExpiryDelta(Statistics.meanBy(noFeeOutliers)(_.update.cltvExpiryDelta).toInt)
    val proportional = MilliSatoshi(Statistics.meanBy(noFeeOutliers)(_.update.feeProportionalMillionths).toLong)
    val base = MilliSatoshi(Statistics.meanBy(noFeeOutliers)(_.update.feeBaseMsat).toLong)
    AvgHopParams(cltvExpiryDelta, proportional, base, noFeeOutliers.size)
  }
}
