package immortan

import immortan.SyncMaster._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.softwaremill.quicklens._
import QueryShortChannelIdsTlv.QueryFlagType._
import fr.acinq.eclair.router.{Announcements, Sync}
import immortan.crypto.{CanBeRepliedTo, StateMachine, Tools}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import fr.acinq.eclair.Features.ChannelRangeQueriesExtended
import fr.acinq.eclair.router.Router.Data
import fr.acinq.bitcoin.Crypto.PublicKey
import java.util.concurrent.Executors
import fr.acinq.eclair.ShortChannelId
import immortan.crypto.Noise.KeyPair
import scala.util.Random.shuffle
import scala.collection.mutable
import immortan.utils.Rx


object SyncMaster {
  final val WAITING = 0
  final val SHUT_DOWN = 1
  final val SHORT_ID_SYNC = 2
  final val GOSSIP_SYNC = 3
  final val PHC_SYNC = 4

  final val CMDAddSync = "cmd-add-sync"
  final val CMDGetGossip = "cmd-get-gossip"
  final val CMDShutdown = "cmd-shut-down"

  type ConfirmedBySet = Set[PublicKey]
  type ShortChanIdSet = Set[ShortChannelId]
  type PositionSet = Set[java.lang.Integer]
}

sealed trait SyncWorkerData

case class SyncWorkerShortIdsData(ranges: List[ReplyChannelRange] = Nil, from: Int) extends SyncWorkerData {
  // This class contains a list of shortId ranges collected from a single remote peer, we need to make sure all of them are sound, that is, TLV data is of same size as main data
  def isHolistic: Boolean = ranges.forall(rng => rng.shortChannelIds.array.size == rng.timestamps.timestamps.size && rng.timestamps.timestamps.size == rng.checksums.checksums.size)
  lazy val allShortIds: Seq[ShortChannelId] = ranges.flatMap(_.shortChannelIds.array)
}

case class SyncWorkerGossipData(syncMaster: SyncMaster,
                                queries: Seq[QueryShortChannelIds],
                                updates: Set[ChannelUpdate] = Set.empty,
                                announces: Set[ChannelAnnouncement] = Set.empty,
                                excluded: Set[UpdateCore] = Set.empty) extends SyncWorkerData

case class CMDShortIdsComplete(sync: SyncWorker, data: SyncWorkerShortIdsData)
case class CMDChunkComplete(sync: SyncWorker, data: SyncWorkerGossipData)
case class SyncDisconnected(sync: SyncWorker, removePeer: Boolean)
case class CMDGossipComplete(sync: SyncWorker)

// This entirely relies on fact that peer sends ChannelAnnouncement messages first, then ChannelUpdate messages

case class SyncWorkerPHCData(phcMaster: PHCSyncMaster,
                             updates: Set[ChannelUpdate],
                             nodeIdToShortIds: Map[PublicKey, ShortChanIdSet] = Map.empty,
                             expectedPositions: Map[ShortChannelId, PositionSet] = Map.empty,
                             announces: Map[ShortChannelId, ChannelAnnouncement] = Map.empty) extends SyncWorkerData {

  def withNewAnnounce(ann: ChannelAnnouncement): SyncWorkerPHCData = {
    val nodeId1ToShortIds = nodeIdToShortIds.getOrElse(ann.nodeId1, Set.empty) + ann.shortChannelId
    val nodeId2ToShortIds = nodeIdToShortIds.getOrElse(ann.nodeId2, Set.empty) + ann.shortChannelId
    val nodeIdToShortIds1 = nodeIdToShortIds.updated(ann.nodeId1, nodeId1ToShortIds).updated(ann.nodeId2, nodeId2ToShortIds)
    copy(expectedPositions = expectedPositions.updated(ann.shortChannelId, ChannelUpdate.fullSet), announces = announces.updated(ann.shortChannelId, ann), nodeIdToShortIds = nodeIdToShortIds1)
  }

  def withNewUpdate(cu: ChannelUpdate): SyncWorkerPHCData = {
    val oneLessPosition = expectedPositions.getOrElse(cu.shortChannelId, Set.empty) - cu.position
    copy(expectedPositions = expectedPositions.updated(cu.shortChannelId, oneLessPosition), updates = updates + cu)
  }

  def isAcceptable(ann: ChannelAnnouncement): Boolean = {
    val notTooMuchNode1PHCs = nodeIdToShortIds.getOrElse(ann.nodeId1, Set.empty).size < LNParams.syncParams.maxPHCPerNode
    val notTooMuchNode2PHCs = nodeIdToShortIds.getOrElse(ann.nodeId2, Set.empty).size < LNParams.syncParams.maxPHCPerNode
    val isCorrect = Tools.hostedShortChanId(ann.nodeId1.value, ann.nodeId2.value) == ann.shortChannelId
    ann.isPHC && isCorrect && notTooMuchNode1PHCs && notTooMuchNode2PHCs
  }

  def isUpdateAcceptable(cu: ChannelUpdate): Boolean =
    cu.htlcMaximumMsat.exists(cap => cap >= LNParams.syncParams.minPHCCapacity && cap <= LNParams.syncParams.maxPHCCapacity && cap > cu.htlcMinimumMsat) && // Capacity is fine
      announces.get(cu.shortChannelId).map(_ getNodeIdSameSideAs cu).exists(Announcements checkSig cu) && // We have received a related announce, signature is valid
      expectedPositions.getOrElse(cu.shortChannelId, Set.empty).contains(cu.position) // Remote node must not send the same update twice
}

case class SyncWorker(master: CanBeRepliedTo, keyPair: KeyPair, remoteInfo: RemoteNodeInfo, ourInit: Init) extends StateMachine[SyncWorkerData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)
  val pair: KeyPairAndPubKey = KeyPairAndPubKey(keyPair, remoteInfo.nodeId)

  val listener: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = me process worker
    override def onMessage(worker: CommsTower.Worker, msg: LightningMessage): Unit = me process msg

    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      val supportsExtQueries = worker.theirInit.forall { theirInit =>
        LNParams.isPeerSupports(theirInit)(ChannelRangeQueriesExtended)
      }

      // This disconnect is unexpected, normal shoutdown removes listener
      master process SyncDisconnected(me, removePeer = !supportsExtQueries)
      CommsTower.listeners(worker.pair) -= listener
    }
  }

  become(null, WAITING)
  // Note that our keyPair is always ranom here
  CommsTower.listen(Set(listener), pair, remoteInfo)

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (data1: SyncWorkerPHCData, null, WAITING) => become(data1, PHC_SYNC)
    case (data1: SyncWorkerShortIdsData, null, WAITING) => become(data1, SHORT_ID_SYNC)
    case (data1: SyncWorkerGossipData, _, WAITING | SHORT_ID_SYNC) => become(data1, GOSSIP_SYNC)

    case (worker: CommsTower.Worker, data: SyncWorkerShortIdsData, SHORT_ID_SYNC) =>
      val tlv = QueryChannelRangeTlv.QueryFlags(QueryChannelRangeTlv.QueryFlags.WANT_ALL)
      val query = QueryChannelRange(LNParams.chainHash, data.from, Int.MaxValue, TlvStream apply tlv)
      worker.handler process query

    case (reply: ReplyChannelRange, data1: SyncWorkerShortIdsData, SHORT_ID_SYNC) =>
      val updatedData: SyncWorkerShortIdsData = data1.copy(ranges = reply +: data1.ranges)
      if (reply.syncComplete == 1) master process CMDShortIdsComplete(me, updatedData)
      else become(updatedData, SHORT_ID_SYNC)

    // GOSSIP_SYNC

    case (_: CommsTower.Worker, _: SyncWorkerGossipData, GOSSIP_SYNC) =>
      // Remote peer is connected, (re-)start remaining gossip sync
      me process CMDGetGossip

    case (CMDGetGossip, data1: SyncWorkerGossipData, GOSSIP_SYNC) if data1.queries.isEmpty =>
      // We have no more queries left, inform master
      master process CMDGossipComplete(me)
      me process CMDShutdown

    case (CMDGetGossip, data1: SyncWorkerGossipData, GOSSIP_SYNC) =>
      // We still have queries left, send another one to peer
      CommsTower.sendMany(data1.queries.take(1), pair)

    case (update: ChannelUpdate, d1: SyncWorkerGossipData, GOSSIP_SYNC) if d1.syncMaster.provenButShouldBeExcluded(update) => become(d1.copy(excluded = d1.excluded + update.core), GOSSIP_SYNC)
    case (update: ChannelUpdate, d1: SyncWorkerGossipData, GOSSIP_SYNC) if d1.syncMaster.provenAndNotExcluded(update.shortChannelId) => become(d1.copy(updates = d1.updates + update.lite), GOSSIP_SYNC)
    case (ann: ChannelAnnouncement, d1: SyncWorkerGossipData, GOSSIP_SYNC) if d1.syncMaster.provenShortIds.contains(ann.shortChannelId) => become(d1.copy(announces = d1.announces + ann.lite), GOSSIP_SYNC)

    case (_: ReplyShortChannelIdsEnd, data1: SyncWorkerGossipData, GOSSIP_SYNC) =>
      // We have completed current chunk, inform master and either continue or complete
      become(SyncWorkerGossipData(data1.syncMaster, data1.queries.tail), GOSSIP_SYNC)
      master process CMDChunkComplete(me, data1)
      me process CMDGetGossip

    // PHC_SYNC

    case (worker: CommsTower.Worker, _: SyncWorkerPHCData, PHC_SYNC) => worker.handler process QueryPublicHostedChannels(LNParams.chainHash)
    case (update: ChannelUpdate, d1: SyncWorkerPHCData, PHC_SYNC) if d1.isUpdateAcceptable(update) => become(d1.withNewUpdate(update.lite), PHC_SYNC)
    case (ann: ChannelAnnouncement, d1: SyncWorkerPHCData, PHC_SYNC) if d1.isAcceptable(ann) && d1.phcMaster.isAcceptable(ann) => become(d1.withNewAnnounce(ann.lite), PHC_SYNC)

    case (_: ReplyPublicHostedChannelsEnd, completeSyncData: SyncWorkerPHCData, PHC_SYNC) =>
      // Peer has informed us that there is no more PHC gossip left, inform master and shut down
      master process completeSyncData
      me process CMDShutdown

    case (CMDShutdown, _, _) =>
      become(freshData = null, SHUT_DOWN)
      CommsTower forget pair

    case _ =>
  }
}

sealed trait SyncMasterData extends { me =>
  def getNewSync(master: CanBeRepliedTo): SyncWorker = {
    // This relies on (1) baseSyncs items are never removed
    // AND (2) size of baseSyncs is >= LNParams.maxNodesToSyncFrom
    val unusedSyncs = activeSyncs.foldLeft(baseInfos ++ extInfos)(_ - _.remoteInfo)
    SyncWorker(master, randomKeyPair, shuffle(unusedSyncs.toList).head, LNParams.ourInit)
  }

  def withoutSync(sd: SyncDisconnected): SyncMasterData = me
    .modify(_.extInfos).usingIf(sd.removePeer)(_ - sd.sync.remoteInfo)
    .modify(_.activeSyncs).using(_ - sd.sync)

  def baseInfos: Set[RemoteNodeInfo]
  def extInfos: Set[RemoteNodeInfo]
  def activeSyncs: Set[SyncWorker]
}

case class PureRoutingData(announces: Set[ChannelAnnouncement], updates: Set[ChannelUpdate], excluded: Set[UpdateCore] = Set.empty)
case class SyncMasterShortIdData(baseInfos: Set[RemoteNodeInfo], extInfos: Set[RemoteNodeInfo], activeSyncs: Set[SyncWorker], ranges: Map[PublicKey, SyncWorkerShortIdsData] = Map.empty) extends SyncMasterData
case class SyncMasterGossipData(baseInfos: Set[RemoteNodeInfo], extInfos: Set[RemoteNodeInfo], activeSyncs: Set[SyncWorker], chunksLeft: Int) extends SyncMasterData

case class UpdateConifrmState(liteUpdOpt: Option[ChannelUpdate], confirmedBy: ConfirmedBySet) {
  def add(cu: ChannelUpdate, from: PublicKey): UpdateConifrmState = copy(liteUpdOpt = Some(cu), confirmedBy = confirmedBy + from)
}

abstract class SyncMaster(excluded: Set[Long], routerData: Data) extends StateMachine[SyncMasterData] with CanBeRepliedTo { me =>
  val confirmedChanUpdates: mutable.Map[UpdateCore, UpdateConifrmState] = mutable.Map.empty withDefaultValue UpdateConifrmState(None, Set.empty)
  val confirmedChanAnnounces: mutable.Map[ChannelAnnouncement, ConfirmedBySet] = mutable.Map.empty withDefaultValue Set.empty
  var newExcludedChanUpdates: Set[UpdateCore] = Set.empty
  var provenShortIds: ShortChanIdSet = Set.empty

  def onChunkSyncComplete(pure: PureRoutingData): Unit
  def onTotalSyncComplete: Unit

  def hasCapacityIssues(update: ChannelUpdate): Boolean = update.htlcMaximumMsat.forall(cap => cap < LNParams.syncParams.minCapacity || cap <= update.htlcMinimumMsat)
  def provenButShouldBeExcluded(update: ChannelUpdate): Boolean = provenShortIds.contains(update.shortChannelId) && hasCapacityIssues(update)
  def provenAndNotExcluded(shortId: ShortChannelId): Boolean = provenShortIds.contains(shortId) && !excluded.contains(shortId.id)

  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)
  become(null, SHORT_ID_SYNC)

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (setupData: SyncMasterShortIdData, null, SHORT_ID_SYNC) if setupData.baseInfos.nonEmpty =>
      val rng = 0 until LNParams.syncParams.maxNodesToSyncFrom
      become(freshData = setupData, SHORT_ID_SYNC)
      rng.foreach(_ => me process CMDAddSync)

    case (CMDAddSync, data1: SyncMasterShortIdData, SHORT_ID_SYNC) if data1.activeSyncs.size < LNParams.syncParams.maxNodesToSyncFrom =>
      // We are asked to create a new worker AND we don't have enough workers yet: create a new one and instruct it to sync right away

      val newSyncWorker = data.getNewSync(me)
      become(data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker), SHORT_ID_SYNC)
      newSyncWorker process SyncWorkerShortIdsData(ranges = Nil, from = 0)

    case (sd: SyncDisconnected, data1: SyncMasterShortIdData, SHORT_ID_SYNC) =>
      become(data1.copy(ranges = data1.ranges - sd.sync.pair.them).withoutSync(sd), SHORT_ID_SYNC)
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process CMDAddSync)

    case (CMDShortIdsComplete(sync, ranges1), SyncMasterShortIdData(baseSyncs, extSyncs, activeSyncs, ranges), SHORT_ID_SYNC) =>
      val data1 = SyncMasterShortIdData(baseSyncs, extSyncs, ranges = ranges + (sync.pair.them -> ranges1), activeSyncs = activeSyncs)
      val isEnoughEvidence = data1.ranges.size == LNParams.syncParams.maxNodesToSyncFrom
      become(data1, SHORT_ID_SYNC)

      if (isEnoughEvidence) {
        // Collected enough channel ranges to start gossip
        val goodRanges = data1.ranges.values.filter(_.isHolistic)
        val accum = mutable.Map.empty[ShortChannelId, Int] withDefaultValue 0
        goodRanges.flatMap(_.allShortIds).foreach(shortId => accum(shortId) += 1)
        provenShortIds = accum.collect { case (shortId, confs) if confs > LNParams.syncParams.acceptThreshold => shortId }.toSet
        val queries: Seq[QueryShortChannelIds] = goodRanges.maxBy(_.allShortIds.size).ranges.par.flatMap(reply2Query).toList
        // println(s"Re-querying ${queries.flatMap(_.shortChannelIds.array).size} channels")

        // Transfer every worker into gossip syncing state
        become(SyncMasterGossipData(baseSyncs, extSyncs, activeSyncs, LNParams.syncParams.chunksToWait), GOSSIP_SYNC)
        for (currentSync <- activeSyncs) currentSync process SyncWorkerGossipData(me, queries)
        for (currentSync <- activeSyncs) currentSync process CMDGetGossip
      }

    // GOSSIP_SYNC

    case (workerData: SyncWorkerGossipData, data1: SyncMasterGossipData, GOSSIP_SYNC) if data1.activeSyncs.size < LNParams.syncParams.maxNodesToSyncFrom =>
      // Turns out one of the workers has disconnected while getting gossip, create one with unused remote nodeId and track its progress
      // Important: we retain pending queries from previous sync worker, that's why we need worker data here

      val newSyncWorker = data1.getNewSync(me)
      become(data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker), GOSSIP_SYNC)
      newSyncWorker process SyncWorkerGossipData(me, workerData.queries)

    case (sd: SyncDisconnected, data1: SyncMasterGossipData, GOSSIP_SYNC) =>
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process sd.sync.data)
      become(data1.withoutSync(sd), GOSSIP_SYNC)

    case (CMDChunkComplete(sync, workerData), data1: SyncMasterGossipData, GOSSIP_SYNC) =>
      for (liteAnnounce <- workerData.announces) confirmedChanAnnounces(liteAnnounce) = confirmedChanAnnounces(liteAnnounce) + sync.pair.them
      for (liteUpdate <- workerData.updates) confirmedChanUpdates(liteUpdate.core) = confirmedChanUpdates(liteUpdate.core).add(liteUpdate, sync.pair.them)
      newExcludedChanUpdates ++= workerData.excluded

      if (data1.chunksLeft > 0) {
        // We batch multiple chunks to have less upstream db calls
        val nextData = data1.copy(chunksLeft = data1.chunksLeft - 1)
        become(nextData, GOSSIP_SYNC)
      } else {
        // Current batch is ready, send it out and start a new one right away
        become(data1.copy(chunksLeft = LNParams.syncParams.chunksToWait), GOSSIP_SYNC)
        sendPureNormalNetworkData
      }

    case (CMDGossipComplete(sync), data1: SyncMasterGossipData, GOSSIP_SYNC) =>
      val nextData = data1.copy(activeSyncs = data1.activeSyncs - sync)

      if (nextData.activeSyncs.nonEmpty) {
        become(nextData, GOSSIP_SYNC)
      } else {
        become(null, SHUT_DOWN)
        sendPureNormalNetworkData
        onTotalSyncComplete
      }

    case _ =>
  }

  def sendPureNormalNetworkData: Unit = {
    val goodAnnounces = confirmedChanAnnounces.collect { case (announce, confirmedByNodes) if confirmedByNodes.size > LNParams.syncParams.acceptThreshold => announce }.toSet
    val goodUpdates = confirmedChanUpdates.values.collect { case UpdateConifrmState(Some(update), confs) if confs.size > LNParams.syncParams.acceptThreshold => update }.toSet
    me onChunkSyncComplete PureRoutingData(goodAnnounces, goodUpdates, newExcludedChanUpdates)
    for (announce <- goodAnnounces) confirmedChanAnnounces -= announce
    for (update <- goodUpdates) confirmedChanUpdates -= update.core
    newExcludedChanUpdates = Set.empty
  }

  def reply2Query(reply: ReplyChannelRange): Iterator[QueryShortChannelIds] = {
    val stack = (reply.shortChannelIds.array, reply.timestamps.timestamps, reply.checksums.checksums)

    val shortIdFlagSeq = for {
      (shortId, theirTimestamps, theirChecksums) <- stack.zipped if provenAndNotExcluded(shortId)
      finalFlag = computeFlag(shortId, theirTimestamps, theirChecksums) if finalFlag != 0
    } yield (shortId, finalFlag)

    val groupedShortIdFlagSeqs = shortIdFlagSeq.toList.grouped(LNParams.syncParams.messagesToAsk)

    for {
      requestChunk <- groupedShortIdFlagSeqs
      (chunkShortIds, chunkRequestFlags) = requestChunk.unzip
      shortChannelIds = EncodedShortChannelIds(reply.shortChannelIds.encoding, chunkShortIds)
      tlv = QueryShortChannelIdsTlv.EncodedQueryFlags(reply.shortChannelIds.encoding, chunkRequestFlags)
    } yield QueryShortChannelIds(LNParams.chainHash, shortChannelIds, TlvStream apply tlv)
  }

  private def computeFlag(shortlId: ShortChannelId, theirTimestamps: ReplyChannelRangeTlv.Timestamps, theirChecksums: ReplyChannelRangeTlv.Checksums) =
    if (routerData.channels contains shortlId) {
      val (stamps, checksums) = Sync.getChannelDigestInfo(routerData.channels)(shortlId)
      val shouldRequestUpdate1 = Sync.shouldRequestUpdate(stamps.timestamp1, checksums.checksum1, theirTimestamps.timestamp1, theirChecksums.checksum1)
      val shouldRequestUpdate2 = Sync.shouldRequestUpdate(stamps.timestamp2, checksums.checksum2, theirTimestamps.timestamp2, theirChecksums.checksum2)

      val flagUpdate1 = if (shouldRequestUpdate1) INCLUDE_CHANNEL_UPDATE_1 else 0
      val flagUpdate2 = if (shouldRequestUpdate2) INCLUDE_CHANNEL_UPDATE_2 else 0
      0 | flagUpdate1 | flagUpdate2
    } else {
      INCLUDE_CHANNEL_ANNOUNCEMENT | INCLUDE_CHANNEL_UPDATE_1 | INCLUDE_CHANNEL_UPDATE_2
    }
}

case class CompleteHostedRoutingData(announces: Set[ChannelAnnouncement], updates: Set[ChannelUpdate] = Set.empty)
case class SyncMasterPHCData(baseInfos: Set[RemoteNodeInfo], extInfos: Set[RemoteNodeInfo], activeSyncs: Set[SyncWorker], attemptsLeft: Int = 12) extends SyncMasterData

abstract class PHCSyncMaster(routerData: Data) extends StateMachine[SyncMasterData] with CanBeRepliedTo { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)
  become(null, PHC_SYNC)

  // These checks require graph
  def isAcceptable(ann: ChannelAnnouncement): Boolean = {
    val node1HasEnoughIncomingChans = routerData.graph.vertices.getOrElse(ann.nodeId1, Nil).size >= LNParams.syncParams.minNormalChansForPHC
    val node2HasEnoughIncomingChans = routerData.graph.vertices.getOrElse(ann.nodeId2, Nil).size >= LNParams.syncParams.minNormalChansForPHC
    node1HasEnoughIncomingChans && node2HasEnoughIncomingChans
  }

  def onSyncComplete(pure: CompleteHostedRoutingData): Unit

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (setupData: SyncMasterPHCData, null, PHC_SYNC) if setupData.baseInfos.nonEmpty =>
      become(freshData = setupData, PHC_SYNC)
      me process CMDAddSync

    case (CMDAddSync, data1: SyncMasterPHCData, PHC_SYNC) if data1.activeSyncs.isEmpty =>
      // We are asked to create a new worker AND we don't have a worker yet: create one

      val newSyncWorker = data1.getNewSync(me)
      become(data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker), PHC_SYNC)
      newSyncWorker process SyncWorkerPHCData(me, updates = Set.empty)

    case (sd: SyncDisconnected, data1: SyncMasterPHCData, PHC_SYNC) if data1.attemptsLeft > 0 =>
      become(data1.copy(attemptsLeft = data1.attemptsLeft - 1).withoutSync(sd), PHC_SYNC)
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process CMDAddSync)

    case (_: SyncWorker, _, PHC_SYNC) =>
      // No more reconnect attempts left
      become(null, SHUT_DOWN)

    case (d1: SyncWorkerPHCData, _, PHC_SYNC) =>
      // Worker has informed us that PHC sync is complete, shut everything down
      val pure = CompleteHostedRoutingData(d1.announces.values.toSet, d1.updates)
      become(null, SHUT_DOWN)
      onSyncComplete(pure)

    case _ =>
  }
}