package immortan.fsm

import fr.acinq.eclair.wire._
import scala.concurrent.duration._
import immortan.fsm.AccountExistenceCheck._
import immortan.{CommsTower, ConnectionListener, RemoteNodeInfo, StorageFormat}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import java.util.concurrent.Executors
import fr.acinq.bitcoin.ByteVector32
import immortan.crypto.StateMachine
import immortan.utils.Rx


object AccountExistenceCheck {
  final val OPERATIONAL = "existance-state-operational"
  final val FINALIZED = "existance-state-finalized"
  final val CMDCancel = "existance-cmd-cancel"

  case class PeerDisconnected(worker: CommsTower.Worker)
  case class PeerResponse(msg: HostedChannelMessage, worker: CommsTower.Worker)
  case class CheckData(hosts: Set[RemoteNodeInfo], results: Map[RemoteNodeInfo, Boolean], reconnectsLeft: Int)
  case class CMDStart(remoteInfos: Set[RemoteNodeInfo] = Set.empty)
}

abstract class AccountExistenceCheck(format: StorageFormat, chainHash: ByteVector32) extends StateMachine[CheckData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)

  lazy private val accountCheckListener = new ConnectionListener {
    override def onDisconnect(worker: CommsTower.Worker): Unit = me process PeerDisconnected(worker)
    override def onHostedMessage(worker: CommsTower.Worker, msg: HostedChannelMessage): Unit = me process PeerResponse(msg, worker)

    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val peerSpecificSecret = format.attachedChannelSecret(theirNodeId = worker.info.nodeId)
      val peerSpecificRefundPubKey = format.keys.refundPubKey(theirNodeId = worker.info.nodeId)
      val invokeMsg = InvokeHostedChannel(chainHash, peerSpecificRefundPubKey, peerSpecificSecret)
      worker.handler process invokeMsg
    }
  }

  def onTimeout: Unit
  def onNoAccountFound: Unit
  def onPresentAccount: Unit

  def doProcess(change: Any): Unit = (change, state) match {
    case (PeerDisconnected(worker), OPERATIONAL) if data.reconnectsLeft > 0 =>
      become(data.copy(reconnectsLeft = data.reconnectsLeft - 1), OPERATIONAL)
      Rx.ioQueue.delay(3.seconds).foreach(_ => me process worker)

    case (_: PeerDisconnected, OPERATIONAL) =>
      // We've run out of reconnect attempts
      doSearch(force = true)

    case (worker: CommsTower.Worker, OPERATIONAL) =>
      // We get previously scheduled worker, use its peer data to reconnect again
      CommsTower.listenNative(Set(accountCheckListener), worker.info)

    case (PeerResponse(_: InitHostedChannel, worker), OPERATIONAL) =>
      // Remote node offers to create a new channel, no "account" there
      become(data.copy(results = data.results - worker.info), OPERATIONAL)
      doSearch(force = false)

    case (PeerResponse(remoteLCSS: LastCrossSignedState, worker), OPERATIONAL) =>
      val isLocalSigOk = remoteLCSS.verifyRemoteSig(worker.info.nodeSpecificPubKey)
      // Remote node replies with some channel state, check our signature to make sure state is valid
      val results1 = if (isLocalSigOk) data.results.updated(worker.info, true) else data.results - worker.info
      become(data.copy(results = results1), OPERATIONAL)
      doSearch(force = false)

    case (CMDCancel, OPERATIONAL) =>
      // User has manually cancelled a check, disconnect all peers
      for (info <- data.hosts) CommsTower forget info.nodeSpecificPair
      become(data, FINALIZED)

    case (CMDStart(remoteInfos), null) =>
      become(CheckData(remoteInfos, remoteInfos.map(_ -> false).toMap, remoteInfos.size * 4), OPERATIONAL)
      for (info <- remoteInfos) CommsTower.listenNative(Set(accountCheckListener), info)
      Rx.ioQueue.delay(30.seconds).foreach(_ => me doSearch true)

    case _ =>
  }

  private def doSearch(force: Boolean): Unit =
    if (data.results.values.toSet contains true) {
      // At least one remote peer has confirmed a channel
      // we do not wait for the rest and proceed right away
      me doProcess CMDCancel
      onPresentAccount
    } else if (data.results.isEmpty) {
      // All peers replied, none has a channel
      me doProcess CMDCancel
      onNoAccountFound
    } else if (force) {
      me doProcess CMDCancel
      onTimeout
    }
}