package immortan.fsm

import scala.concurrent.duration._
import fr.acinq.eclair.wire.{HostedChannelMessage, Init, QueryPreimages, ReplyPreimages}
import immortan.{CommsTower, ConnectionListener, KeyPairAndPubKey, RemoteNodeInfo}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import immortan.fsm.PreimageCheck.{FINALIZED, OPERATIONAL}
import fr.acinq.bitcoin.{ByteVector32, Crypto}
import immortan.crypto.Tools.randomKeyPair
import java.util.concurrent.Executors
import immortan.crypto.StateMachine
import immortan.utils.Rx


object PreimageCheck {
  final val OPERATIONAL = 0
  final val FINALIZED = 1

  final val CMDCancel = "preimage-check-cmd-cancel"
  case class PeerDisconnected(worker: CommsTower.Worker)
  case class PeerResponse(msg: HostedChannelMessage, worker: CommsTower.Worker)
  case class CMDStart(hosts: Set[RemoteNodeInfo], hashes: Set[ByteVector32] = Set.empty)

  case class CheckData(pairs: Map[RemoteNodeInfo, KeyPairAndPubKey], pending: Set[RemoteNodeInfo],
                       hashes: Set[ByteVector32], hashToPreimage: Map[ByteVector32, ByteVector32] = Map.empty)
}

abstract class PreimageCheck extends StateMachine[PreimageCheck.CheckData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def randomPair(info: RemoteNodeInfo): (RemoteNodeInfo, KeyPairAndPubKey) = info -> KeyPairAndPubKey(randomKeyPair, info.nodeId)
  def process(changeMessage: Any): Unit = scala.concurrent.Future(me doProcess changeMessage)

  def onTimeout(preimages: Map[ByteVector32, ByteVector32] = Map.empty): Unit
  def onComplete(preimages: Map[ByteVector32, ByteVector32] = Map.empty): Unit

  private lazy val listener = new ConnectionListener {
    override def onDisconnect(worker: CommsTower.Worker): Unit = me process PreimageCheck.PeerDisconnected(worker)
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = worker.handler process QueryPreimages(data.hashes.toList)
    override def onHostedMessage(worker: CommsTower.Worker, msg: HostedChannelMessage): Unit = me process PreimageCheck.PeerResponse(msg, worker)
  }

  def doProcess(change: Any): Unit = (change, state) match {
    case (msg: PreimageCheck.PeerDisconnected, OPERATIONAL) =>
      // Keep trying to reconnect with delays until final timeout
      Rx.ioQueue.delay(3.seconds).foreach(_ => me process msg.worker)
      CommsTower forget msg.worker.pair

    case (worker: CommsTower.Worker, OPERATIONAL) =>
      val newPair @ (info, pair) = randomPair(worker.info)
      CommsTower.listen(listeners1 = Set(listener), pair, info)
      become(data.copy(pairs = data.pairs + newPair), OPERATIONAL)

    case (PreimageCheck.PeerResponse(msg: ReplyPreimages, worker), OPERATIONAL) =>
      // One of remote nodes replies, check if we have all preimages of interest collected
      val hashToPreimage1 = data.hashToPreimage ++ msg.preimages.map(Crypto sha256 _).zip(msg.preimages)
      become(data.copy(hashToPreimage = hashToPreimage1, pending = data.pending - worker.info), OPERATIONAL)
      doCheck(force = false)

    case (PreimageCheck.CMDCancel, OPERATIONAL) =>
      // User has manually cancelled a check, disconnect all peers
      for (pair <- data.pairs.values) CommsTower forget pair
      become(data, FINALIZED)

    case (PreimageCheck.CMDStart(hosts, hashes), -1) =>
      become(PreimageCheck.CheckData(hosts.map(randomPair).toMap, pending = hosts, hashes), OPERATIONAL)
      for (Tuple2(info, pair) <- data.pairs) CommsTower.listen(listeners1 = Set(listener), pair, info)
      Rx.ioQueue.delay(30.seconds).foreach(_ => me doCheck true)

    case _ =>
  }

  def doCheck(force: Boolean): Unit = {
    // Of all peer replies filter our preimages of interest
    val collected = data.hashToPreimage.filterKeys(data.hashes.contains)

    if (collected.size == data.hashes.size) {
      // We have collected all of our preimages
      me doProcess PreimageCheck.CMDCancel
      onComplete(collected)
    } else if (data.pending.isEmpty) {
      // Finish with whatever we have collected
      me doProcess PreimageCheck.CMDCancel
      onComplete(collected)
    } else if (force) {
      // Finish with whatever we have collected
      me doProcess PreimageCheck.CMDCancel
      onComplete(collected)
    }
  }
}
