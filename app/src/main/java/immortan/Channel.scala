package immortan

import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import akka.actor.{Actor, ActorRef, Props}
import immortan.crypto.{CanBeRepliedTo, StateMachine}
import fr.acinq.eclair.transactions.{RemoteFulfill, RemoteReject}
import fr.acinq.eclair.blockchain.CurrentBlockCount
import scala.concurrent.ExecutionContextExecutor
import fr.acinq.eclair.wire.LightningMessage
import immortan.Channel.channelContext
import java.util.concurrent.Executors
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi
import immortan.crypto.Tools.none
import scala.concurrent.Future
import scala.util.Failure


object Channel {
  final val WAIT_FOR_INIT = 0
  final val WAIT_FOR_ACCEPT = 1
  final val WAIT_FUNDING_DONE = 2
  final val SLEEPING = 3
  final val CLOSING = 4
  final val OPEN = 5

  // Single stacking thread for all channels, must be used when asking channels for pending payments to avoid race conditions
  implicit val channelContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext fromExecutor Executors.newSingleThreadExecutor

  def load(listeners: Set[ChannelListener], bag: ChannelBag): Map[ByteVector32, Channel] = bag.all.map {
    case data: HasNormalCommitments => data.channelId -> ChannelNormal.make(listeners, data, LNParams.chainWallet, bag)
    case data: HostedCommits => data.channelId -> ChannelHosted.make(listeners, data, bag)
    case _ => throw new RuntimeException
  }.toMap

  def chanAndCommitsOpt(chan: Channel): Option[ChanAndCommits] = chan.data match {
    case data: HasNormalCommitments => ChanAndCommits(chan, data.commitments).asSome
    case data: HostedCommits => ChanAndCommits(chan, data).asSome
    case _ => None
  }

  def estimateBalance(chan: Channel): MilliSatoshi = chan.data match {
    case data: HasNormalCommitments => data.commitments.localCommit.spec.toLocal
    case data: HostedCommits => data.nextLocalSpec.toLocal
    case _ => MilliSatoshi(0L)
  }

  def isOperational(chan: Channel): Boolean = chan.data match {
    case data: DATA_NORMAL => data.localShutdown.isEmpty && data.remoteShutdown.isEmpty
    case hostedCommits: HostedCommits => hostedCommits.error.isEmpty
    case _ => false
  }

  def isWaiting(chan: Channel): Boolean = chan.data match {
    case _: DATA_WAIT_FOR_FUNDING_CONFIRMED => true
    case _: DATA_WAIT_FOR_FUNDING_LOCKED => true
    case _ => false
  }

  def isOperationalOrWaiting(chan: Channel): Boolean = isOperational(chan) || isWaiting(chan)
  def isOperationalAndOpen(chan: Channel): Boolean = isOperational(chan) && OPEN == chan.state
  def isOperationalAndSleeping(chan: Channel): Boolean = isOperational(chan) && SLEEPING == chan.state
}

trait Channel extends StateMachine[ChannelData] with CanBeRepliedTo { me =>
  def process(changeMsg: Any): Unit = Future(me doProcess changeMsg).onComplete {
    case Failure(reason) => events onException Tuple3(me, data, reason)
    case _ => // Do nothing
  }

  def SEND(msg: LightningMessage*): Unit

  def STORE(data: PersistentChannelData): PersistentChannelData

  def BECOME(data1: ChannelData, state1: Int): Unit = {
    // Transition must be defined before vars are updated
    val trans = (me, data, data1, state, state1)
    super.become(data1, state1)
    events.onBecome(trans)
  }

  def StoreBecomeSend(data1: PersistentChannelData, state1: Int, lnMessage: LightningMessage*): Unit = {
    // Storing goes first to ensure we retain an updated data before revealing it if anything goes wrong

    STORE(data1)
    BECOME(data1, state1)
    SEND(lnMessage:_*)
  }

  var listeners = Set.empty[ChannelListener]

  val events: ChannelListener = new ChannelListener {
    override def onException: PartialFunction[ChannelListener.Malfunction, Unit] = {
      case failure => for (lst <- listeners if lst.onException isDefinedAt failure) lst onException failure
    }

    override def onBecome: PartialFunction[ChannelListener.Transition, Unit] = {
      case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition
    }

    override def stateUpdated(rejects: Seq[RemoteReject] = Nil): Unit = for (lst <- listeners) lst.stateUpdated(rejects)
    override def localAddRejected(reason: LocalAddRejected): Unit = for (lst <- listeners) lst.localAddRejected(reason)
    override def fulfillReceived(fulfill: RemoteFulfill): Unit = for (lst <- listeners) lst.fulfillReceived(fulfill)
    override def addReceived(add: UpdateAddHtlcExt): Unit = for (lst <- listeners) lst.addReceived(add)
  }

  val receiver: ActorRef = LNParams.system actorOf Props(new ActorEventsReceiver)

  class ActorEventsReceiver extends Actor {
    context.system.eventStream.subscribe(channel = classOf[CurrentBlockCount], subscriber = self)
    override def receive: Receive = main(lastSeenBlockCount = None, useDelay = true)

    def main(lastSeenBlockCount: Option[CurrentBlockCount], useDelay: Boolean): Receive = {
      case currentBlockCount: CurrentBlockCount if lastSeenBlockCount.isEmpty && useDelay =>
        // Delay first block count propagation to give peer a last change to resolve on reconnect
        context.system.scheduler.scheduleOnce(10.seconds)(self ! "propagate")(LNParams.ec)
        context become main(currentBlockCount.asSome, useDelay = true)

      case currentBlockCount: CurrentBlockCount if lastSeenBlockCount.isDefined && useDelay =>
        // Replace block count with a new one if this happens while propagation is being delayed
        context become main(currentBlockCount.asSome, useDelay = true)

      case "propagate" if lastSeenBlockCount.isDefined =>
        // Propagate all subsequent block counts right away
        context become main(None, useDelay = false)
        // Popagate the last delayed block count
        process(lastSeenBlockCount.get)

      case msg =>
        process(msg)
    }
  }
}

object ChannelListener {
  type Malfunction = (Channel, ChannelData, Throwable)
  type Transition = (Channel, ChannelData, ChannelData, Int, Int)
}

trait ChannelListener {
  def onException: PartialFunction[ChannelListener.Malfunction, Unit] = none
  def onBecome: PartialFunction[ChannelListener.Transition, Unit] = none
  def stateUpdated(rejects: Seq[RemoteReject] = Nil): Unit = none
  def localAddRejected(reason: LocalAddRejected): Unit = none
  def fulfillReceived(fulfill: RemoteFulfill): Unit = none
  def addReceived(add: UpdateAddHtlcExt): Unit = none
}

case class ChanAndCommits(chan: Channel, commits: Commitments)
