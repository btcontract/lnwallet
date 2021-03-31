package immortan

import immortan.fsm._
import fr.acinq.eclair._
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import immortan.PaymentStatus._
import immortan.ChannelMaster._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._

import fr.acinq.eclair.transactions.{RemoteFulfill, RemoteReject}
import immortan.ChannelListener.{Malfunction, Transition}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import immortan.crypto.{CanBeRepliedTo, StateMachine}
import immortan.utils.{Rx, WalletEventsListener}

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.blockchain.CurrentBlockCount
import fr.acinq.eclair.payment.IncomingPacket
import com.google.common.cache.LoadingCache
import fr.acinq.bitcoin.ByteVector32
import scala.util.Try


object ChannelMaster {
  type PreimageTry = Try[ByteVector32]
  type PaymentInfoTry = Try[PaymentInfo]

  type OutgoingAdds = Iterable[UpdateAddHtlc]
  type ReasonableResolutions = Iterable[ReasonableResolution]
  type ReasonableTrampolines = Iterable[ReasonableTrampoline]
  type ReasonableLocals = Iterable[ReasonableLocal]

  var NO_CHANNEL: StateMachine[ChannelData] with CanBeRepliedTo =
    // It's possible that user removes an HC from system at runtime
    // or that peer sends a message targeted to non-exiting local channel
    new StateMachine[ChannelData] with CanBeRepliedTo {
      def process(change: Any): Unit = doProcess(change)
      def doProcess(change: Any): Unit = none
    }

  final val NO_SECRET = ByteVector32.Zeroes

  final val NO_PREIMAGE = ByteVector32.One

  def initResolve(ext: UpdateAddHtlcExt): IncomingResolution = IncomingPacket.decrypt(ext.theirAdd, ext.remoteInfo.nodeSpecificPrivKey) match {
    case _ if LNParams.chainDisconnectedForTooLong => CMD_FAIL_HTLC(Right(TemporaryNodeFailure), ext.remoteInfo.nodeSpecificPrivKey, ext.theirAdd)
    case Left(_: BadOnion) => fallbackResolve(secret = LNParams.format.keys.fakeInvoiceKey(ext.theirAdd.paymentHash), ext.theirAdd)
    case Left(onionFailure) => CMD_FAIL_HTLC(Right(onionFailure), ext.remoteInfo.nodeSpecificPrivKey, ext.theirAdd)
    case Right(packet: IncomingPacket) => defineResolution(ext.remoteInfo.nodeSpecificPrivKey, packet)
  }

  def fallbackResolve(secret: PrivateKey, theirAdd: UpdateAddHtlc): IncomingResolution = IncomingPacket.decrypt(theirAdd, secret) match {
    case Left(failure: BadOnion) => CMD_FAIL_MALFORMED_HTLC(failure.onionHash, failureCode = failure.code, theirAdd)
    case Left(onionFailure) => CMD_FAIL_HTLC(Right(onionFailure), secret, theirAdd)
    case Right(packet: IncomingPacket) => defineResolution(secret, packet)
  }

  // Make sure incoming payment secret is always present
  private def defineResolution(secret: PrivateKey, pkt: IncomingPacket): IncomingResolution = pkt match {
    case packet: IncomingPacket.FinalPacket if packet.payload.paymentSecret.exists(_ != NO_SECRET) => ReasonableLocal(packet, secret)
    case packet: IncomingPacket.NodeRelayPacket if packet.outerPayload.paymentSecret.exists(_ != NO_SECRET) => ReasonableTrampoline(packet, secret)
    case packet: IncomingPacket.ChannelRelayPacket => CMD_FAIL_HTLC(Right(LNParams incorrectDetails packet.add.amountMsat), secret, packet.add)
    case packet: IncomingPacket.NodeRelayPacket => CMD_FAIL_HTLC(Right(LNParams incorrectDetails packet.add.amountMsat), secret, packet.add)
    case packet: IncomingPacket.FinalPacket => CMD_FAIL_HTLC(Right(LNParams incorrectDetails packet.add.amountMsat), secret, packet.add)
  }
}

case class InFlightPayments(out: Map[FullPaymentTag, OutgoingAdds], in: Map[FullPaymentTag, ReasonableResolutions] = Map.empty) {
  // Incoming HTLC tag is extracted from onion, corresponsing outgoing HTLC tag is stored in TLV, this way in/out can be linked
  val allTags: Set[FullPaymentTag] = out.keySet ++ in.keySet
}

class ChannelMaster(val payBag: PaymentBag, val chanBag: ChannelBag, val dataBag: DataBag, val pf: PathFinder) extends ChannelListener { me =>
  val getPaymentInfoMemo: LoadingCache[ByteVector32, PaymentInfoTry] = memoize(payBag.getPaymentInfo)
  val initResolveMemo: LoadingCache[UpdateAddHtlcExt, IncomingResolution] = memoize(initResolve)
  val getPreimageMemo: LoadingCache[ByteVector32, PreimageTry] = memoize(payBag.getPreimage)

  val sockChannelBridge: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      fromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_ONLINE)
    }

    override def onMessage(worker: CommsTower.Worker, msg: LightningMessage): Unit = msg match {
      case nodeError: Error if nodeError.channelId == ByteVector32.Zeroes => fromNode(worker.info.nodeId).foreach(_.chan process nodeError)
      case channelUpdate: ChannelUpdate => fromNode(worker.info.nodeId).foreach(_.chan process channelUpdate)
      case message: HasChannelId => sendTo(message, message.channelId)
      case _ => // Do nothing
    }

    override def onHostedMessage(worker: CommsTower.Worker, msg: HostedChannelMessage): Unit = msg match {
      case branding: HostedChannelBranding => dataBag.putBranding(worker.info.nodeId, branding)
      case _ => hostedFromNode(worker.info.nodeId).foreach(_ process msg)
    }

    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      fromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_OFFLINE)
      delayedReconnect(worker)
    }
  }

  // Keep track of chain height
  // Keep track of last disconnect stamp
  // Notify channels about current chain height
  // Connect sockets once chain height becomes known
  val chainChannelBridge: WalletEventsListener = new WalletEventsListener {
    override def onCurrentBlockCount(event: CurrentBlockCount): Unit = {
      LNParams.blockCount.set(event.blockCount)
      all.values.foreach(_ process event)
    }

    override def onWalletReady(event: WalletReady): Unit = {
      LNParams.blockCount.set(event.height)
      LNParams.lastDisconnect = None
      initConnect
    }

    override def onElectrumDisconnected: Unit = {
      LNParams.lastDisconnect = System.currentTimeMillis.toSome
    }
  }

  val opm: OutgoingPaymentMaster = new OutgoingPaymentMaster(me)
  var inProcessors = Map.empty[FullPaymentTag, IncomingPaymentProcessor]
  var all = Map.empty[ByteVector32, Channel]
  pf.listeners += opm

  // CHANNEL MANAGEMENT

  def delayedReconnect(worker: CommsTower.Worker): Unit =
    // Reconnect by default, but may be overridden if needed
    Rx.ioQueue.delay(5.seconds).foreach(_ => initConnect)

  def implantChannel(cs: Commitments, freshChannel: Channel): Unit = {
    all += Tuple2(cs.channelId, freshChannel) // Put this channel to vector of established channels
    freshChannel.listeners = Set(me) // REPLACE with standard channel listeners to new established channel
    initConnect // Add standard connection listeners for this peer
  }

  def initConnect: Unit = {
    val eligibleForConnect = all.values.filter(Channel.isOperationalOrWaiting).flatMap(Channel.chanAndCommitsOpt)
    for (cnc <- eligibleForConnect) CommsTower.listenNative(Set(sockChannelBridge), cnc.commits.remoteInfo)
  }

  def allIncomingResolutions: Iterable[IncomingResolution] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.crossSignedIncoming).map(initResolveMemo.get)
  def allInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.allOutgoing).groupBy(_.fullTag)

  def fromNode(nodeId: PublicKey): Iterable[ChanAndCommits] = all.values.flatMap(Channel.chanAndCommitsOpt).filter(_.commits.remoteInfo.nodeId == nodeId)
  def hostedFromNode(nodeId: PublicKey): Option[ChannelHosted] = fromNode(nodeId: PublicKey).collectFirst { case ChanAndCommits(chan: ChannelHosted, _) => chan }
  def sendTo(change: Any, chanId: ByteVector32): Unit = all.getOrElse(chanId, NO_CHANNEL) process change

  // RECEIVE/SEND UTILITIES

  // Example: (5, 30, 50, 60, 100) -> (50, 60, 100), receivable will be 50 (the idea is for any remaining channel to be able to get a smallest remaining channel receivable)
  def maxSortedReceivables(sorted: Seq[ChanAndCommits] = Nil): Seq[ChanAndCommits] = sorted.dropWhile(_.commits.availableForReceive * Math.max(sorted.size - 2, 1) <= sorted.last.commits.availableForReceive).takeRight(4)
  def sortedReceivables: Seq[ChanAndCommits] = all.values.filter(Channel.isOperational).flatMap(Channel.chanAndCommitsOpt).filter(_.commits.updateOpt.isDefined).toList.sortBy(_.commits.availableForReceive)

  def maxSendable: MilliSatoshi = {
    val chans = all.values.filter(Channel.isOperational)
    val sendableNoFee = opm.getSendable(chans, maxFee = 0L.msat).values.sum
    // Subtract max send fee from EACH channel since ANY channel MAY use all of it
    opm.getSendable(chans, maxFee = sendableNoFee * LNParams.offChainFeeRatio).values.sum
  }

  def checkIfSendable(tag: FullPaymentTag, amount: MilliSatoshi): Int = opm.data.payments.get(tag) match {
    case Some(outgoingFSM) if PENDING == outgoingFSM.state || INIT == outgoingFSM.state => PaymentInfo.NOT_SENDABLE_IN_FLIGHT // This payment is pending in FSM
    case Some(outgoingFSM) if SUCCEEDED == outgoingFSM.state => PaymentInfo.NOT_SENDABLE_SUCCESS // This payment has just been fulfilled at runtime
    case _ if getPreimageMemo.get(tag.paymentHash).isSuccess => PaymentInfo.NOT_SENDABLE_SUCCESS // Preimage has already been revealed
    case _ if LNParams.chainDisconnectedForTooLong => PaymentInfo.NOT_SENDABLE_CHAIN_DISCONNECT // Chain wallet is lagging
    case _ if amount > maxSendable => PaymentInfo.NOT_SENDABLE_LOW_FUNDS // Not enough funds in a wallet
    case _ => PaymentInfo.SENDABLE // Has never been sent or ABORTED by now
  }

  def notifyFSMs(out: Map[FullPaymentTag, OutgoingAdds], in: Iterable[IncomingResolution], rejects: Seq[RemoteReject], makeMissingOutgoingFSM: Boolean): Unit = {
    in.foreach { case finalResolve: FinalResolution => sendTo(finalResolve, finalResolve.theirAdd.channelId) case _ => } // First, immediately resolve invalid adds
    val partialIncoming = in.collect { case resolve: ReasonableResolution => resolve }.groupBy(_.fullTag) // Then, collect reasonable adds which need further analysis
    val bag = InFlightPayments(out, partialIncoming)

    bag.allTags.foreach {
      case fullTag if PaymentTagTlv.TRAMPLOINE_ROUTED == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new TrampolinePaymentRelayer(fullTag, me).tuple
      case fullTag if PaymentTagTlv.FINAL_INCOMING == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new IncomingPaymentReceiver(fullTag, me).tuple
      case fullTag if PaymentTagTlv.LOCALLY_SENT == fullTag.tag && makeMissingOutgoingFSM => opm process CreateSenderFSM(fullTag, localPaymentListener)
      case _ => // Do nothing
    }

    // Incoming FSM may have been created, but now no related incoming or outgoing payments are left
    // this is used by incoming FSMs to finalize themselves including removal from `inProcessors` map
    inProcessors.values.foreach(_ doProcess bag)
    rejects.foreach(opm.process)
  }

  // These are executed in Channel context

  override def onException: PartialFunction[Malfunction, Unit] = { case (_, error: CMDException) => opm process error }

  override def onBecome: PartialFunction[Transition, Unit] = { case (_, _, _, SLEEPING | SUSPENDED, OPEN) => opm process OutgoingPaymentMaster.CMDChanGotOnline }

  override def stateUpdated(rejects: Seq[RemoteReject] = Nil): Unit = notifyFSMs(allInChannelOutgoing, allIncomingResolutions, rejects, makeMissingOutgoingFSM = false)

  override def fulfillReceived(fulfill: RemoteFulfill): Unit = opm process fulfill

  // Mainly to prolong timeouts
  override def addReceived(add: UpdateAddHtlcExt): Unit = initResolveMemo.get(add) match {
    case resolve: ReasonableTrampoline => inProcessors.values.find(_.fullTag == resolve.fullTag).foreach(_ doProcess resolve)
    case resolve: ReasonableLocal => inProcessors.values.find(_.fullTag == resolve.fullTag).foreach(_ doProcess resolve)
    case _ => // Do nothing
  }

  // Executed in OutgoingPaymentMaster context

  val localPaymentListener: OutgoingPaymentEvents = new OutgoingPaymentEvents {
    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = chanBag.db txWrap {
      dataBag.putReport(data.cmd.fullTag.paymentHash, data failuresAsString LNParams.denomination)
      payBag.updAbortedOutgoing(data.cmd.fullTag.paymentHash)
      opm process RemoveSenderFSM(data.cmd.fullTag)
    }

    override def preimageRevealed(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = {
      // We have just seen a first preimage for locally initiated outgoing payment

      chanBag.db txWrap {
        // First, unconditionally persist a preimage before doing anything else
        payBag.addPreimage(fulfill.ourAdd.paymentHash, fulfill.preimage)
        // Should be silently disregarded if this is a routed payment
        payBag.updOkOutgoing(fulfill, data.usedFee)
      }

      chanBag.db txWrap {
        payBag.getPaymentInfo(fulfill.ourAdd.paymentHash).foreach { paymentInfo =>
          dataBag.putReport(fulfill.ourAdd.paymentHash, data usedRoutesAsString LNParams.denomination)
          payBag.addSearchablePayment(paymentInfo.description.queryText, fulfill.ourAdd.paymentHash)
          for (ext <- data.successfulUpdates) pf.normalStore.incrementChannelScore(ext.update)
        }
      }

      getPaymentInfoMemo.invalidate(fulfill.ourAdd.paymentHash)
      getPreimageMemo.invalidate(fulfill.ourAdd.paymentHash)
    }
  }
}
