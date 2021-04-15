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
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import immortan.ChannelListener.{Malfunction, Transition}
import fr.acinq.eclair.transactions.{RemoteFulfill, RemoteReject}
import immortan.crypto.{CanBeRepliedTo, CanBeShutDown, StateMachine}
import immortan.utils.{FeeRatesInfo, FeeRatesListener, Rx, WalletEventsListener}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import java.util.concurrent.atomic.AtomicLong
import fr.acinq.eclair.payment.IncomingPacket
import com.google.common.cache.LoadingCache
import fr.acinq.bitcoin.ByteVector32
import rx.lang.scala.Subject
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

  final val updateCounter = new AtomicLong(0)

  final val stateUpdateStream: Subject[Long] = Subject[Long]

  final val statusUpdateStream: Subject[ChannelData] = Subject[ChannelData]

  final val NO_SECRET = ByteVector32.Zeroes

  final val NO_PREIMAGE = ByteVector32.One

  def initResolve(ext: UpdateAddHtlcExt): IncomingResolution = IncomingPacket.decrypt(ext.theirAdd, ext.remoteInfo.nodeSpecificPrivKey) match {
    case _ if LNParams.isChainDisconnectedTooLong => CMD_FAIL_HTLC(Right(TemporaryNodeFailure), ext.remoteInfo.nodeSpecificPrivKey, ext.theirAdd)
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

class ChannelMaster(val payBag: PaymentBag, val chanBag: ChannelBag, val dataBag: DataBag, val pf: PathFinder) extends ChannelListener with CanBeShutDown { me =>
  val getPaymentInfoMemo: LoadingCache[ByteVector32, PaymentInfoTry] = memoize(payBag.getPaymentInfo)
  val initResolveMemo: LoadingCache[UpdateAddHtlcExt, IncomingResolution] = memoize(initResolve)
  val getPreimageMemo: LoadingCache[ByteVector32, PreimageTry] = memoize(payBag.getPreimage)

  val chainChannelListener: WalletEventsListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = {
      // Sync is complete now, we can start channel connections
      // Invalidate last disconnect stamp since we're up again
      LNParams.lastDisconnect.set(Long.MaxValue)
      LNParams.blockCount.set(event.height)
      initConnect
    }

    override def onChainDisconnected: Unit = {
      // Remember to eventually stop accepting payments
      // note that there may be many these events in a row
      LNParams.lastDisconnect.set(System.currentTimeMillis)
    }
  }

  val feeRatesListener: FeeRatesListener = new FeeRatesListener {
    // Unless anchor outputs are used we need to priodically adjust channel fee-rates to avoid force-closing
    def onFeeRates(rates: FeeRatesInfo): Unit = all.values.foreach(_ process CMD_CHECK_FEERATE)
  }

  val socketChannelListener: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      // Note that this may be sent multiple times after chain wallet reconnects
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
      Rx.ioQueue.delay(5.seconds).foreach(_ => initConnect)
    }
  }

  val localPaymentListener: OutgoingListener = new OutgoingListener {
    override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = chanBag.db txWrap {
      // Note that fully SUCCEEDED outgoing FSM for locally initiated payments won't get removed from OutgoingPaymentMaster
      // also note that this method MAY get called multiple times for multipart payments if fulfills happen between restarts
      payBag.setPreimage(fulfill.ourAdd.paymentHash, fulfill.preimage)

      getPaymentInfoMemo.get(fulfill.ourAdd.paymentHash).filter(_.status != PaymentStatus.SUCCEEDED).foreach { paymentInfo =>
        // Persist various payment metadata if this is ACTUALLY the first preimage (otherwise payment would be marked as successful)
        payBag.addSearchablePayment(paymentInfo.description.queryText, fulfill.ourAdd.paymentHash)
        payBag.updOkOutgoing(fulfill, data.usedFee)

        if (data.inFlightParts.nonEmpty) {
          // Sender FSM won't have in-flight parts after restart
          val usedRoutesReport = data.usedRoutesAsString(LNParams.denomination)
          for (ext <- data.successfulUpdates) pf.normalBag.incrementScore(ext.update)
          dataBag.putReport(fulfill.ourAdd.paymentHash, usedRoutesReport)
        }
      }

      getPaymentInfoMemo.invalidate(fulfill.ourAdd.paymentHash)
      getPreimageMemo.invalidate(fulfill.ourAdd.paymentHash)
    }

    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = chanBag.db txWrap {
      // This method gets called after NO payment parts are left in system, irregardless of restarts
      val failureReport = data.failuresAsString(LNParams.denomination)
      dataBag.putReport(data.cmd.fullTag.paymentHash, failureReport)
      payBag.updAbortedOutgoing(data.cmd.fullTag.paymentHash)
      opm process RemoveSenderFSM(data.cmd.fullTag)
    }
  }

  val opm: OutgoingPaymentMaster = new OutgoingPaymentMaster(me)
  var inProcessors = Map.empty[FullPaymentTag, IncomingPaymentProcessor]
  var all = Map.empty[ByteVector32, Channel]

  // CHANNEL MANAGEMENT

  override def becomeShutDown: Unit = {
    for (channel <- all.values) channel.listeners = Set.empty
    for (fsm <- inProcessors.values) fsm.becomeShutDown
    pf.listeners = Set.empty
  }

  def implantChannel(cs: Commitments, freshChannel: Channel): Unit = {
    all += Tuple2(cs.channelId, freshChannel) // Put this channel to vector of established channels
    freshChannel.listeners = Set(me) // REPLACE with standard channel listeners to new established channel
    initConnect // Add standard connection listeners for this peer
  }

  def initConnect: Unit = {
    val eligibleForConnect = all.values.filter(Channel.isOperationalOrWaiting).flatMap(Channel.chanAndCommitsOpt)
    for (cnc <- eligibleForConnect) CommsTower.listenNative(Set(socketChannelListener), cnc.commits.remoteInfo)
  }

  def allIncomingResolutions: Iterable[IncomingResolution] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.crossSignedIncoming).map(initResolveMemo.get)
  def allInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.allOutgoing).groupBy(_.fullTag)

  def fromNode(nodeId: PublicKey): Iterable[ChanAndCommits] = all.values.flatMap(Channel.chanAndCommitsOpt).filter(_.commits.remoteInfo.nodeId == nodeId)
  def hostedFromNode(nodeId: PublicKey): Option[ChannelHosted] = fromNode(nodeId).collectFirst { case ChanAndCommits(chan: ChannelHosted, _) => chan }
  def allHosted: Map[ByteVector32, ChannelHosted] = all.collect { case (channelId, hostedChannel: ChannelHosted) => channelId -> hostedChannel }
  def sendTo(change: Any, chanId: ByteVector32): Unit = all.getOrElse(chanId, NO_CHANNEL) process change

  // RECEIVE/SEND UTILITIES

  // Example: (5, 30, 50, 60, 100) -> (50, 60, 100), receivable will be 50 (the idea is for any remaining channel to be able to get a smallest remaining channel receivable)
  def maxSortedReceivables(sorted: Seq[ChanAndCommits] = Nil): Seq[ChanAndCommits] = sorted.dropWhile(_.commits.availableForReceive * Math.max(sorted.size - 2, 1) <= sorted.last.commits.availableForReceive)
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
    case _ if LNParams.isChainDisconnectedTooLong => PaymentInfo.NOT_SENDABLE_CHAIN_DISCONNECT // Chain wallet is lagging
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

  override def fulfillReceived(fulfill: RemoteFulfill): Unit = opm process fulfill

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (_, error: CMDException) =>
      opm process error
  }

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, _, data1, OPEN, SLEEPING | SUSPENDED | CLOSING) =>
      statusUpdateStream.onNext(data1)

    case (_: ChannelNormal, _, data1, SLEEPING, CLOSING) =>
      statusUpdateStream.onNext(data1)

    case (_, _, data1, SLEEPING | SUSPENDED, OPEN) =>
      opm process OutgoingPaymentMaster.CMDChanGotOnline
      statusUpdateStream.onNext(data1)
  }

  override def stateUpdated(rejects: Seq[RemoteReject] = Nil): Unit = {
    notifyFSMs(allInChannelOutgoing, allIncomingResolutions, rejects, makeMissingOutgoingFSM = false)
    stateUpdateStream.onNext(updateCounter.incrementAndGet)
  }

  // Mainly to prolong timeouts
  override def addReceived(add: UpdateAddHtlcExt): Unit = initResolveMemo.get(add) match {
    case resolve: ReasonableTrampoline => inProcessors.values.find(_.fullTag == resolve.fullTag).foreach(_ doProcess resolve)
    case resolve: ReasonableLocal => inProcessors.values.find(_.fullTag == resolve.fullTag).foreach(_ doProcess resolve)
    case _ => // Do nothing
  }
}
