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
import java.util.concurrent.atomic.AtomicLong
import fr.acinq.eclair.payment.IncomingPacket
import com.google.common.cache.LoadingCache
import fr.acinq.bitcoin.ByteVector32
import rx.lang.scala.Subject
import immortan.utils.Rx
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
    // or peer sends a message targeted to non-exiting local channel
    new StateMachine[ChannelData] with CanBeRepliedTo {
      def process(change: Any): Unit = doProcess(change)
      def doProcess(change: Any): Unit = none
    }

  final val NO_PREIMAGE = ByteVector32.One

  final val NO_SECRET = ByteVector32.Zeroes

  final val updateCounter = new AtomicLong(0)

  final val stateUpdateStream: Subject[Long] = Subject[Long]

  final val statusUpdateStream: Subject[Long] = Subject[Long]

  def notifyStateUpdated: Unit = stateUpdateStream.onNext(updateCounter.incrementAndGet)

  def notifyStatusUpdated: Unit = statusUpdateStream.onNext(updateCounter.incrementAndGet)

  def initResolve(ext: UpdateAddHtlcExt): IncomingResolution = IncomingPacket.decrypt(ext.theirAdd, ext.remoteInfo.nodeSpecificPrivKey) match {
    case _ if LNParams.isChainDisconnectedTooLong => CMD_FAIL_HTLC(Right(TemporaryNodeFailure), ext.remoteInfo.nodeSpecificPrivKey, ext.theirAdd)
    case Left(_: BadOnion) => fallbackResolve(secret = LNParams.secret.keys.fakeInvoiceKey(ext.theirAdd.paymentHash), ext.theirAdd)
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

  val localPaymentListener: OutgoingPaymentListener = new OutgoingPaymentListener {
    override def wholePaymentSucceeded(data: OutgoingPaymentSenderData): Unit =
      opm process RemoveSenderFSM(data.cmd.fullTag)

    override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = chanBag.db txWrap {
      // This method gets called after NO payment parts are left in system, irregardless of restarts
      val failureReport = data.failuresAsString(LNParams.denomination)
      dataBag.putReport(data.cmd.fullTag.paymentHash, failureReport)
      payBag.updAbortedOutgoing(data.cmd.fullTag.paymentHash)
      opm process RemoveSenderFSM(data.cmd.fullTag)
    }

    override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = chanBag.db txWrap {
      // also note that this method MAY get called multiple times for multipart payments if fulfills happen between restarts
      payBag.setPreimage(fulfill.ourAdd.paymentHash, fulfill.preimage)

      getPaymentInfoMemo.get(fulfill.ourAdd.paymentHash).filter(_.status != PaymentStatus.SUCCEEDED).foreach { paymentInfo =>
        // Persist various payment metadata if this is ACTUALLY the first preimage (otherwise payment would be marked as successful)
        payBag.addSearchablePayment(paymentInfo.description.queryText, fulfill.ourAdd.paymentHash)
        payBag.updOkOutgoing(fulfill, data.usedFee)

        if (data.inFlightParts.nonEmpty) {
          // Sender FSM won't have in-flight parts after restart
          val usedRoutesReport = data.usedRoutesAsString(LNParams.denomination)
          dataBag.putReport(fulfill.ourAdd.paymentHash, usedRoutesReport)
          data.successfulUpdates.foreach(pf.normalBag.incrementScore)
        }
      }

      getPaymentInfoMemo.invalidate(fulfill.ourAdd.paymentHash)
      getPreimageMemo.invalidate(fulfill.ourAdd.paymentHash)
    }
  }

  val opm: OutgoingPaymentMaster = new OutgoingPaymentMaster(me)
  var inProcessors = Map.empty[FullPaymentTag, IncomingPaymentProcessor]
  var all = Map.empty[ByteVector32, Channel]

  // CHANNEL MANAGEMENT

  override def becomeShutDown: Unit = {
    // Outgoing FSMs won't receive anything without channel listeners
    for (channel <- all.values) channel.listeners = Set.empty
    for (fsm <- inProcessors.values) fsm.becomeShutDown
    pf.listeners = Set.empty
  }

  def implantChannel(cs: Commitments, freshChannel: Channel): Unit = {
    // Note that this removes all listeners this channel previously had
    all += Tuple2(cs.channelId, freshChannel)
    freshChannel.listeners = Set(me)
    notifyStatusUpdated
    initConnect
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

  // It is correct to only use availableForReceive for both HC/NC and not take their maxHtlcValueInFlightMsat into account because:
  // - in NC case we always set local NC.maxHtlcValueInFlightMsat to channel capacity so NC.availableForReceive is always less than NC.maxHtlcValueInFlightMsat
  // - in HC case we don't have local HC.maxHtlcValueInFlightMsat at all and only look at HC.availableForReceive

  def receivableSorted: Seq[ChanAndCommits] = all.values
    .filter(Channel.isOperational).flatMap(Channel.chanAndCommitsOpt)
    .filter(_.commits.updateOpt.isDefined).toList.sortBy(_.commits.availableForReceive)

  // Example: (5/O, 30/O, 50/S, 60/O, 100/O) -> (50/Sleeping, 60/Open, 100/Open) -> 60
  // the idea is for any OPEN channel to be able to get a smallest remaining channel receivable
  def maxReceivableSingle(sorted: Seq[ChanAndCommits] = Nil): Seq[ChanAndCommits] = receivableSorted
    .dropWhile(_.commits.availableForReceive * Math.max(sorted.size - 2, 1) <= sorted.last.commits.availableForReceive)
    .sortBy(cnc => Channel isOperationalAndOpen cnc.chan compare false)

  type CommitsAndTotal = (Seq[ChanAndCommits], MilliSatoshi)
  // Example: (5/O, 50/S, 60/O, 100/O) -> (50/Sleeping, 60/Open, 100/Open) -> 50*3 = 150
  // the idea is for smallest remaining channel to be able to handle an evenly split amount
  def maxReceivableMany(sorted: Seq[ChanAndCommits], takeAtMostChannels: Int): Option[CommitsAndTotal] = {
    val withoutSmall = sorted.dropWhile(_.commits.availableForReceive * sorted.size < sorted.last.commits.availableForReceive).takeRight(takeAtMostChannels)
    val candidates = for (cs <- withoutSmall.indices map withoutSmall.drop) yield (cs, cs.head.commits.availableForReceive * cs.size)
    if (candidates.isEmpty) None else candidates.maxBy { case (_, totalReceivable) => totalReceivable }.toSome
  }

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

    // An FSM may have been created, but now no related payments are left
    // this specific change is used by FSMs to properly finalize themselves
    for (inFSM <- inProcessors.values) inFSM doProcess bag
    for (reject <- rejects) opm process reject
    opm process bag
  }

  // These are executed in Channel context

  override def fulfillReceived(fulfill: RemoteFulfill): Unit = opm process fulfill

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (_, _, commandError: CMDException) => opm process commandError
  }

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, _, _, WAIT_FUNDING_DONE | SLEEPING | SUSPENDED, OPEN) =>
      opm process OutgoingPaymentMaster.CMDChanGotOnline
      notifyStatusUpdated

    case (_, _, _, OPEN, SLEEPING | SUSPENDED | CLOSING) =>
      notifyStatusUpdated

    case (_: ChannelNormal, _, _, SLEEPING, CLOSING) =>
      notifyStatusUpdated
  }

  override def stateUpdated(rejects: Seq[RemoteReject] = Nil): Unit = {
    // Outgoing FSM should have been created on app startup, here we specifically do not ever recreate it
    notifyFSMs(allInChannelOutgoing, allIncomingResolutions, rejects, makeMissingOutgoingFSM = false)
    notifyStateUpdated
  }

  // Mainly to prolong timeouts
  override def addReceived(add: UpdateAddHtlcExt): Unit = for {
    resolve <- Option(initResolveMemo get add) collect { case resolve: ReasonableResolution => resolve }
    incomingFSM <- inProcessors.values.find(incomingFSM => resolve.fullTag == incomingFSM.fullTag)
  } incomingFSM doProcess resolve
}
