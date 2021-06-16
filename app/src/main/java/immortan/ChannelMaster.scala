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
import immortan.utils.{PaymentRequestExt, Rx}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Satoshi}
import immortan.ChannelListener.{Malfunction, Transition}
import fr.acinq.eclair.payment.{IncomingPacket, PaymentRequest}
import fr.acinq.eclair.transactions.{LocalFulfill, RemoteFulfill, RemoteReject}
import immortan.fsm.OutgoingPaymentMaster.CMDChanGotOnline
import fr.acinq.eclair.router.RouteCalculation
import java.util.concurrent.atomic.AtomicLong
import com.google.common.cache.LoadingCache
import immortan.crypto.CanBeShutDown
import rx.lang.scala.Subject
import scala.util.Try


object ChannelMaster {
  type PreimageTry = Try[ByteVector32]
  type PaymentInfoTry = Try[PaymentInfo]
  type RevealedLocalFulfills = Iterable[LocalFulfill]

  type OutgoingAdds = Iterable[UpdateAddHtlc]
  type ReasonableResolutions = Iterable[ReasonableResolution]
  type ReasonableTrampolines = Iterable[ReasonableTrampoline]
  type ReasonableLocals = Iterable[ReasonableLocal]

  final val updateCounter = new AtomicLong(0)
  final val stateUpdateStream: Subject[Long] = Subject[Long]
  final val statusUpdateStream: Subject[Long] = Subject[Long]
  final val paymentDbStream: Subject[Long] = Subject[Long]
  final val relayDbStream: Subject[Long] = Subject[Long]
  final val txDbStream: Subject[Long] = Subject[Long]

  def next(stream: Subject[Long] = null): Unit = stream.onNext(updateCounter.incrementAndGet)

  final val hashRevealStream: Subject[ByteVector32] = Subject[ByteVector32]
  final val remoteFulfillStream: Subject[RemoteFulfill] = Subject[RemoteFulfill]
  final val unknownReestablishStream: Subject[UnknownReestablish] = Subject[UnknownReestablish]

  final val lnPaymentAddedStream: Subject[ByteVector32] = Subject[ByteVector32]
  final val chainTxAddedStream: Subject[ByteVector32] = Subject[ByteVector32]
  final val relayAddedStream: Subject[ByteVector32] = Subject[ByteVector32]
  final val payLinkAddedStream: Subject[String] = Subject[String]

  final val NO_PREIMAGE = ByteVector32.One
  final val NO_SECRET = ByteVector32.Zeroes

  def initResolve(ext: UpdateAddHtlcExt): IncomingResolution = IncomingPacket.decrypt(ext.theirAdd, ext.remoteInfo.nodeSpecificPrivKey) match {
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
    case packet: IncomingPacket.FinalPacket if packet.payload.paymentSecret != NO_SECRET => ReasonableLocal(packet, secret)
    case packet: IncomingPacket.NodeRelayPacket if packet.outerPayload.paymentSecret != NO_SECRET => ReasonableTrampoline(packet, secret)
    case packet: IncomingPacket.ChannelRelayPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
    case packet: IncomingPacket.NodeRelayPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
    case packet: IncomingPacket.FinalPacket => CMD_FAIL_HTLC(LNParams.incorrectDetails(packet.add.amountMsat).asRight, secret, packet.add)
  }

  // Of all incoming payments inside of HCs for which we have revealed a preimage, find those which are dangerously close to expiration
  def dangerousHCRevealed(revealed: Map[ByteVector32, RevealedLocalFulfills], tip: Long, hash: ByteVector32): Iterable[LocalFulfill] =
    revealed.getOrElse(hash, Iterable.empty).filter(tip > _.theirAdd.cltvExpiry.toLong - LNParams.hcFulfillSafetyBlocks)
}

case class InFlightPayments(out: Map[FullPaymentTag, OutgoingAdds], in: Map[FullPaymentTag, ReasonableResolutions] = Map.empty) {
  // Incoming HTLC tag is extracted from onion, corresponsing outgoing HTLC tag is stored in TLV, this way in/out can be linked
  val allTags: Set[FullPaymentTag] = out.keySet ++ in.keySet
}

class ChannelMaster(val payBag: PaymentBag, val chanBag: ChannelBag, val dataBag: DataBag, val pf: PathFinder) extends ChannelListener with ConnectionListener with CanBeShutDown { me =>
  val getPaymentInfoMemo: LoadingCache[ByteVector32, PaymentInfoTry] = memoize(payBag.getPaymentInfo)
  val initResolveMemo: LoadingCache[UpdateAddHtlcExt, IncomingResolution] = memoize(initResolve)
  val getPreimageMemo: LoadingCache[ByteVector32, PreimageTry] = memoize(payBag.getPreimage)

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
      // Note that this method MAY get called multiple times for multipart payments if fulfills happen between restarts
      getPaymentInfoMemo.get(fulfill.ourAdd.paymentHash).filter(_.status != PaymentStatus.SUCCEEDED).foreach { paymentInfo =>
        // Persist payment metadata if this is ACTUALLY the first preimage (otherwise payment would be marked as successful)
        payBag.addSearchablePayment(paymentInfo.description.queryText, fulfill.ourAdd.paymentHash)
        payBag.updOkOutgoing(fulfill, data.usedFee)

        if (data.inFlightParts.nonEmpty) {
          // Sender FSM won't have in-flight parts after restart
          val usedRoutesReport = data.usedRoutesAsString(LNParams.denomination)
          dataBag.putReport(fulfill.ourAdd.paymentHash, usedRoutesReport)
          // We only increment scores for normal channels, never for HCs
          data.successfulUpdates.foreach(pf.normalBag.incrementScore)
        }
      }

      payBag.setPreimage(fulfill.ourAdd.paymentHash, fulfill.theirPreimage)
      getPaymentInfoMemo.invalidate(fulfill.ourAdd.paymentHash)
      getPreimageMemo.invalidate(fulfill.ourAdd.paymentHash)
    }
  }

  val opm: OutgoingPaymentMaster = new OutgoingPaymentMaster(me)
  var inProcessors = Map.empty[FullPaymentTag, IncomingPaymentProcessor]
  var all = Map.empty[ByteVector32, Channel]

  // CONNECTION LISTENER

  // Note that this may be sent multiple times after chain wallet reconnects
  override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit =
    fromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_ONLINE)

  override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
    case msg: Error if msg.channelId == ByteVector32.Zeroes => fromNode(worker.info.nodeId).foreach(_.chan process msg)
    case msg: ChannelReestablish if !all.contains(msg.channelId) => unknownReestablishStream onNext UnknownReestablish(worker, msg)
    case msg: ChannelUpdate => fromNode(worker.info.nodeId).foreach(_.chan process msg)
    case msg: HasChannelId => sendTo(msg, msg.channelId)
    case _ => // Do nothing
  }

  override def onHostedMessage(worker: CommsTower.Worker, message: HostedChannelMessage): Unit = message match {
    case msg: HostedChannelBranding => dataBag.putBranding(worker.info.nodeId, msg)
    case _ => hostedFromNode(worker.info.nodeId).foreach(_ process message)
  }

  override def onDisconnect(worker: CommsTower.Worker): Unit = {
    fromNode(worker.info.nodeId).foreach(_.chan process CMD_SOCKET_OFFLINE)
    Rx.ioQueue.delay(5.seconds).foreach(_ => initConnect)
  }

  // CHANNEL MANAGEMENT

  override def becomeShutDown: Unit = {
    // Outgoing FSMs won't receive anything without channel listeners
    for (channel <- all.values) channel.listeners = Set.empty
    for (fsm <- inProcessors.values) fsm.becomeShutDown
    pf.subscription.unsubscribe
    pf.listeners = Set.empty
  }

  def implantChannel(cs: Commitments, freshChannel: Channel): Unit = {
    // Note that this removes all listeners this channel previously had
    all += Tuple2(cs.channelId, freshChannel)
    freshChannel.listeners = Set(me)
    next(statusUpdateStream)
    initConnect
  }

  def initConnect: Unit =
    all.values.flatMap(Channel.chanAndCommitsOpt).foreach { cnc =>
      // Connect to all peers with channels, including CLOSED ones
      CommsTower.listenNative(Set(me), cnc.commits.remoteInfo)
    }

  // Marks as failed those payments which did not make it into channels before an app has been restarted
  def markAsFailed(paymentInfos: Iterable[PaymentInfo], inFlightOutgoing: Map[FullPaymentTag, OutgoingAdds] = Map.empty): Unit = paymentInfos
    .collect { case outgoingPayInfo if !outgoingPayInfo.isIncoming && outgoingPayInfo.status == PaymentStatus.PENDING => outgoingPayInfo.fullTag }
    .collect { case fullTag if fullTag.tag == PaymentTagTlv.LOCALLY_SENT && !inFlightOutgoing.contains(fullTag) => fullTag.paymentHash }
    .foreach(payBag.updAbortedOutgoing)

  def allInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.allOutgoing).groupBy(_.fullTag)

  def allIncomingRevealed: Map[ByteVector32, RevealedLocalFulfills] = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.revealedFulfills).groupBy(_.theirAdd.paymentHash)

  def pendingRefundsAmount: Satoshi = all.values.map(_.data).collect { case c: DATA_CLOSING => c.forceCloseCommitPublished }.flatten.flatMap(_.delayedRefundsLeft).map(_.txOut.head.amount).sum

  def allHosted: Iterable[ChannelHosted] = all.values.collect { case chan: ChannelHosted => chan }

  def hostedFromNode(nodeId: PublicKey): Option[ChannelHosted] = fromNode(nodeId).collectFirst { case ChanAndCommits(chan: ChannelHosted, _) => chan }

  def fromNode(nodeId: PublicKey): Iterable[ChanAndCommits] = all.values.flatMap(Channel.chanAndCommitsOpt).filter(_.commits.remoteInfo.nodeId == nodeId)

  var sendTo: (Any, ByteVector32) => Unit = (change, channelId) => all.get(channelId).foreach(_ process change)

  // RECEIVE/SEND UTILITIES

  // It is correct to only use availableForReceive for both HC/NC and not take their maxHtlcValueInFlightMsat into account because:
  // - in NC case we always set local NC.maxHtlcValueInFlightMsat to channel capacity so NC.availableForReceive is always less than NC.maxHtlcValueInFlightMsat
  // - in HC case we don't have local HC.maxHtlcValueInFlightMsat at all and only look at HC.availableForReceive

  def operationalCncs(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = chans.filter(Channel.isOperational).flatMap(Channel.chanAndCommitsOpt).toList

  def sortedReceivable(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = operationalCncs(chans).filter(_.commits.updateOpt.isDefined).sortBy(_.commits.availableForReceive)

  def sortedSendable(chans: Iterable[Channel] = Nil): Seq[ChanAndCommits] = operationalCncs(chans).sortBy(_.commits.availableForSend)

  def maxReceivable(sorted: Seq[ChanAndCommits] = Nil): Seq[ChanAndCommits] = {
    // Sorting example: (5/Open, 30/Open, 50/Sleeping, 60/Open, 100/Open) -> (50/Sleeping, 60/Open, 100/Open) -> 60/Open as first one
    val viable = sorted.dropWhile(_.commits.availableForReceive * Math.max(sorted.size - 2, 1) < sorted.last.commits.availableForReceive)
    viable.sortBy(cnc => if (Channel isOperationalAndOpen cnc.chan) 0 else 1)
  }

  def maxSendable(chans: Iterable[Channel] = Nil): MilliSatoshi = {
    val inPrincipleUsableChans = chans.filter(Channel.isOperational)
    val sendableNoFee = opm.getSendable(inPrincipleUsableChans, maxFee = 0L.msat).values.sum
    val theoreticalMaxFee = LNParams.maxOffChainFeeAboveRatio.max(sendableNoFee * LNParams.maxOffChainFeeRatio)
    // Subtract max theoretical fee from EACH channel since ANY channel MAY use ALL of fee reserve
    opm.getSendable(inPrincipleUsableChans, maxFee = theoreticalMaxFee).values.sum
  }

  def makeSendCmd(prExt: PaymentRequestExt, toSend: MilliSatoshi, allowedChans: Seq[Channel], typicalChainTxFee: MilliSatoshi, capLNFeeToChain: Boolean): SendMultiPart = {
    val fullTag = FullPaymentTag(paymentHash = prExt.pr.paymentHash, paymentSecret = prExt.pr.paymentSecret.get, tag = PaymentTagTlv.LOCALLY_SENT)
    val extraEdges = RouteCalculation.makeExtraEdges(prExt.pr.routingInfo, target = prExt.pr.nodeId)

    val feeReserve = toSend * LNParams.maxOffChainFeeRatio match {
      case percent if percent < LNParams.maxOffChainFeeAboveRatio => LNParams.maxOffChainFeeAboveRatio
      case percent if percent > typicalChainTxFee && capLNFeeToChain => typicalChainTxFee
      case percent => percent
    }

    val split = SplitInfo(totalSum = 0L.msat, toSend)
    // Supply relative cltv expiry in case if we initiate a payment when chain tip is not yet known
    val chainExpiry = Right(prExt.pr.minFinalCltvExpiryDelta getOrElse LNParams.minInvoiceExpiryDelta)
    // An assumption is that toSend is at most maxSendable so max theoretically possible off-chain fee is already counted in, so we can send amount + fee
    SendMultiPart(fullTag, chainExpiry, split, LNParams.routerConf, targetNodeId = prExt.pr.nodeId, feeReserve, allowedChans, fullTag.paymentSecret, extraEdges)
  }

  def makePrExt(toReceive: MilliSatoshi, allowedChans: Seq[ChanAndCommits], description: PaymentDescription, preimage: ByteVector32): PaymentRequestExt = {
    // Make a BOLT11 payment request with hints leading to us from provided peer nodes and fake payee nodeId derived from payment hash

    val hash = Crypto.sha256(preimage)
    val invoiceKey = LNParams.secret.keys.fakeInvoiceKey(hash)
    val hops = allowedChans.map(_.commits.updateOpt).zip(allowedChans).collect { case Some(upd) ~ cnc => upd.extraHop(cnc.commits.remoteInfo.nodeId) :: Nil }
    val pr = PaymentRequest(LNParams.chainHash, Some(toReceive), hash, invoiceKey, description.invoiceText, LNParams.incomingFinalCltvExpiry, hops.toList)
    PaymentRequestExt.from(pr)
  }

  def localSend(cmd: SendMultiPart): Unit = {
    opm process CreateSenderFSM(cmd.fullTag, localPaymentListener)
    opm process ClearFailures
    opm process cmd
  }

  def localSendToSelf(sources: List[Channel], destination: ChanAndCommits, preimage: ByteVector32, typicalChainTxFee: MilliSatoshi, capLNFeeToChain: Boolean): Unit = {
    val prExt = makePrExt(maxSendable(sources).min(destination.commits.availableForReceive), List(destination), PlainDescription(split = None, label = None, new String), preimage)
    val keySendCmd = makeSendCmd(prExt, prExt.pr.amount.get, sources, typicalChainTxFee, capLNFeeToChain).copy(userCustomTlvs = GenericTlv(OnionCodecs.keySendNumber, preimage) :: Nil)
    localSend(keySendCmd)
  }

  def checkIfSendable(paymentHash: ByteVector32): Option[Int] =
    opm.data.payments.values.find(fsm => fsm.fullTag.tag == PaymentTagTlv.LOCALLY_SENT && fsm.fullTag.paymentHash == paymentHash) match {
      case Some(outgoingFSM) if PENDING == outgoingFSM.state || INIT == outgoingFSM.state => Some(PaymentInfo.NOT_SENDABLE_IN_FLIGHT) // This payment is pending in FSM
      case _ if getPreimageMemo.get(paymentHash).isSuccess => Some(PaymentInfo.NOT_SENDABLE_SUCCESS) // Preimage has already been revealed for in/out payment
      case _ => None // Has never been either sent or requested, or ABORTED by now
    }

  // These are executed in Channel context

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (_: ChannelTransitionFail, chan: ChannelNormal, _: HasNormalCommitments) =>
      // Execute immediately in same thread to not let channel get updated
      chan doProcess CMD_CLOSE(scriptPubKey = None, force = true)

    case (_: ChannelTransitionFail, chan: ChannelHosted, hc: HostedCommits) =>
      // Execute immediately in same thread to not let channel get updated
      chan.localSuspend(hc, ErrorCodes.ERR_HOSTED_MANUAL_SUSPEND)
  }

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, _, _, prev, CLOSING) if prev != CLOSING =>
      // Previously operational NC got broken
      next(stateUpdateStream)

    case (_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.error.isEmpty && nextHc.error.nonEmpty =>
      // Previously operational HC got suspended
      next(stateUpdateStream)

    case (_, prevHc: HostedCommits, nextHc: HostedCommits, _, _)
      if prevHc.error.nonEmpty && nextHc.error.isEmpty =>
      // Previously suspended HC got operational
      opm process CMDChanGotOnline
      next(stateUpdateStream)

    case (_, _, _, prev, SLEEPING) if prev != SLEEPING =>
      // Channel which was not SLEEPING became SLEEPING
      next(statusUpdateStream)

    case (chan, _, _, prev, OPEN) if prev != OPEN =>
      // Channel which was not OPEN became operational and OPEN
      // We may get here after getting fresh feerates so check again
      chan process CMD_CHECK_FEERATE
      opm process CMDChanGotOnline
      next(statusUpdateStream)
  }

  // Used to notify about an existance of preimage BEFORE new state is committed in origin channel
  // should always be followed by real or simulated state update to let incoming FSMs finalize properly
  override def fulfillReceived(fulfill: RemoteFulfill): Unit = opm process fulfill

  // Used to notify about outgoing adds which can not be committed, or not committed any more
  // also contains invariants which instruct outgoing FSM to abort a payment right away
  override def addRejectedLocally(reason: LocalReject): Unit = opm process reason

  // Used to notify about outgoing adds which were failed by peer AFTER new state is committed in origin channel
  // this means it's safe to retry amounts from these failed payments, there will be no cross-signed duplicates
  override def addRejectedRemotely(reason: RemoteReject): Unit = opm process reason

  override def notifyResolvers: Unit = {
    // Used to notify FSMs that we have cross-signed incoming HTLCs which FSMs may somehow act upon
    val allIns = all.values.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.crossSignedIncoming).map(initResolveMemo.get)
    allIns.foreach { case finalResolve: FinalResolution => sendTo(finalResolve, finalResolve.theirAdd.channelId) case _ => }
    val reasonableIncoming = allIns.collect { case resolution: ReasonableResolution => resolution }.groupBy(_.fullTag)
    val inFlightsBag = InFlightPayments(allInChannelOutgoing, reasonableIncoming)

    inFlightsBag.allTags.collect {
      case fullTag if PaymentTagTlv.TRAMPLOINE_ROUTED == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new TrampolinePaymentRelayer(fullTag, me).tuple
      case fullTag if PaymentTagTlv.FINAL_INCOMING == fullTag.tag && !inProcessors.contains(fullTag) => inProcessors += new IncomingPaymentReceiver(fullTag, me).tuple
      case fullTag if PaymentTagTlv.LOCALLY_SENT == fullTag.tag => opm process CreateSenderFSM(fullTag, localPaymentListener)
    }

    // FSM exists because there were related HTLCs, none may be left now
    // this change is used by existing FSMs to properly finalize themselves
    for (incomingFSM <- inProcessors.values) incomingFSM doProcess inFlightsBag
    // Sign all fails and fulfills that could have been sent from FSMs above
    for (chan <- all.values) chan process CMD_SIGN
    // Maybe remove successful outgoing FSMs
    opm process inFlightsBag
    next(stateUpdateStream)
  }

  // Mainly to prolong FSM timeouts once another add is seen (but not yet committed)
  override def addReceived(add: UpdateAddHtlcExt): Unit = initResolveMemo.getUnchecked(add) match {
    case resolution: ReasonableResolution => inProcessors.get(resolution.fullTag).foreach(_ doProcess resolution)
    case _ => // Do nothing, invalid add will be failed after it gets committed
  }
}
