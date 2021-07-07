package immortan.fsm

import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import immortan.fsm.IncomingPaymentProcessor._
import fr.acinq.eclair.channel.{CMD_FAIL_HTLC, CMD_FULFILL_HTLC, ReasonableLocal, ReasonableTrampoline}
import immortan.{Channel, ChannelMaster, InFlightPayments, LNParams, PaymentStatus}
import immortan.ChannelMaster.{ReasonableLocals, ReasonableTrampolines}
import immortan.crypto.{CanBeShutDown, StateMachine}
import fr.acinq.bitcoin.{ByteVector32, Crypto}

import fr.acinq.eclair.transactions.RemoteFulfill
import fr.acinq.eclair.router.RouteCalculation
import fr.acinq.eclair.payment.IncomingPacket
import immortan.fsm.PaymentFailure.Failures
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Any2Some
import scala.util.Success


object IncomingPaymentProcessor {
  final val CMDTimeout = "cmd-timeout"

  final val SHUTDOWN = 0
  final val FINALIZING = 1
  final val RECEIVING = 2
  final val SENDING = 3
}

sealed trait IncomingPaymentProcessor extends StateMachine[IncomingProcessorData] with CanBeShutDown { me =>
  lazy val tuple: (FullPaymentTag, IncomingPaymentProcessor) = (fullTag, me)
  var lastAmountIn: MilliSatoshi = MilliSatoshi(0L)
  val fullTag: FullPaymentTag
}

// LOCAL RECEIVER

sealed trait IncomingProcessorData

case class IncomingRevealed(preimage: ByteVector32) extends IncomingProcessorData
case class IncomingAborted(failure: Option[FailureMessage] = None) extends IncomingProcessorData

class IncomingPaymentReceiver(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor {
  def gotAllParts(adds: ReasonableLocals): Boolean = adds.headOption.exists(lastAmountIn >= _.packet.payload.totalAmount)
  def keySendIsCorrect(preimage: ByteVector32): Boolean = Crypto.sha256(preimage) == fullTag.paymentHash

  require(fullTag.tag == PaymentTagTlv.FINAL_INCOMING)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, RECEIVING | FINALIZING) if !inFlight.in.contains(fullTag) =>
      // We have previously failed or fulfilled an incoming payment and all parts have been cleared
      becomeShutDown

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      val keySendPreimage = adds.headOption.flatMap(_.packet.payload.paymentPreimage)
      // Important: when creating new invoice we SPECIFICALLY DO NOT put a preimage into preimage storage
      // we only do that once we reveal a preimage, thus letting us know that we have already revealed it on restart
      // having PaymentStatus.SUCCEEDED in payment db is not enough because that table does not get included in backup
      lastAmountIn = adds.map(_.add.amountMsat).sum

      cm.getPaymentInfoMemo.get(fullTag.paymentHash).toOption match {
        case None => cm.getPreimageMemo.get(fullTag.paymentHash).toOption match {
          case Some(paymentPreimage) => becomeRevealed(paymentPreimage, new String, adds) // Did not ask but fulfill since we happen to have a preimage
          case None if keySendPreimage.exists(keySendIsCorrect) && gotAllParts(adds) => becomeRevealed(keySendPreimage.get, new String, adds) // Keysend and got all parts
          case None => // Do not fail just yet, this may be keysend but not all parts have reached us yet
        }

        case Some(info) if info.isIncoming && PaymentStatus.SUCCEEDED == info.status => becomeRevealed(info.preimage, info.description.queryText, adds) // Already revealed, but not finalized
        case _ if adds.exists(_.add.cltvExpiry.toLong < LNParams.blockCount.get + LNParams.cltvRejectThreshold) => becomeAborted(IncomingAborted(None), adds) // Not enough time to react if stalls
        case Some(info) if info.isIncoming && info.prExt.pr.amount.exists(lastAmountIn.>=) => becomeRevealed(info.preimage, info.description.queryText, adds) // Got enough parts to cover an amount
        case _ => // Do nothing, wait for more parts or a timeout
      }

    case (_: ReasonableLocal, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // Too many time has passed since last seen incoming payment
      become(IncomingAborted(PaymentTimeout.asSome), FINALIZING)
      cm.notifyResolvers

    case (inFlight: InFlightPayments, revealed: IncomingRevealed, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      fulfill(revealed.preimage, adds)

    case (inFlight: InFlightPayments, aborted: IncomingAborted, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      abort(aborted, adds)

    case _ =>
  }

  def fulfill(preimage: ByteVector32, adds: ReasonableLocals): Unit =
    for (local <- adds) cm.sendTo(CMD_FULFILL_HTLC(preimage, local.add), local.add.channelId)

  def abort(data1: IncomingAborted, adds: ReasonableLocals): Unit = data1.failure match {
    case None => for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(LNParams.incorrectDetails(local.add.amountMsat).asRight, local.secret, local.add), local.add.channelId)
    case Some(specificLocalFail) => for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(specificLocalFail.asRight, local.secret, local.add), local.add.channelId)
  }

  def becomeAborted(data1: IncomingAborted, adds: ReasonableLocals): Unit = {
    // Fail parts and retain a failure message to maybe re-fail using the same error
    become(data1, FINALIZING)
    abort(data1, adds)
  }

  def becomeRevealed(preimage: ByteVector32, queryText: String, adds: ReasonableLocals): Unit = cm.chanBag.db txWrap {
    // With final payment we ALREADY know a preimage (unless keySend), but also put it into storage once preimage gets revealed
    // doing so makes it transferrable as storage db gets included in backup file, unlike payments db
    cm.payBag.addSearchablePayment(queryText, fullTag.paymentHash)
    cm.payBag.updOkIncoming(lastAmountIn, fullTag.paymentHash)
    cm.payBag.setPreimage(fullTag.paymentHash, preimage)

    cm.getPaymentInfoMemo.invalidate(fullTag.paymentHash)
    cm.getPreimageMemo.invalidate(fullTag.paymentHash)
    become(IncomingRevealed(preimage), FINALIZING)
    fulfill(preimage, adds)
  }

  override def becomeShutDown: Unit = {
    cm.inProcessors -= fullTag
    become(null, SHUTDOWN)
  }
}

// TRAMPOLINE RELAYER

case class TrampolineStopping(retry: Boolean) extends IncomingProcessorData // SENDING
case class TrampolineProcessing(finalNodeId: PublicKey) extends IncomingProcessorData // SENDING
case class TrampolineRevealed(preimage: ByteVector32, senderData: Option[OutgoingPaymentSenderData] = None) extends IncomingProcessorData // SENDING | FINALIZING
case class TrampolineAborted(failure: FailureMessage) extends IncomingProcessorData // FINALIZING

class TrampolinePaymentRelayer(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor with OutgoingPaymentListener { self =>
  // Important: we may have outgoing leftovers on restart, so we always need to create a sender FSM right away, which will be firing events once leftovers get finalized
  override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = self doProcess TrampolineRevealed(fulfill.theirPreimage, data.asSome)
  override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = self doProcess data

  def collectedEnough(adds: ReasonableTrampolines): Boolean = firstOpt(adds).exists(lastAmountIn >= _.outerPayload.totalAmount)
  def firstOpt(adds: ReasonableTrampolines): Option[IncomingPacket.NodeRelayPacket] = adds.headOption.map(_.packet)
  def expiryIn(adds: ReasonableTrampolines): CltvExpiry = adds.map(_.add.cltvExpiry).min

  def relayFee(innerPayload: Onion.NodeRelayPayload, params: TrampolineOn): MilliSatoshi = {
    val linearProportional = proportionalFee(innerPayload.amountToForward, params.feeProportionalMillionths)
    trampolineFee(linearProportional.toLong, params.feeBaseMsat, params.exponent, params.logExponent)
  }

  def validateRelay(params: TrampolineOn, adds: ReasonableTrampolines, blockHeight: Long): Option[FailureMessage] = firstOpt(adds) collectFirst {
    case pkt if pkt.innerPayload.invoiceFeatures.isDefined && pkt.innerPayload.paymentSecret.isEmpty => InvalidOnionPayload(UInt64(8), 0) // We do not serve legacy recepients
    case pkt if relayFee(pkt.innerPayload, params) > lastAmountIn - pkt.innerPayload.amountToForward => TrampolineFeeInsufficient // Proposed trampoline fee is less than required
    case pkt if adds.map(_.packet.innerPayload.amountToForward).toSet.size != 1 => LNParams.incorrectDetails(pkt.add.amountMsat) // All incoming parts must have the same amount to be forwareded
    case pkt if adds.map(_.packet.outerPayload.totalAmount).toSet.size != 1 => LNParams.incorrectDetails(pkt.add.amountMsat) // All incoming parts must have the same TotalAmount value
    case pkt if expiryIn(adds) - pkt.innerPayload.outgoingCltv < params.cltvExpiryDelta => TrampolineExpiryTooSoon // Proposed delta is less than required by our node
    case _ if !adds.map(_.add.channelId).flatMap(cm.all.get).forall(Channel.isOperational) => TemporaryNodeFailure // Some channels got into error state, better stop
    case pkt if CltvExpiry(blockHeight) >= pkt.innerPayload.outgoingCltv => TrampolineExpiryTooSoon // Final recepient's CLTV expiry is below current chain height
    case pkt if pkt.innerPayload.amountToForward < params.minimumMsat => TemporaryNodeFailure // Peer wants to route less than a minimum we have told them about
  }

  def abortedWithError(failures: Failures, finalNodeId: PublicKey): TrampolineAborted = {
    val finalNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode == finalNodeId => remote.packet.failureMessage }
    val routingNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode != finalNodeId => remote.packet.failureMessage }
    val localNoRoutesFoundError = failures.collectFirst { case local: LocalFailure if local.status == PaymentFailure.NO_ROUTES_FOUND => TrampolineFeeInsufficient }
    TrampolineAborted(finalNodeFailure orElse routingNodeFailure orElse localNoRoutesFoundError getOrElse TemporaryNodeFailure)
  }

  require(fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED)
  cm.opm process CreateSenderFSM(fullTag, listener = self)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, RECEIVING | SENDING | FINALIZING) if !inFlight.allTags.contains(fullTag) =>
      // This happens AFTER we have resolved all outgoing payments and started resolving related incoming payments
      becomeShutDown

    case (inFlight: InFlightPayments, TrampolineRevealed(preimage, senderData), SENDING) =>
      // A special case after we have just received a first preimage and can become revealed
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      becomeFinalRevealed(preimage, ins)

      firstOpt(ins).foreach { packet =>
        // First, we may not have incoming HTLCs at all in pathological states
        val reserve = packet.outerPayload.totalAmount - packet.innerPayload.amountToForward
        val actualEarnings = senderData.filter(_.inFlightParts.nonEmpty).map(reserve - _.usedFee)
        // Second, used fee in sender data may be incorrect after restart, use fallback in that case
        val finalEarnings = actualEarnings getOrElse relayFee(packet.innerPayload, LNParams.trampoline)
        cm.payBag.addRelayedPreimageInfo(fullTag, preimage, packet.innerPayload.amountToForward, finalEarnings)
      }

    case (revealed: TrampolineRevealed, _: TrampolineAborted, FINALIZING) =>
      // We were winding a relay down but suddenly got a preimage
      becomeInitRevealed(revealed)

    case (revealed: TrampolineRevealed, _, RECEIVING | SENDING) =>
      // This specifically omits (TrampolineRevealed x FINALIZING) state
      // We have outgoing in-flight payments and just got a preimage
      becomeInitRevealed(revealed)

    case (_: OutgoingPaymentSenderData, TrampolineStopping(true), SENDING) =>
      // We were waiting for all outgoing parts to fail on app restart, try again
      // Note that senderFSM has removed itself on first failure, so we create it again
      become(null, RECEIVING)
      cm.notifyResolvers

    case (data: OutgoingPaymentSenderData, TrampolineStopping(false), SENDING) =>
      // We were waiting for all outgoing parts to fail on app restart, fail incoming
      become(abortedWithError(data.failures, invalidPubKey), FINALIZING)
      cm.notifyResolvers

    case (data: OutgoingPaymentSenderData, processing: TrampolineProcessing, SENDING) =>
      // This was a normal operation where we were trying to deliver a payment to recipient
      become(abortedWithError(data.failures, processing.finalNodeId), FINALIZING)
      cm.notifyResolvers

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      // We have either just got another notification or restored an app with parts present
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      val outs = inFlight.out.getOrElse(fullTag, Nil)
      lastAmountIn = ins.map(_.add.amountMsat).sum

      cm.getPreimageMemo.get(fullTag.paymentHash) match {
        case Success(preimage) => becomeFinalRevealed(preimage, ins)
        case _ if outs.isEmpty && collectedEnough(ins) => becomeSendingOrAborted(ins) // We have collected enough incoming parts: start sending
        case _ if outs.nonEmpty && collectedEnough(ins) => become(TrampolineStopping(retry = true), SENDING) // Probably a restart: fail and retry afterwards
        case _ if outs.nonEmpty => become(TrampolineStopping(retry = false), SENDING) // Have not collected enough incoming yet have outgoing: fail without retry
        case _ => // Do nothing, wait for more parts with a timeout
      }

    case (_: ReasonableTrampoline, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // Sender must not have outgoing payments in this state
      become(TrampolineAborted(PaymentTimeout), FINALIZING)
      cm.notifyResolvers

    case (inFlight: InFlightPayments, revealed: TrampolineRevealed, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      fulfill(revealed.preimage, ins)

    case (inFlight: InFlightPayments, aborted: TrampolineAborted, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      abort(aborted, ins)

    case _ =>
  }

  def fulfill(preimage: ByteVector32, adds: ReasonableTrampolines): Unit =
    for (local <- adds) cm.sendTo(CMD_FULFILL_HTLC(preimage, local.add), local.add.channelId)

  def abort(data1: TrampolineAborted, adds: ReasonableTrampolines): Unit =
    for (local <- adds) cm.sendTo(CMD_FAIL_HTLC(data1.failure.asRight, local.secret, local.add), local.add.channelId)

  def becomeSendingOrAborted(adds: ReasonableTrampolines): Unit = {
    require(adds.nonEmpty, "A set of incoming HTLCs must be non-empty here")
    val result = validateRelay(LNParams.trampoline, adds, LNParams.blockCount.get)

    result match {
      case Some(failure) =>
        val data1 = TrampolineAborted(failure)
        become(data1, FINALIZING)
        abort(data1, adds)

      case None =>
        val inner = firstOpt(adds).get.innerPayload
        val totalFeeReserve = lastAmountIn - inner.amountToForward - relayFee(inner, LNParams.trampoline)
        val extraEdges = RouteCalculation.makeExtraEdges(inner.invoiceRoutingInfo.getOrElse(Nil), inner.outgoingNodeId)
        val routerConf = LNParams.routerConf.copy(initCltvMaxDelta = expiryIn(adds) - inner.outgoingCltv - LNParams.ourRoutingOurCltvExpiryDelta)
        // It makes no sense to try to route out a payment through channels used by peer to route it in, this also includes possible unused multiple channels from same peer
        val allowedChans = cm.all -- adds.map(_.add.channelId).flatMap(cm.all.get).flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo.nodeId).flatMap(cm.allFromNode).map(_.commits.channelId)
        val send = SendMultiPart(fullTag, Left(inner.outgoingCltv), SplitInfo(inner.amountToForward, inner.amountToForward), routerConf, inner.outgoingNodeId, totalFeeReserve, allowedChans.values.toSeq)

        cm.opm process ClearFailures
        become(TrampolineProcessing(inner.outgoingNodeId), SENDING)
        // If invoice features are present then sender is asking for non-trampoline relay, it's known that recipient supports MPP
        if (inner.invoiceFeatures.isDefined) cm.opm process send.copy(assistedEdges = extraEdges, outerPaymentSecret = inner.paymentSecret.get)
        else cm.opm process send.copy(onionTlvs = OnionTlv.TrampolineOnion(adds.head.packet.nextPacket) :: Nil, outerPaymentSecret = randomBytes32)
    }
  }

  def becomeInitRevealed(revealed: TrampolineRevealed): Unit = {
    // First, unconditionally persist a preimage before doing anything else
    cm.payBag.setPreimage(fullTag.paymentHash, revealed.preimage)
    cm.getPreimageMemo.invalidate(fullTag.paymentHash)
    // Await for subsequent incoming leftovers
    become(revealed, SENDING)
    cm.notifyResolvers
  }

  def becomeFinalRevealed(preimage: ByteVector32, adds: ReasonableTrampolines): Unit = {
    // We might not have enough OR no incoming payments at all in pathological states
    become(TrampolineRevealed(preimage, senderData = None), FINALIZING)
    fulfill(preimage, adds)
  }

  override def becomeShutDown: Unit = {
    cm.opm process RemoveSenderFSM(fullTag)
    cm.inProcessors -= fullTag
    become(null, SHUTDOWN)
  }
}
