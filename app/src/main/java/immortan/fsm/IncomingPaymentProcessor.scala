package immortan.fsm

import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import immortan.fsm.IncomingPaymentProcessor._
import immortan.ChannelMaster.{OutgoingAdds, PreimageTry, ReasonableLocals, ReasonableTrampolines}
import immortan.{ChannelMaster, InFlightPayments, LNParams, PaymentInfo, PaymentStatus}
import fr.acinq.eclair.channel.{ReasonableLocal, ReasonableTrampoline}
import immortan.crypto.{CanBeShutDown, StateMachine}
import fr.acinq.eclair.transactions.RemoteFulfill
import fr.acinq.eclair.router.RouteCalculation
import fr.acinq.eclair.payment.IncomingPacket
import immortan.fsm.PaymentFailure.Failures
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Any2Some
import fr.acinq.bitcoin.ByteVector32
import scala.util.Success


object IncomingPaymentProcessor {
  final val SHUTDOWN = "incoming-processor-shutdown"
  final val FINALIZING = "incoming-processor-finalizing"
  final val RECEIVING = "incoming-processor-receiving"
  final val SENDING = "incoming-processor-sending"
  final val CMDTimeout = "cmd-timeout"
}

sealed trait IncomingPaymentProcessor extends StateMachine[IncomingProcessorData] with CanBeShutDown { me =>
  lazy val tuple: (FullPaymentTag, IncomingPaymentProcessor) = (fullTag, me)
  val fullTag: FullPaymentTag
}

// LOCAL RECEIVER

sealed trait IncomingProcessorData

case class IncomingRevealed(preimage: ByteVector32) extends IncomingProcessorData
case class IncomingAborted(failure: Option[FailureMessage] = None) extends IncomingProcessorData

class IncomingPaymentReceiver(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor {
  def askCovered(adds: ReasonableLocals, info: PaymentInfo): Boolean = info.pr.amount.exists(asked => amountIn(adds) >= asked)
  def amountIn(adds: ReasonableLocals): MilliSatoshi = adds.map(_.add.amountMsat).sum

  require(fullTag.tag == PaymentTagTlv.FINAL_INCOMING)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, RECEIVING | FINALIZING) if !inFlight.in.contains(fullTag) =>
      // We have previously failed or fulfilled an incoming payment and all parts have been cleared
      becomeShutDown

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      // Important: when creating new invoice we SPECIFICALLY DO NOT put a preimage into preimage storage
      // we only do that once we reveal a preimage, thus letting us know that we have already revealed it on restart
      // having PaymentStatus.SUCCEEDED in payment db is not enough because that table does not get included in backup
      val preimageTry: PreimageTry = cm.getPreimageMemo.get(fullTag.paymentHash)

      cm.getPaymentInfoMemo.get(fullTag.paymentHash).toOption match {
        case None if preimageTry.isSuccess => becomeRevealed(preimageTry.get, adds) // We did not ask for this, but have a preimage: fulfill anyway
        case Some(alreadyRevealed) if alreadyRevealed.isIncoming && PaymentStatus.SUCCEEDED == alreadyRevealed.status => becomeRevealed(alreadyRevealed.preimage, adds)
        case _ if adds.exists(_.add.cltvExpiry.toLong < LNParams.blockCount.get + LNParams.cltvRejectThreshold) => becomeAborted(IncomingAborted(None), adds)
        case Some(covered) if covered.isIncoming && covered.pr.amount.isDefined && askCovered(adds, covered) => becomeRevealed(covered.preimage, adds)
        case None => becomeAborted(IncomingAborted(None), adds) // We did not ask for this and there is no preimage: nothing to do but fail
        case _ => // Do nothing, wait for more parts or a timeout
      }

    case (_: ReasonableLocal, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // Too many time has passed since last seen incoming payment
      become(IncomingAborted(PaymentTimeout.toSome), FINALIZING)
      cm.stateUpdated(Nil)

    case (inFlight: InFlightPayments, revealed: IncomingRevealed, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      fulfill(revealed.preimage, adds)

    case (inFlight: InFlightPayments, aborted: IncomingAborted, FINALIZING) =>
      val adds = inFlight.in(fullTag).asInstanceOf[ReasonableLocals]
      abort(aborted, adds)

    case _ =>
  }

  // Utils

  def fulfill(preimage: ByteVector32, adds: ReasonableLocals): Unit = {
    for (local <- adds) cm.sendTo(local.fulfillCommand(preimage), local.add.channelId)
  }

  def abort(data1: IncomingAborted, adds: ReasonableLocals): Unit = data1.failure match {
    case None => for (local <- adds) cm.sendTo(local.incorrectDetailsFailCommand, local.add.channelId)
    case Some(fail) => for (local <- adds) cm.sendTo(local.failCommand(fail), local.add.channelId)
  }

  def becomeAborted(data1: IncomingAborted, adds: ReasonableLocals): Unit = {
    // Fail parts and retain a failure message to maybe re-fail using the same error
    become(data1, FINALIZING)
    abort(data1, adds)
  }

  def becomeRevealed(preimage: ByteVector32, adds: ReasonableLocals): Unit = {
    // With final payment we ALREADY know a preimage, but also put it into storage
    // doing so makes it transferrable as storage db gets included in backup file
    cm.payBag.updOkIncoming(amountIn(adds), fullTag.paymentHash)
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

object TrampolinePaymentRelayer {
  def first(adds: ReasonableTrampolines): IncomingPacket.NodeRelayPacket = adds.head.packet
  def firstOption(adds: ReasonableTrampolines): Option[IncomingPacket.NodeRelayPacket] = adds.headOption.map(_.packet)
  def relayCovered(adds: ReasonableTrampolines): Boolean = firstOption(adds).exists(amountIn(adds) >= _.outerPayload.totalAmount)

  def amountIn(adds: ReasonableTrampolines): MilliSatoshi = adds.map(_.add.amountMsat).sum
  def expiryIn(adds: ReasonableTrampolines): CltvExpiry = adds.map(_.add.cltvExpiry).min

  def relayFee(innerPayload: Onion.NodeRelayPayload, params: TrampolineOn): MilliSatoshi = {
    val linearProportional = proportionalFee(innerPayload.amountToForward, params.feeProportionalMillionths)
    trampolineFee(linearProportional.toLong, params.feeBaseMsat, params.exponent, params.logExponent)
  }

  def validateRelay(params: TrampolineOn, adds: ReasonableTrampolines, blockHeight: Long): Option[FailureMessage] =
    if (first(adds).innerPayload.invoiceFeatures.isDefined && first(adds).innerPayload.paymentSecret.isEmpty) Some(TemporaryNodeFailure) // We do not deliver to legacy recepients
    else if (relayFee(first(adds).innerPayload, params) > amountIn(adds) - first(adds).innerPayload.amountToForward) Some(TrampolineFeeInsufficient) // Proposed trampoline fee is less than required by our node
    else if (adds.map(_.packet.innerPayload.amountToForward).toSet.size != 1) Some(LNParams incorrectDetails first(adds).add.amountMsat) // All incoming parts must have the same amount to be forwareded
    else if (adds.map(_.packet.outerPayload.totalAmount).toSet.size != 1) Some(LNParams incorrectDetails first(adds).add.amountMsat) // All incoming parts must have the same TotalAmount value
    else if (expiryIn(adds) - first(adds).innerPayload.outgoingCltv < params.cltvExpiryDelta) Some(TrampolineExpiryTooSoon) // Proposed delta is less than required by our node
    else if (CltvExpiry(blockHeight) >= first(adds).innerPayload.outgoingCltv) Some(TrampolineExpiryTooSoon) // Recepient's CLTV expiry is below current chain height
    else if (first(adds).innerPayload.amountToForward < params.minimumMsat) Some(TemporaryNodeFailure)
    else None

  def abortedWithError(failures: Failures, finalNodeId: PublicKey): TrampolineAborted = {
    val finalNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode == finalNodeId => remote.packet.failureMessage }
    val routingNodeFailure = failures.collectFirst { case remote: RemoteFailure if remote.packet.originNode != finalNodeId => remote.packet.failureMessage }
    val localNoRoutesFoundError = failures.collectFirst { case local: LocalFailure if local.status == PaymentFailure.NO_ROUTES_FOUND => TrampolineFeeInsufficient }
    TrampolineAborted(finalNodeFailure orElse routingNodeFailure orElse localNoRoutesFoundError getOrElse TemporaryNodeFailure)
  }
}

case class TrampolineProcessing(finalNodeId: PublicKey) extends IncomingProcessorData // SENDING
case class TrampolineStopping(retryOnceFinalized: Boolean) extends IncomingProcessorData // SENDING
case class TrampolineRevealed(preimage: ByteVector32, senderData: Option[OutgoingPaymentSenderData] = None) extends IncomingProcessorData // SENDING | FINALIZING
case class TrampolineAborted(failure: FailureMessage) extends IncomingProcessorData // FINALIZING

class TrampolinePaymentRelayer(val fullTag: FullPaymentTag, cm: ChannelMaster) extends IncomingPaymentProcessor with OutgoingListener { self =>
  // Important: we may have outgoing leftovers on restart, so we always need to create a sender FSM right away, which will be firing events once leftovers get finalized
  override def gotFirstPreimage(data: OutgoingPaymentSenderData, fulfill: RemoteFulfill): Unit = self doProcess TrampolineRevealed(fulfill.preimage, data.toSome)
  override def wholePaymentFailed(data: OutgoingPaymentSenderData): Unit = self doProcess data

  import immortan.fsm.TrampolinePaymentRelayer._
  require(fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED)
  cm.opm process CreateSenderFSM(fullTag, listener = self)
  delayedCMDWorker.replaceWork(CMDTimeout)
  become(null, RECEIVING)

  def doProcess(msg: Any): Unit = (msg, data, state) match {
    case (inFlight: InFlightPayments, _, FINALIZING | SENDING) if !inFlight.allTags.contains(fullTag) =>
      // This happens AFTER we have resolved all outgoing payments and started resolving related incoming payments
      becomeShutDown

    case (inFlight: InFlightPayments, TrampolineRevealed(preimage, senderData), SENDING) =>
      // A special case after we have just received a first preimage and can become revealed
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      becomeFinalRevealed(preimage, ins)

      firstOption(ins).foreach { packet =>
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
      become(null, RECEIVING)
      cm.stateUpdated(Nil)

    case (data: OutgoingPaymentSenderData, TrampolineStopping(false), SENDING) =>
      // We were waiting for all outgoing parts to fail on app restart, fail incoming
      become(abortedWithError(data.failures, invalidPubKey), FINALIZING)
      cm.stateUpdated(Nil)

    case (data: OutgoingPaymentSenderData, processing: TrampolineProcessing, SENDING) =>
      // This was a normal operation where we were trying to deliver a payment to recipient
      become(abortedWithError(data.failures, processing.finalNodeId), FINALIZING)
      cm.stateUpdated(Nil)

    case (inFlight: InFlightPayments, null, RECEIVING) =>
      // We have either just seen another part or restored an app with parts
      val preimageTry: PreimageTry = cm.getPreimageMemo.get(fullTag.paymentHash)
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      val outs: OutgoingAdds = inFlight.out.getOrElse(fullTag, Nil)

      preimageTry match {
        case Success(preimage) => becomeFinalRevealed(preimage, ins)
        case _ if relayCovered(ins) && outs.isEmpty => becomeSendingOrAborted(ins)
        case _ if relayCovered(ins) && outs.nonEmpty => become(TrampolineStopping(retryOnceFinalized = true), SENDING) // App has been restarted midway, fail safely and retry
        case _ if outs.nonEmpty => become(TrampolineStopping(retryOnceFinalized = false), SENDING) // Have not collected enough yet have outgoing (this is pathologic state)
        case _ if !inFlight.allTags.contains(fullTag) => becomeShutDown // Somehow no leftovers are present at all, nothing left to do
        case _ => // Do nothing, wait for more parts with a timeout
      }

    case (_: ReasonableTrampoline, null, RECEIVING) =>
      // Just saw another related add so prolong timeout
      delayedCMDWorker.replaceWork(CMDTimeout)

    case (CMDTimeout, null, RECEIVING) =>
      // Sender must not have outgoing payments in this state
      become(TrampolineAborted(PaymentTimeout), FINALIZING)
      cm.stateUpdated(Nil)

    case (inFlight: InFlightPayments, revealed: TrampolineRevealed, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      fulfill(revealed.preimage, ins)

    case (inFlight: InFlightPayments, aborted: TrampolineAborted, FINALIZING) =>
      val ins = inFlight.in.getOrElse(fullTag, Nil).asInstanceOf[ReasonableTrampolines]
      abort(aborted, ins)

    case _ =>
  }

  def fulfill(preimage: ByteVector32, adds: ReasonableTrampolines): Unit = {
    for (local <- adds) cm.sendTo(local.fulfillCommand(preimage), local.add.channelId)
  }

  def abort(data1: TrampolineAborted, adds: ReasonableTrampolines): Unit = {
    for (local <- adds) cm.sendTo(local.failCommand(data1.failure), local.add.channelId)
  }

  def becomeSendingOrAborted(adds: ReasonableTrampolines): Unit = {
    require(adds.nonEmpty, "A set of incoming HTLCs must be non-empty")
    val result = validateRelay(LNParams.trampoline, adds, LNParams.blockCount.get)

    result match {
      case Some(failure) =>
        val data1 = TrampolineAborted(failure)
        become(data1, FINALIZING)
        abort(data1, adds)

      case None =>
        val innerPayload = first(adds).innerPayload
        val totalFeeReserve = amountIn(adds) - innerPayload.amountToForward - relayFee(innerPayload, LNParams.trampoline)
        val routerConf = LNParams.routerConf.copy(maxCltvDelta = expiryIn(adds) - innerPayload.outgoingCltv - LNParams.trampoline.cltvExpiryDelta)
        val extraEdges = RouteCalculation.makeExtraEdges(innerPayload.invoiceRoutingInfo.map(_.map(_.toList).toList).getOrElse(Nil), innerPayload.outgoingNodeId)

        // It makes no sense to try to route out a payment through channels used by peer to route it in
        // Also, if routing is undesired by user BUT peer does want to route then we exclude HCs to reduce risk
        val excludeHostedChanIds: Set[ByteVector32] = if (LNParams.isRoutingDesired) Set.empty else cm.allHosted.keySet
        val allowedChans = cm.all -- adds.map(_.add.channelId) -- excludeHostedChanIds

        val send = SendMultiPart(fullTag, routerConf, innerPayload.outgoingNodeId,
          onionTotal = innerPayload.amountToForward, actualTotal = innerPayload.amountToForward,
          totalFeeReserve, targetExpiry = innerPayload.outgoingCltv, allowedChans.values.toSeq)

        become(TrampolineProcessing(innerPayload.outgoingNodeId), SENDING)
        // If invoice features are present then sender is asking for relay to non-trampoline recipient, it is known that recipient supports MPP
        if (innerPayload.invoiceFeatures.isDefined) cm.opm process send.copy(assistedEdges = extraEdges, outerPaymentSecret = innerPayload.paymentSecret.get)
        else cm.opm process send.copy(onionTlvs = OnionTlv.TrampolineOnion(adds.head.packet.nextPacket) :: Nil, outerPaymentSecret = randomBytes32)
    }
  }

  def becomeInitRevealed(revealed: TrampolineRevealed): Unit = {
    // First, unconditionally persist a preimage before doing anything else
    cm.payBag.setPreimage(fullTag.paymentHash, revealed.preimage)
    cm.getPreimageMemo.invalidate(fullTag.paymentHash)
    // Await for subsequent incoming leftovers
    become(revealed, SENDING)
    cm.stateUpdated(Nil)
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
