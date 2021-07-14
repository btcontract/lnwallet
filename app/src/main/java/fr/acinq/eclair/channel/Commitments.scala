package fr.acinq.eclair.channel

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import fr.acinq.eclair.wire._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.transactions.DirectedHtlc._
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.eclair.crypto.{Generators, ShaChain}
import immortan.crypto.Tools.{Any2Some, newFeerate, none}
import immortan.{LNParams, RemoteNodeInfo, UpdateAddHtlcExt}
import fr.acinq.eclair.channel.Helpers.HashToPreimage
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.utils.Rx


case class LocalChanges(proposed: List[UpdateMessage], signed: List[UpdateMessage], acked: List[UpdateMessage] = Nil) {
  lazy val adds: List[UpdateAddHtlc] = all.collect { case add: UpdateAddHtlc => add }
  lazy val all: List[UpdateMessage] = proposed ++ signed ++ acked
}

case class RemoteChanges(proposed: List[UpdateMessage], acked: List[UpdateMessage], signed: List[UpdateMessage] = Nil) {
  lazy val adds: List[UpdateAddHtlc] = all.collect { case add: UpdateAddHtlc => add }
  lazy val all: List[UpdateMessage] = proposed ++ signed ++ acked
}

case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: ByteVector64, remoteSig: ByteVector64)

case class PublishableTxs(commitTx: CommitTx, htlcTxsAndSigs: List[HtlcTxAndSigs] = Nil)

case class LocalCommit(index: Long, spec: CommitmentSpec, publishableTxs: PublishableTxs)

case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: ByteVector32, remotePerCommitmentPoint: PublicKey)

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, sentAfterLocalCommitIndex: Long)

trait Commitments {
  def channelId: ByteVector32
  def remoteInfo: RemoteNodeInfo
  def updateOpt: Option[ChannelUpdate]
  def startedAt: Long

  def minSendable: MilliSatoshi
  def maxSendInFlight: MilliSatoshi

  def availableForSend: MilliSatoshi
  def availableForReceive: MilliSatoshi

  def allOutgoing: Set[UpdateAddHtlc] // Cross-signed PLUS not yet signed payments offered by us
  def crossSignedIncoming: Set[UpdateAddHtlcExt] // Cross-signed incoming payments offered by them
  def revealedFulfills: Set[LocalFulfill] // Incoming payments for which we have releaved a preimge

  def getPendingFulfills(preimages: HashToPreimage = Map.empty): Set[LocalFulfill] = for {
    // Find still present cross-signed incoming payments for which we have revealed a preimage
    UpdateAddHtlcExt(theirAdd, _) <- crossSignedIncoming
    ourPreimage <- preimages.get(theirAdd.paymentHash)
  } yield LocalFulfill(theirAdd, ourPreimage)
}

case class NormalCommits(channelFlags: Byte, channelId: ByteVector32, channelVersion: ChannelVersion, remoteNextCommitInfo: Either[WaitingForRevocation, PublicKey],
                         remotePerCommitmentSecrets: ShaChain, updateOpt: Option[ChannelUpdate], postCloseOutgoingResolvedIds: Set[Long], remoteInfo: RemoteNodeInfo, localParams: LocalParams,
                         remoteParams: RemoteParams, localCommit: LocalCommit, remoteCommit: RemoteCommit, localChanges: LocalChanges, remoteChanges: RemoteChanges, localNextHtlcId: Long,
                         remoteNextHtlcId: Long, commitInput: InputInfo, startedAt: Long = System.currentTimeMillis) extends Commitments { me =>

  val minSendable: MilliSatoshi = remoteParams.htlcMinimum.max(localParams.htlcMinimum)

  val maxSendInFlight: MilliSatoshi = remoteParams.maxHtlcValueInFlightMsat.toMilliSatoshi

  val latestReducedRemoteSpec: CommitmentSpec = {
    val latestRemoteCommit = remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit).getOrElse(remoteCommit)
    CommitmentSpec.reduce(latestRemoteCommit.spec, remoteChanges.acked, localChanges.proposed)
  }

  val allOutgoing: Set[UpdateAddHtlc] = {
    val allOutgoingAdds = localCommit.spec.outgoingAdds ++ localChanges.adds
    allOutgoingAdds.filterNot(add => postCloseOutgoingResolvedIds contains add.id)
  }

  val crossSignedIncoming: Set[UpdateAddHtlcExt] = for (theirAdd <- remoteCommit.spec.outgoingAdds) yield UpdateAddHtlcExt(theirAdd, remoteInfo)

  val revealedFulfills: Set[LocalFulfill] = getPendingFulfills(Helpers extractRevealedPreimages localChanges.all)

  val availableForSend: MilliSatoshi = {
    // We need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
    val balanceNoFees = latestReducedRemoteSpec.toRemote - remoteParams.channelReserve

    if (localParams.isFunder) {
      val feerate = latestReducedRemoteSpec.copy(feeratePerKw = latestReducedRemoteSpec.feeratePerKw)
      val feeBuffer = htlcOutputFee(latestReducedRemoteSpec.feeratePerKw, channelVersion.commitmentFormat)
      val commitFees = commitTxFeeMsat(remoteParams.dustLimit, latestReducedRemoteSpec, channelVersion.commitmentFormat)
      val trimThreshold = offeredHtlcTrimThreshold(remoteParams.dustLimit, latestReducedRemoteSpec, channelVersion.commitmentFormat)
      val funderFeeBuffer = commitTxFeeMsat(remoteParams.dustLimit, feerate, channelVersion.commitmentFormat) + feeBuffer
      val amountToReserve = commitFees.max(funderFeeBuffer)

      if (balanceNoFees - amountToReserve < trimThreshold) {
        // Htlc will be trimmed so there will be no chain fees
        balanceNoFees - amountToReserve
      } else {
        // Htlc will have an output in the commitment tx, so there will be additional fees.
        val commitFees1 = commitFees + htlcOutputFee(latestReducedRemoteSpec.feeratePerKw, channelVersion.commitmentFormat)
        balanceNoFees - commitFees1.max(funderFeeBuffer + feeBuffer)
      }
    } else {
      balanceNoFees
    }
  }

  val availableForReceive: MilliSatoshi = {
    val reduced = CommitmentSpec.reduce(localCommit.spec, localChanges.acked, remoteChanges.proposed)
    val balanceNoFees = reduced.toRemote - localParams.channelReserve

    if (localParams.isFunder) {
      // The fundee doesn't pay on-chain fees
      balanceNoFees
    } else {
      val feerate = reduced.copy(feeratePerKw = reduced.feeratePerKw)
      val feeBuffer = htlcOutputFee(reduced.feeratePerKw, channelVersion.commitmentFormat)
      val commitFees = commitTxFeeMsat(localParams.dustLimit, reduced, channelVersion.commitmentFormat)
      val trimThreshold = receivedHtlcTrimThreshold(localParams.dustLimit, reduced, channelVersion.commitmentFormat)
      val funderFeeBuffer = commitTxFeeMsat(localParams.dustLimit, feerate, channelVersion.commitmentFormat) + feeBuffer
      val amountToReserve = commitFees.max(funderFeeBuffer)

      if (balanceNoFees - amountToReserve < trimThreshold) {
        // Htlc will be trimmed so there will be no chain fees
        balanceNoFees - amountToReserve
      } else {
        // Htlc will have an output in the commitment tx, so there will be additional fees.
        val commitFees1 = commitFees + htlcOutputFee(reduced.feeratePerKw, channelVersion.commitmentFormat)
        balanceNoFees - commitFees1.max(funderFeeBuffer + feeBuffer)
      }
    }
  }

  def isMoreRecent(other: NormalCommits): Boolean = {
    val ourNextCommitSent = remoteCommit.index == other.remoteCommit.index && remoteNextCommitInfo.isLeft && other.remoteNextCommitInfo.isRight
    localCommit.index > other.localCommit.index || remoteCommit.index > other.remoteCommit.index || ourNextCommitSent
  }

  def hasPendingHtlcsOrFeeUpdate: Boolean = {
    val changes = localChanges.signed ++ localChanges.acked ++ remoteChanges.signed ++ remoteChanges.acked
    val pendingHtlcs = localCommit.spec.htlcs.nonEmpty || remoteCommit.spec.htlcs.nonEmpty || remoteNextCommitInfo.isLeft
    pendingHtlcs || changes.exists { case _: UpdateFee => true case _ => false }
  }

  def addLocalProposal(proposal: UpdateMessage): NormalCommits = me.modify(_.localChanges.proposed).using(_ :+ proposal)

  def addRemoteProposal(proposal: UpdateMessage): NormalCommits = me.modify(_.remoteChanges.proposed).using(_ :+ proposal)

  def localHasUnsignedOutgoingHtlcs: Boolean = localChanges.proposed.exists { case _: UpdateAddHtlc => true case _ => false }

  def remoteHasUnsignedOutgoingHtlcs: Boolean = remoteChanges.proposed.exists { case _: UpdateAddHtlc => true case _ => false }

  def remoteHasUnsignedOutgoingUpdateFee: Boolean = remoteChanges.proposed.exists { case _: UpdateFee => true case _ => false }

  def localHasChanges: Boolean = remoteChanges.acked.nonEmpty || localChanges.proposed.nonEmpty

  type UpdatedNCAndAdd = (NormalCommits, UpdateAddHtlc)
  def sendAdd(cmd: CMD_ADD_HTLC, blockHeight: Long): Either[LocalReject, UpdatedNCAndAdd] = {
    if (LNParams.maxCltvExpiryDelta.toCltvExpiry(blockHeight) < cmd.cltvExpiry) return InPrincipleNotSendable(cmd.incompleteAdd).asLeft
    if (CltvExpiry(blockHeight) >= cmd.cltvExpiry) return InPrincipleNotSendable(cmd.incompleteAdd).asLeft
    if (cmd.firstAmount < minSendable) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft

    val completeAdd = cmd.incompleteAdd.copy(channelId = channelId, id = localNextHtlcId)
    val commitments1 = addLocalProposal(completeAdd).copy(localNextHtlcId = localNextHtlcId + 1)
    val totalOutgoingHtlcs = commitments1.latestReducedRemoteSpec.htlcs.collect(incoming).size

    val feeBuffer = htlcOutputFee(commitments1.latestReducedRemoteSpec.feeratePerKw, channelVersion.commitmentFormat)
    val feerate = commitments1.latestReducedRemoteSpec.copy(feeratePerKw = commitments1.latestReducedRemoteSpec.feeratePerKw)
    val funderFeeBuffer = commitTxFeeMsat(commitments1.remoteParams.dustLimit, feerate, channelVersion.commitmentFormat) + feeBuffer

    val receiverWithReserve = commitments1.latestReducedRemoteSpec.toLocal - commitments1.localParams.channelReserve
    val senderWithReserve = commitments1.latestReducedRemoteSpec.toRemote - commitments1.remoteParams.channelReserve
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, commitments1.latestReducedRemoteSpec, channelVersion.commitmentFormat)

    val missingForReceiver = if (commitments1.localParams.isFunder) receiverWithReserve else receiverWithReserve - fees
    val missingForSender = if (commitments1.localParams.isFunder) senderWithReserve - fees.max(funderFeeBuffer.truncateToSatoshi) else senderWithReserve

    if (missingForSender < 0L.sat) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (missingForReceiver < 0L.sat && !localParams.isFunder) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (commitments1.allOutgoing.foldLeft(0L.msat)(_ + _.amountMsat) > maxSendInFlight) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft
    if (totalOutgoingHtlcs > commitments1.remoteParams.maxAcceptedHtlcs) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft // This is from spec and prevents remote force-close
    if (totalOutgoingHtlcs > commitments1.localParams.maxAcceptedHtlcs) return ChannelNotAbleToSend(cmd.incompleteAdd).asLeft // This is needed for peer backup and routing to safely work
    Right(commitments1, completeAdd)
  }

  def sendFulfill(cmd: CMD_FULFILL_HTLC): (NormalCommits, UpdateFulfillHtlc) = {
    val msg = UpdateFulfillHtlc(channelId, cmd.theirAdd.id, cmd.preimage)
    (addLocalProposal(msg), msg)
  }

  def receiveAdd(add: UpdateAddHtlc): NormalCommits = {
    if (localParams.htlcMinimum.max(1L.msat) > add.amountMsat) throw ChannelTransitionFail(channelId)
    if (add.id != remoteNextHtlcId) throw ChannelTransitionFail(channelId)

    // Let's compute the current commitment *as seen by us* including this change
    val commitments1 = addRemoteProposal(add).copy(remoteNextHtlcId = remoteNextHtlcId + 1)
    val reduced = CommitmentSpec.reduce(commitments1.localCommit.spec, commitments1.localChanges.acked, commitments1.remoteChanges.proposed)

    val senderWithReserve = reduced.toRemote - commitments1.localParams.channelReserve
    val receiverWithReserve = reduced.toLocal - commitments1.remoteParams.channelReserve
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
    val missingForSender = if (commitments1.localParams.isFunder) senderWithReserve else senderWithReserve - fees
    val missingForReceiver = if (commitments1.localParams.isFunder) receiverWithReserve - fees else receiverWithReserve

    if (missingForSender < 0L.sat) throw ChannelTransitionFail(channelId) else if (missingForReceiver < 0L.sat && localParams.isFunder) throw ChannelTransitionFail(channelId)
    // We do not check whether total incoming payments amount exceeds our local maxHtlcValueInFlightMsat becase it is always set to a whole channel capacity
    if (reduced.htlcs.collect(incoming).size > commitments1.localParams.maxAcceptedHtlcs) throw ChannelTransitionFail(channelId)
    commitments1
  }

  def receiveFulfill(fulfill: UpdateFulfillHtlc): (NormalCommits, UpdateAddHtlc) = localCommit.spec.findOutgoingHtlcById(fulfill.id) match {
    case Some(ourAdd) if ourAdd.add.paymentHash != fulfill.paymentHash => throw ChannelTransitionFail(channelId)
    case Some(ourAdd) => (addRemoteProposal(fulfill), ourAdd.add)
    case None => throw ChannelTransitionFail(channelId)
  }

  def receiveFail(fail: UpdateFailHtlc): NormalCommits = localCommit.spec.findOutgoingHtlcById(fail.id) match {
    case None => throw ChannelTransitionFail(channelId)
    case _ => addRemoteProposal(fail)
  }

  def receiveFailMalformed(fail: UpdateFailMalformedHtlc): NormalCommits = localCommit.spec.findOutgoingHtlcById(fail.id) match {
    case _ if fail.failureCode.&(FailureMessageCodecs.BADONION) != 0 => throw ChannelTransitionFail(channelId)
    case None => throw ChannelTransitionFail(channelId)
    case _ => addRemoteProposal(fail)
  }

  def sendFee(rate: FeeratePerKw): (NormalCommits, Satoshi, UpdateFee) = {
    val msg: UpdateFee = UpdateFee(channelId = channelId, feeratePerKw = rate)
    // Let's compute the current commitment *as seen by them* with this change taken into account
    val commitments1 = me.modify(_.localChanges.proposed).using(changes => changes.filter { case _: UpdateFee => false case _ => true } :+ msg)
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, commitments1.latestReducedRemoteSpec, channelVersion.commitmentFormat)
    val reserve = commitments1.latestReducedRemoteSpec.toRemote.truncateToSatoshi - commitments1.remoteParams.channelReserve - fees
    (commitments1, reserve, msg)
  }

  def receiveFee(fee: UpdateFee): NormalCommits = {
    if (localParams.isFunder) throw ChannelTransitionFail(channelId)
    if (fee.feeratePerKw < FeeratePerKw.MinimumFeeratePerKw) throw ChannelTransitionFail(channelId)
    val commitments1 = me.modify(_.remoteChanges.proposed).using(changes => changes.filter { case _: UpdateFee => false case _ => true } :+ fee)
    val reduced = CommitmentSpec.reduce(commitments1.localCommit.spec, commitments1.localChanges.acked, commitments1.remoteChanges.proposed)

    val threshold = Transactions.offeredHtlcTrimThreshold(remoteParams.dustLimit, localCommit.spec, channelVersion.commitmentFormat)
    val largeRoutedExist = allOutgoing.exists(ourAdd => ourAdd.amountMsat > threshold * LNParams.minForceClosableOutgoingHtlcAmountToFeeRatio && ourAdd.fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED)
    val dangerousState = largeRoutedExist && newFeerate(LNParams.feeRates.info, reduced, LNParams.shouldForceClosePaymentFeerateDiff).isDefined && fee.feeratePerKw < commitments1.localCommit.spec.feeratePerKw

    if (dangerousState) {
      // We force feerate update and block this thread while it's being executed, will have an updated info once done
      Rx.retry(Rx.ioQueue.map(_ => LNParams.feeRates.reloadData), Rx.incSec, 1 to 3).toBlocking.subscribe(LNParams.feeRates.updateInfo, none)
      // We have seen a suspiciously lower feerate update from peer, then force-checked current network feerates and they are NOT THAT low
      val stillDangerousState = newFeerate(LNParams.feeRates.info, reduced, LNParams.shouldForceClosePaymentFeerateDiff).isDefined
      // It is too dangerous to have outgoing routed HTLCs with such a low feerate since they may not get confirmed in time
      if (stillDangerousState) throw ChannelTransitionFail(channelId)
    }

    val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
    val missing = reduced.toRemote.truncateToSatoshi - commitments1.localParams.channelReserve - fees
    if (missing < 0L.sat) throw ChannelTransitionFail(channelId)
    commitments1
  }

  def sendCommit: (NormalCommits, CommitSig, RemoteCommit) =
    remoteNextCommitInfo match {
      case Right(remoteNextPoint) if localHasChanges =>
        val localChanges1 = localChanges.copy(proposed = Nil, signed = localChanges.proposed)
        val remoteChanges1 = remoteChanges.copy(acked = Nil, signed = remoteChanges.acked)

        val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
          NormalCommits.makeRemoteTxs(channelVersion, remoteCommit.index + 1, localParams,
            remoteParams, commitInput, remoteNextPoint, latestReducedRemoteSpec)

        val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
        val htlcSigs = for (htlc <- sortedHtlcTxs) yield localParams.keys.sign(htlc, localParams.keys.htlcKey.privateKey, remoteNextPoint, TxOwner.Remote, channelVersion.commitmentFormat)
        val commitSig = CommitSig(channelId, Transactions.sign(remoteCommitTx, localParams.keys.fundingKey.privateKey, TxOwner.Remote, channelVersion.commitmentFormat), htlcSigs.toList)
        val waiting = WaitingForRevocation(RemoteCommit(remoteCommit.index + 1, latestReducedRemoteSpec, remoteCommitTx.tx.txid, remoteNextPoint), commitSig, localCommit.index)
        val commitments1 = copy(remoteNextCommitInfo = Left(waiting), localChanges = localChanges1, remoteChanges = remoteChanges1)
        (commitments1, commitSig, waiting.nextRemoteCommit)

      case _ =>
        throw ChannelTransitionFail(channelId)
    }

  def receiveCommit(commit: CommitSig): (NormalCommits, RevokeAndAck) = {
    val localPerCommitmentPoint = localParams.keys.commitmentPoint(localCommit.index + 1)
    val spec = CommitmentSpec.reduce(localCommit.spec, localChanges.acked, remoteChanges.proposed)

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      NormalCommits.makeLocalTxs(channelVersion, localCommit.index + 1,
        localParams, remoteParams, commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val localCommitTxSig = Transactions.sign(localCommitTx, localParams.keys.fundingKey.privateKey, TxOwner.Local, channelVersion.commitmentFormat)
    val signedCommitTx = Transactions.addSigs(localCommitTx, localParams.keys.fundingKey.publicKey, remoteParams.fundingPubKey, localCommitTxSig, commit.signature)
    val htlcSigs = for (htlc <- sortedHtlcTxs) yield localParams.keys.sign(htlc, localParams.keys.htlcKey.privateKey, localPerCommitmentPoint, TxOwner.Local, channelVersion.commitmentFormat)
    val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    if (Transactions.checkSpendable(signedCommitTx).isFailure) throw ChannelTransitionFail(channelId)
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw ChannelTransitionFail(channelId)

    val htlcTxsAndSigs = combined.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val withSigs = Transactions.addSigs(htlcTx, localSig, remoteSig, channelVersion.commitmentFormat)
        if (Transactions.checkSpendable(withSigs).isFailure) throw ChannelTransitionFail(channelId)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        // We can't check that htlc-success tx are spendable because we need the payment preimage
        // Thus we only check the remote sig, we verify the signature from their point of view, where it is a remote tx
        val sigChecks = Transactions.checkSig(htlcTx, remoteSig, remoteHtlcPubkey, TxOwner.Remote, channelVersion.commitmentFormat)
        if (!sigChecks) throw ChannelTransitionFail(channelId)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    val publishableTxs = PublishableTxs(signedCommitTx, htlcTxsAndSigs)
    val localPerCommitmentSecret = localParams.keys.commitmentSecret(localCommit.index)
    val localNextPerCommitmentPoint = localParams.keys.commitmentPoint(localCommit.index + 2)
    val localCommit1 = LocalCommit(index = localCommit.index + 1, spec, publishableTxs = publishableTxs)
    val theirChanges1 = remoteChanges.copy(proposed = Nil, acked = remoteChanges.acked ++ remoteChanges.proposed)
    val revocation = RevokeAndAck(channelId, perCommitmentSecret = localPerCommitmentSecret, nextPerCommitmentPoint = localNextPerCommitmentPoint)
    val commitments1 = copy(localCommit = localCommit1, localChanges = localChanges.copy(acked = Nil), remoteChanges = theirChanges1)
    (commitments1, revocation)
  }

  def receiveRevocation(revocation: RevokeAndAck): NormalCommits = remoteNextCommitInfo match {
    case Left(d1: WaitingForRevocation) if revocation.perCommitmentSecret.publicKey == remoteCommit.remotePerCommitmentPoint =>
      val remotePerCommitmentSecrets1 = remotePerCommitmentSecrets.addHash(revocation.perCommitmentSecret.value, 0xFFFFFFFFFFFFL - remoteCommit.index)
      copy(localChanges = localChanges.copy(signed = Nil, acked = localChanges.acked ++ localChanges.signed), remoteChanges = remoteChanges.copy(signed = Nil),
        remoteCommit = d1.nextRemoteCommit, remoteNextCommitInfo = Right(revocation.nextPerCommitmentPoint), remotePerCommitmentSecrets = remotePerCommitmentSecrets1)

    case _ =>
      throw ChannelTransitionFail(channelId)
  }
}

object NormalCommits {
  type HtlcTimeoutTxSeq = Seq[HtlcTimeoutTx]
  type HtlcSuccessTxSeq = Seq[HtlcSuccessTx]

  def makeLocalTxs(channelVersion: ChannelVersion, commitTxNumber: Long, localParams: LocalParams,
                   remoteParams: RemoteParams, commitmentInput: InputInfo, localPerCommitmentPoint: PublicKey,
                   spec: CommitmentSpec): (CommitTx, HtlcTimeoutTxSeq, HtlcSuccessTxSeq) = {

    val localDelayedPayment = Generators.derivePubKey(localParams.keys.delayedPaymentKey.publicKey, localPerCommitmentPoint)
    val localRevocation = Generators.revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)
    val localHtlc = Generators.derivePubKey(localParams.keys.htlcKey.publicKey, localPerCommitmentPoint)
    val remoteHtlc = Generators.derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)

    val outputs: CommitmentOutputs =
      makeCommitTxOutputs(localParams.isFunder, localParams.dustLimit, localRevocation, remoteParams.toSelfDelay,
        localDelayedPayment, remoteParams.paymentBasepoint, localHtlc, remoteHtlc, localParams.keys.fundingKey.publicKey,
        remoteParams.fundingPubKey, spec, channelVersion.commitmentFormat)

    val commitTx = makeCommitTx(commitmentInput, commitTxNumber, localParams.walletStaticPaymentBasepoint, remoteParams.paymentBasepoint, localParams.isFunder, outputs)
    val (timeouts, successes) = makeHtlcTxs(commitTx.tx, localParams.dustLimit, localRevocation, remoteParams.toSelfDelay, localDelayedPayment, spec.feeratePerKw, outputs, channelVersion.commitmentFormat)
    (commitTx, timeouts, successes)
  }

  def makeRemoteTxs(channelVersion: ChannelVersion, commitTxNumber: Long, localParams: LocalParams,
                    remoteParams: RemoteParams, commitmentInput: InputInfo, remotePerCommitmentPoint: PublicKey,
                    spec: CommitmentSpec): (CommitTx, HtlcTimeoutTxSeq, HtlcSuccessTxSeq) = {

    val localHtlc = Generators.derivePubKey(localParams.keys.htlcKey.publicKey, remotePerCommitmentPoint)
    val remoteDelayedPayment = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
    val remoteRevocation = Generators.revocationPubKey(localParams.keys.revocationKey.publicKey, remotePerCommitmentPoint)
    val remoteHtlc = Generators.derivePubKey(remoteParams.htlcBasepoint, remotePerCommitmentPoint)

    val outputs: CommitmentOutputs =
      makeCommitTxOutputs(!localParams.isFunder, remoteParams.dustLimit, remoteRevocation, localParams.toSelfDelay,
        remoteDelayedPayment, localParams.walletStaticPaymentBasepoint, remoteHtlc, localHtlc, remoteParams.fundingPubKey,
        localParams.keys.fundingKey.publicKey, spec, channelVersion.commitmentFormat)

    val commitTx = makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint, localParams.walletStaticPaymentBasepoint, !localParams.isFunder, outputs)
    val (timeouts, successes) = makeHtlcTxs(commitTx.tx, remoteParams.dustLimit, remoteRevocation, localParams.toSelfDelay, remoteDelayedPayment, spec.feeratePerKw, outputs, channelVersion.commitmentFormat)
    (commitTx, timeouts, successes)
  }
}
