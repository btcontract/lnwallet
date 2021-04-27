/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.channel

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import fr.acinq.eclair.wire._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.transactions.DirectedHtlc._
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.eclair.crypto.{Generators, ShaChain}
import immortan.{LNParams, RemoteNodeInfo, UpdateAddHtlcExt}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, OnChainFeeConf}
import fr.acinq.bitcoin.Crypto.PublicKey


case class LocalChanges(proposed: List[UpdateMessage], signed: List[UpdateMessage], acked: List[UpdateMessage] = Nil) {
  lazy val adds: List[UpdateAddHtlc] = all.collect { case add: UpdateAddHtlc => add }
  lazy val all: List[UpdateMessage] = proposed ++ signed ++ acked
}

case class RemoteChanges(proposed: List[UpdateMessage], acked: List[UpdateMessage], signed: List[UpdateMessage] = Nil) {
  lazy val adds: List[UpdateAddHtlc] = all.collect { case add: UpdateAddHtlc => add }
  lazy val all: List[UpdateMessage] = proposed ++ signed ++ acked
}

case class Changes(ourChanges: LocalChanges, theirChanges: RemoteChanges)

case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: ByteVector64, remoteSig: ByteVector64)

case class PublishableTxs(commitTx: CommitTx, htlcTxsAndSigs: List[HtlcTxAndSigs] = Nil)

case class LocalCommit(index: Long, spec: CommitmentSpec, publishableTxs: PublishableTxs)

case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: ByteVector32, remotePerCommitmentPoint: PublicKey)

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, sentAfterLocalCommitIndex: Long, reSignAsap: Boolean = false)

trait Commitments {
  def channelId: ByteVector32
  def remoteInfo: RemoteNodeInfo
  def updateOpt: Option[ChannelUpdate]

  def minSendable: MilliSatoshi
  def availableForSend: MilliSatoshi
  def availableForReceive: MilliSatoshi

  def crossSignedIncoming: Set[UpdateAddHtlcExt] // Cross-signed incoming which we can start to process
  def allOutgoing: Set[UpdateAddHtlc] // Cross-signed PLUS not yet signed payments offered by us
  def maxSendInFlight: MilliSatoshi
}

case class NormalCommits(channelVersion: ChannelVersion, remoteInfo: RemoteNodeInfo, localParams: LocalParams, remoteParams: RemoteParams,
                         channelFlags: Byte, localCommit: LocalCommit, remoteCommit: RemoteCommit, localChanges: LocalChanges, remoteChanges: RemoteChanges,
                         localNextHtlcId: Long, remoteNextHtlcId: Long, remoteNextCommitInfo: Either[WaitingForRevocation, PublicKey], commitInput: InputInfo,
                         remotePerCommitmentSecrets: ShaChain, channelId: ByteVector32, updateOpt: Option[ChannelUpdate] = None,
                         startedAt: Long = System.currentTimeMillis) extends Commitments { me =>

  val latestRemoteCommit: RemoteCommit = remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit).getOrElse(remoteCommit)

  val minSendable: MilliSatoshi = remoteParams.htlcMinimum.max(localParams.htlcMinimum)

  val allOutgoing: Set[UpdateAddHtlc] = localCommit.spec.outgoingAdds ++ remoteCommit.spec.incomingAdds ++ localChanges.adds

  val maxSendInFlight: MilliSatoshi = remoteParams.maxHtlcValueInFlightMsat.toMilliSatoshi

  val crossSignedIncoming: Set[UpdateAddHtlcExt] = for (theirAdd <- remoteCommit.spec.outgoingAdds) yield UpdateAddHtlcExt(theirAdd, remoteInfo)

  val availableForSend: MilliSatoshi = {
    // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
    val reduced = CommitmentSpec.reduce(latestRemoteCommit.spec, remoteChanges.acked, localChanges.proposed)
    val balanceNoFees = (reduced.toRemote - remoteParams.channelReserve).max(0.msat)
    if (localParams.isFunder) {
      // The funder always pays the on-chain fees, so we must subtract that from the amount we can send.
      val commitFees = commitTxFeeMsat(remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
      // the funder needs to keep a "funder fee buffer" (see explanation above)
      val funderFeeBuffer = commitTxFeeMsat(remoteParams.dustLimit, reduced.copy(feeratePerKw = reduced.feeratePerKw * 2),
        channelVersion.commitmentFormat) + htlcOutputFee(reduced.feeratePerKw * 2, channelVersion.commitmentFormat)
      val amountToReserve = commitFees.max(funderFeeBuffer)
      if (balanceNoFees - amountToReserve < offeredHtlcTrimThreshold(remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)) {
        // htlc will be trimmed
        (balanceNoFees - amountToReserve).max(0.msat)
      } else {
        // htlc will have an output in the commitment tx, so there will be additional fees.
        val commitFees1 = commitFees + htlcOutputFee(reduced.feeratePerKw, channelVersion.commitmentFormat)
        // we take the additional fees for that htlc output into account in the fee buffer at a x2 feerate increase
        val funderFeeBuffer1 = funderFeeBuffer + htlcOutputFee(reduced.feeratePerKw * 2, channelVersion.commitmentFormat)
        val amountToReserve1 = commitFees1.max(funderFeeBuffer1)
        (balanceNoFees - amountToReserve1).max(0.msat)
      }
    } else {
      // The fundee doesn't pay on-chain fees.
      balanceNoFees
    }
  }

  val availableForReceive: MilliSatoshi = {
    val reduced = CommitmentSpec.reduce(localCommit.spec, localChanges.acked, remoteChanges.proposed)
    val balanceNoFees = (reduced.toRemote - localParams.channelReserve).max(0.msat)
    if (localParams.isFunder) {
      // The fundee doesn't pay on-chain fees so we don't take those into account when receiving.
      balanceNoFees
    } else {
      // The funder always pays the on-chain fees, so we must subtract that from the amount we can receive.
      val commitFees = commitTxFeeMsat(localParams.dustLimit, reduced, channelVersion.commitmentFormat)
      // we expected the funder to keep a "funder fee buffer" (see explanation above)
      val funderFeeBuffer = commitTxFeeMsat(localParams.dustLimit, reduced.copy(feeratePerKw = reduced.feeratePerKw * 2),
        channelVersion.commitmentFormat) + htlcOutputFee(reduced.feeratePerKw * 2, channelVersion.commitmentFormat)
      val amountToReserve = commitFees.max(funderFeeBuffer)
      if (balanceNoFees - amountToReserve < receivedHtlcTrimThreshold(localParams.dustLimit, reduced, channelVersion.commitmentFormat)) {
        // htlc will be trimmed
        (balanceNoFees - amountToReserve).max(0.msat)
      } else {
        // htlc will have an output in the commitment tx, so there will be additional fees.
        val commitFees1 = commitFees + htlcOutputFee(reduced.feeratePerKw, channelVersion.commitmentFormat)
        // we take the additional fees for that htlc output into account in the fee buffer at a x2 feerate increase
        val funderFeeBuffer1 = funderFeeBuffer + htlcOutputFee(reduced.feeratePerKw * 2, channelVersion.commitmentFormat)
        val amountToReserve1 = commitFees1.max(funderFeeBuffer1)
        (balanceNoFees - amountToReserve1).max(0.msat)
      }
    }
  }

  def isMoreRecent(other: NormalCommits): Boolean = {
    val ourNextCommitSent = remoteCommit.index == other.remoteCommit.index && remoteNextCommitInfo.isLeft && other.remoteNextCommitInfo.isRight
    localCommit.index > other.localCommit.index || remoteCommit.index > other.remoteCommit.index || ourNextCommitSent
  }

  def hasNoPendingHtlcsOrFeeUpdate: Boolean = {
    val feeUpdate = (localChanges.signed ++ localChanges.acked ++ remoteChanges.signed ++ remoteChanges.acked).collectFirst { case _: UpdateFee => true }
    remoteNextCommitInfo.isRight && localCommit.spec.htlcs.isEmpty && remoteCommit.spec.htlcs.isEmpty && feeUpdate.isEmpty
  }

  def alreadyReplied(id: Long): Boolean = {
    val repliedUnsigned = localChanges.proposed.exists {
      case update: UpdateFailMalformedHtlc => id == update.id
      case update: UpdateFulfillHtlc => id == update.id
      case update: UpdateFailHtlc => id == update.id
      case _ => false
    }

    repliedUnsigned || latestRemoteCommit.spec.findOutgoingHtlcById(id).isEmpty
  }

  def addLocalProposal(proposal: UpdateMessage): NormalCommits = me.modify(_.localChanges.proposed).using(_ :+ proposal)

  def addRemoteProposal(proposal: UpdateMessage): NormalCommits = me.modify(_.remoteChanges.proposed).using(_ :+ proposal)

  def hasNoPendingHtlcs: Boolean = localCommit.spec.htlcs.isEmpty && remoteCommit.spec.htlcs.isEmpty && remoteNextCommitInfo.isRight

  def hasPendingOrProposedHtlcs: Boolean = !hasNoPendingHtlcs || localChanges.adds.nonEmpty || remoteChanges.adds.nonEmpty

  def localHasUnsignedOutgoingHtlcs: Boolean = localChanges.proposed.collectFirst { case _: UpdateAddHtlc => true }.isDefined

  def remoteHasUnsignedOutgoingHtlcs: Boolean = remoteChanges.proposed.collectFirst { case _: UpdateAddHtlc => true }.isDefined

  def remoteHasUnsignedOutgoingUpdateFee: Boolean = remoteChanges.proposed.collectFirst { case _: UpdateFee => true }.isDefined

  def localHasChanges: Boolean = remoteChanges.acked.nonEmpty || localChanges.proposed.nonEmpty

  def timedOutOutgoingHtlcs(blockheight: Long): Set[UpdateAddHtlc] =
    latestRemoteCommit.spec.incomingAdds.filter(add => blockheight >= add.cltvExpiry.toLong) ++
      remoteCommit.spec.incomingAdds.filter(add => blockheight >= add.cltvExpiry.toLong) ++
      localCommit.spec.outgoingAdds.filter(add => blockheight >= add.cltvExpiry.toLong)

  def almostTimedOutIncomingHtlcs(blockheight: Long, fulfillSafety: CltvExpiryDelta): Set[UpdateAddHtlc] =
    localCommit.spec.incomingAdds.filter(add => CltvExpiry(blockheight) >= add.cltvExpiry - fulfillSafety)

  def sendAdd(cmd: CMD_ADD_HTLC, blockHeight: Long, feeConf: OnChainFeeConf): (NormalCommits, UpdateAddHtlc) = {
    // we don't want to use too high a refund timeout, because our funds will be locked during that time if the payment is never fulfilled
    if (LNParams.maxCltvExpiryDelta.toCltvExpiry(blockHeight) < cmd.cltvExpiry) throw CMDException(InPrincipleNotSendable, cmd)
    if (CltvExpiry(blockHeight) >= cmd.cltvExpiry) throw CMDException(InPrincipleNotSendable, cmd)
    if (cmd.firstAmount < minSendable) throw CMDException(InPrincipleNotSendable, cmd)

    // we allowed mismatches between our feerates and our remote's as long as commitments didn't contain any HTLC at risk
    // we need to verify that we're not disagreeing on feerates anymore before offering new HTLCs
    // NB: there may be a pending update_fee that hasn't been applied yet that needs to be taken into account
    val localFeeratePerKw = feeConf.getCommitmentFeerate(channelVersion, None)
    val remoteFeeratePerKw = localCommit.spec.feeratePerKw +: remoteChanges.all.collect { case f: UpdateFee => f.feeratePerKw }
    val isFeeDiffTooHigh = feeConf.feerateTolerance.isFeeDiffTooHigh(channelVersion, localFeeratePerKw, _: FeeratePerKw)
    if (remoteFeeratePerKw exists isFeeDiffTooHigh) throw CMDException(new RuntimeException, cmd)

    // let's compute the current commitment *as seen by them* with this change taken into account
    val encryptedTag: TlvStream[Tlv] = TlvStream(PaymentTagTlv.EncryptedPaymentSecret(cmd.encryptedTag) :: Nil)
    val add = UpdateAddHtlc(channelId, localNextHtlcId, cmd.firstAmount, cmd.fullTag.paymentHash, cmd.cltvExpiry, cmd.packetAndSecrets.packet, encryptedTag)
    val commitments1 = addLocalProposal(add).copy(localNextHtlcId = localNextHtlcId + 1)
    // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
    val reduced = CommitmentSpec.reduce(commitments1.latestRemoteCommit.spec, commitments1.remoteChanges.acked, commitments1.localChanges.proposed)
    // the HTLC we are about to create is outgoing, but from their point of view it is incoming
    val outgoingHtlcs = reduced.htlcs.collect(incoming)

    // note that the funder pays the fee, so if sender != funder, both sides will have to afford this payment
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
    // the funder needs to keep an extra buffer to be able to handle a x2 feerate increase and an additional htlc to avoid
    // getting the channel stuck (see https://github.com/lightningnetwork/lightning-rfc/issues/728).
    val funderFeeBuffer = commitTxFeeMsat(commitments1.remoteParams.dustLimit, reduced.copy(feeratePerKw = reduced.feeratePerKw * 2),
      channelVersion.commitmentFormat) + htlcOutputFee(reduced.feeratePerKw * 2, channelVersion.commitmentFormat)
    // NB: increasing the feerate can actually remove htlcs from the commit tx (if they fall below the trim threshold)
    // which may result in a lower commit tx fee; this is why we take the max of the two.
    val missingForReceiver = reduced.toLocal - commitments1.localParams.channelReserve - { if (commitments1.localParams.isFunder) 0L.sat else fees }
    val missingForSender = reduced.toRemote - commitments1.remoteParams.channelReserve - { if (commitments1.localParams.isFunder) fees.max(funderFeeBuffer.truncateToSatoshi) else 0L.sat }
    if (missingForSender < 0L.sat) throw CMDException(new RuntimeException, cmd) else if (missingForReceiver < 0L.sat && localParams.isFunder) throw CMDException(new RuntimeException, cmd)
    if (commitments1.allOutgoing.foldLeft(0L.msat)(_ + _.amountMsat) > maxSendInFlight) throw CMDException(new RuntimeException, cmd)
    if (outgoingHtlcs.size > commitments1.remoteParams.maxAcceptedHtlcs) throw CMDException(new RuntimeException, cmd)
    (commitments1, add)
  }

  def receiveAdd(add: UpdateAddHtlc, feeConf: OnChainFeeConf): NormalCommits = {
    if (add.id != remoteNextHtlcId) throw new RuntimeException

    // we used to not enforce a strictly positive minimum, hence the max(1 msat)
    val htlcMinimum = localParams.htlcMinimum.max(1L.msat)
    if (add.amountMsat < htlcMinimum) throw new RuntimeException

    // we allowed mismatches between our feerates and our remote's as long as commitments didn't contain any HTLC at risk
    // we need to verify that we're not disagreeing on feerates anymore before offering new HTLCs
    // NB: there may be a pending update_fee that hasn't been applied yet that needs to be taken into account
    val localFeeratePerKw = feeConf.getCommitmentFeerate(channelVersion, None)
    val remoteFeeratePerKw = localCommit.spec.feeratePerKw +: remoteChanges.all.collect { case f: UpdateFee => f.feeratePerKw }
    val isFeeDiffTooHigh = feeConf.feerateTolerance.isFeeDiffTooHigh(channelVersion, localFeeratePerKw, _: FeeratePerKw)
    if (remoteFeeratePerKw exists isFeeDiffTooHigh) throw new RuntimeException

    // let's compute the current commitment *as seen by us* including this change
    val commitments1 = addRemoteProposal(add).copy(remoteNextHtlcId = remoteNextHtlcId + 1)
    val reduced = CommitmentSpec.reduce(commitments1.localCommit.spec, commitments1.localChanges.acked, commitments1.remoteChanges.proposed)
    val incomingHtlcs = reduced.htlcs.collect(incoming)

    // note that the funder pays the fee, so if sender != funder, both sides will have to afford this payment
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
    // NB: we don't enforce the funderFeeReserve (see sendAdd) because it would confuse a remote funder that doesn't have this mitigation in place
    val missingForSender = reduced.toRemote - commitments1.localParams.channelReserve - { if (commitments1.localParams.isFunder) 0L.sat else fees }
    val missingForReceiver = reduced.toLocal - commitments1.remoteParams.channelReserve - { if (commitments1.localParams.isFunder) fees else 0L.sat }
    if (missingForSender < 0L.sat) throw new RuntimeException else if (missingForReceiver < 0L.sat && localParams.isFunder) throw new RuntimeException
    // Note: we do not check whether total incoming amount exceeds our local maxHtlcValueInFlightMsat becase it is always set to channel capacity
    if (incomingHtlcs.size > commitments1.localParams.maxAcceptedHtlcs) throw new RuntimeException
    commitments1
  }

  def receiveFulfill(fulfill: UpdateFulfillHtlc): (NormalCommits, UpdateAddHtlc) = {
    val ourAdd = localCommit.spec.findOutgoingHtlcById(fulfill.id).get.add
    if (ourAdd.paymentHash != fulfill.paymentHash) throw new RuntimeException
    (addRemoteProposal(fulfill), ourAdd)
  }

  def receiveFail(fail: UpdateFailHtlc): (NormalCommits, UpdateAddHtlc) = {
    val ourAdd = localCommit.spec.findOutgoingHtlcById(fail.id).get.add
    (addRemoteProposal(fail), ourAdd)
  }

  def receiveFailMalformed(fail: UpdateFailMalformedHtlc): (NormalCommits, UpdateAddHtlc) = {
    require(0 == (fail.failureCode & FailureMessageCodecs.BADONION), "wrong bad onion code")
    val ourAdd = localCommit.spec.findOutgoingHtlcById(fail.id).get.add
    (addRemoteProposal(fail), ourAdd)
  }

  def sendFee(msg: UpdateFee): (NormalCommits, Satoshi) = {
    // Let's compute the current commitment *as seen by them* with this change taken into account
    val commitments1 = me.modify(_.localChanges.proposed).using(_.filter { case _: UpdateFee => false case _ => true } :+ msg)
    val reduced = CommitmentSpec.reduce(commitments1.remoteCommit.spec, commitments1.remoteChanges.acked, commitments1.localChanges.proposed)
    val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
    val reserve = reduced.toRemote.truncateToSatoshi - commitments1.remoteParams.channelReserve - fees
    (commitments1, reserve)
  }

  def receiveFee(fee: UpdateFee, feeConf: OnChainFeeConf): NormalCommits = {
    if (localParams.isFunder) {
      throw new RuntimeException
    } else if (fee.feeratePerKw < FeeratePerKw.MinimumFeeratePerKw) {
      throw new RuntimeException
    } else {
      val localFeeratePerKw = feeConf.feeEstimator.getFeeratePerKw(target = feeConf.feeTargets.commitmentBlockTarget)
      if (Helpers.isFeeDiffTooHigh(localFeeratePerKw, fee.feeratePerKw, feeConf.feerateTolerance) && hasPendingOrProposedHtlcs) {
        throw new RuntimeException
      } else {
        // NB: we check that the funder can afford this new fee even if spec allows to do it at next signature
        // It is easier to do it here because under certain (race) conditions spec allows a lower-than-normal fee to be paid,
        // and it would be tricky to check if the conditions are met at signing
        // (it also means that we need to check the fee of the initial commitment tx somewhere)

        // let's compute the current commitment *as seen by us* including this change
        // update_fee replace each other, so we can remove previous ones
        val commitments1 = me.modify(_.remoteChanges.proposed).using(_.filter { case _: UpdateFee => false case _ => true } :+ fee)
        val reduced = CommitmentSpec.reduce(commitments1.localCommit.spec, commitments1.localChanges.acked, commitments1.remoteChanges.proposed)

        // a node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by the counterparty, after paying the fee
        val fees = commitTxFee(commitments1.remoteParams.dustLimit, reduced, channelVersion.commitmentFormat)
        val missing = reduced.toRemote.truncateToSatoshi - commitments1.localParams.channelReserve - fees
        if (missing < 0.sat) {
          throw new RuntimeException
        } else {
          commitments1
        }
      }
    }
  }

  def sendCommit: (NormalCommits, CommitSig, RemoteCommit) = {
    remoteNextCommitInfo match {
      case Right(remoteNextPerCommitmentPoint) if localHasChanges =>
        // remote commitment will includes all local changes + remote acked changes
        val spec = CommitmentSpec.reduce(remoteCommit.spec, remoteChanges.acked, localChanges.proposed)
        val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) = NormalCommits.makeRemoteTxs(remoteInfo, channelVersion, remoteCommit.index + 1, localParams, remoteParams, commitInput, remoteNextPerCommitmentPoint, spec)
        val sig = remoteInfo.sign(remoteCommitTx, remoteInfo.fundingPublicKey(localParams.fundingKeyPath), TxOwner.Remote, channelVersion.commitmentFormat)

        val sortedHtlcTxs: Seq[TransactionWithInputInfo] = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
        val htlcSigs = sortedHtlcTxs.map(remoteInfo.sign(_, remoteInfo.htlcPoint(localParams.fundingKeyPath), remoteNextPerCommitmentPoint, TxOwner.Remote, channelVersion.commitmentFormat))

        val commitSig = CommitSig(channelId = channelId, signature = sig, htlcSignatures = htlcSigs.toList)
        val waiting = WaitingForRevocation(RemoteCommit(remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint), commitSig, localCommit.index)
        val commitments1 = copy(remoteNextCommitInfo = Left(waiting), localChanges = localChanges.copy(proposed = Nil, signed = localChanges.proposed),
          remoteChanges = remoteChanges.copy(acked = Nil, signed = remoteChanges.acked))
        (commitments1, commitSig, waiting.nextRemoteCommit)
      case _ =>
        throw new RuntimeException
    }
  }

  def receiveCommit(commit: CommitSig): (NormalCommits, RevokeAndAck) = {
    // they sent us a signature for *their* view of *our* next commit tx
    // so in terms of rev.hashes and indexes we have:
    // ourCommit.index -> our current revocation hash, which is about to become our old revocation hash
    // ourCommit.index + 1 -> our next revocation hash, used by *them* to build the sig we've just received, and which
    // is about to become our current revocation hash
    // ourCommit.index + 2 -> which is about to become our next revocation hash
    // we will reply to this sig with our old revocation hash preimage (at index) and our next revocation hash (at index + 1)
    // and will increment our index

    val spec = CommitmentSpec.reduce(localCommit.spec, localChanges.acked, remoteChanges.proposed)
    val localPerCommitmentPoint = remoteInfo.commitmentPoint(localParams.fundingKeyPath, localCommit.index + 1)
    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) = NormalCommits.makeLocalTxs(remoteInfo, channelVersion, localCommit.index + 1, localParams, remoteParams, commitInput, localPerCommitmentPoint, spec)
    val sig = remoteInfo.sign(localCommitTx, remoteInfo.fundingPublicKey(localParams.fundingKeyPath), TxOwner.Local, channelVersion.commitmentFormat)

    // no need to compute htlc sigs if commit sig doesn't check out
    val signedCommitTx = Transactions.addSigs(localCommitTx, remoteInfo.fundingPublicKey(localParams.fundingKeyPath).publicKey, remoteParams.fundingPubKey, sig, commit.signature)
    if (Transactions.checkSpendable(signedCommitTx).isFailure) throw new RuntimeException

    val sortedHtlcTxs: Seq[TransactionWithInputInfo] = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new RuntimeException

    val htlcSigs = sortedHtlcTxs.map(remoteInfo.sign(_, remoteInfo.htlcPoint(localParams.fundingKeyPath), localPerCommitmentPoint, TxOwner.Local, channelVersion.commitmentFormat))
    val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)
    // combine the sigs to make signed txes
    val htlcTxsAndSigs = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val withSigs = Transactions.addSigs(htlcTx, localSig, remoteSig, channelVersion.commitmentFormat)
        if (Transactions.checkSpendable(withSigs).isFailure) throw new RuntimeException
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)
      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        // we can't check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
        // we verify the signature from their point of view, where it is a remote tx
        val sigChecks = Transactions.checkSig(htlcTx, remoteSig, remoteHtlcPubkey, TxOwner.Remote, channelVersion.commitmentFormat)
        if (!sigChecks) throw new RuntimeException
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    // we will send our revocation preimage + our next revocation hash
    val localPerCommitmentSecret = remoteInfo.commitmentSecret(localParams.fundingKeyPath, localCommit.index)
    val localNextPerCommitmentPoint = remoteInfo.commitmentPoint(localParams.fundingKeyPath, localCommit.index + 2)
    val revocation = RevokeAndAck(channelId = channelId, perCommitmentSecret = localPerCommitmentSecret, nextPerCommitmentPoint = localNextPerCommitmentPoint)

    // update our commitment data
    val localCommit1 = LocalCommit(index = localCommit.index + 1, spec, publishableTxs = PublishableTxs(signedCommitTx, htlcTxsAndSigs))
    val ourChanges1 = localChanges.copy(acked = Nil)
    val theirChanges1 = remoteChanges.copy(proposed = Nil, acked = remoteChanges.acked ++ remoteChanges.proposed)
    val commitments1 = copy(localCommit = localCommit1, localChanges = ourChanges1, remoteChanges = theirChanges1)
    (commitments1, revocation)
  }

  def receiveRevocation(revocation: RevokeAndAck): NormalCommits = {
    // we receive a revocation because we just sent them a sig for their next commit tx
    remoteNextCommitInfo match {
      case Left(_) if revocation.perCommitmentSecret.publicKey != remoteCommit.remotePerCommitmentPoint =>
        throw new RuntimeException
      case Left(d1: WaitingForRevocation) =>
        copy(
          localChanges = localChanges.copy(signed = Nil, acked = localChanges.acked ++ localChanges.signed),
          remoteChanges = remoteChanges.copy(signed = Nil),
          remoteCommit = d1.nextRemoteCommit,
          remoteNextCommitInfo = Right(revocation.nextPerCommitmentPoint),
          remotePerCommitmentSecrets = remotePerCommitmentSecrets.addHash(revocation.perCommitmentSecret.value, 0xFFFFFFFFFFFFL - remoteCommit.index))
      case Right(_) =>
        throw new RuntimeException
    }
  }
}

object NormalCommits {
  type HtlcTimeoutTxSeq = Seq[HtlcTimeoutTx]

  type HtlcSuccessTxSeq = Seq[HtlcSuccessTx]

  def makeLocalTxs(remoteInfo: RemoteNodeInfo, channelVersion: ChannelVersion, commitTxNumber: Long, localParams: LocalParams, remoteParams: RemoteParams,
                   commitmentInput: InputInfo, localPerCommitmentPoint: PublicKey, spec: CommitmentSpec): (CommitTx, HtlcTimeoutTxSeq, HtlcSuccessTxSeq) = {
    val localFundingPubkey = remoteInfo.fundingPublicKey(localParams.fundingKeyPath).publicKey
    val localDelayedPaymentPubkey = Generators.derivePubKey(remoteInfo.delayedPaymentPoint(localParams.fundingKeyPath).publicKey, localPerCommitmentPoint)
    val localHtlcPubkey = Generators.derivePubKey(remoteInfo.htlcPoint(localParams.fundingKeyPath).publicKey, localPerCommitmentPoint)
    val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, localPerCommitmentPoint)
    val localRevocationPubkey = Generators.revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)
    val outputs = makeCommitTxOutputs(localParams.isFunder, localParams.dustLimit, localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPaymentPubkey,
      remotePaymentPubkey = localParams.walletStaticPaymentBasepoint, localHtlcPubkey, remoteHtlcPubkey, localFundingPubkey, remoteParams.fundingPubKey, spec, channelVersion.commitmentFormat)
    val commitTx = Transactions.makeCommitTx(commitmentInput, commitTxNumber, localParams.walletStaticPaymentBasepoint, remoteParams.paymentBasepoint, localParams.isFunder, outputs)
    val (htlcTimeoutTxs, htlcSuccessTxs) = Transactions.makeHtlcTxs(commitTx.tx, localParams.dustLimit, localRevocationPubkey, remoteParams.toSelfDelay,
      localDelayedPaymentPubkey, spec.feeratePerKw, outputs, channelVersion.commitmentFormat)
    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }

  def makeRemoteTxs(remoteInfo: RemoteNodeInfo, channelVersion: ChannelVersion, commitTxNumber: Long, localParams: LocalParams, remoteParams: RemoteParams,
                    commitmentInput: InputInfo, remotePerCommitmentPoint: PublicKey, spec: CommitmentSpec): (CommitTx, HtlcTimeoutTxSeq, HtlcSuccessTxSeq) = {
    val localFundingPubkey = remoteInfo.fundingPublicKey(localParams.fundingKeyPath).publicKey
    val localHtlcPubkey = Generators.derivePubKey(remoteInfo.htlcPoint(localParams.fundingKeyPath).publicKey, remotePerCommitmentPoint)
    val remoteDelayedPaymentPubkey = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
    val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, remotePerCommitmentPoint)
    val remoteRevocationPubkey = Generators.revocationPubKey(remoteInfo.revocationPoint(localParams.fundingKeyPath).publicKey, remotePerCommitmentPoint)
    val outputs = makeCommitTxOutputs(!localParams.isFunder, remoteParams.dustLimit, remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPaymentPubkey,
      localParams.walletStaticPaymentBasepoint, remoteHtlcPubkey, localHtlcPubkey, remoteParams.fundingPubKey, localFundingPubkey, spec, channelVersion.commitmentFormat)
    val commitTx = Transactions.makeCommitTx(commitmentInput, commitTxNumber, remoteParams.paymentBasepoint, localParams.walletStaticPaymentBasepoint, !localParams.isFunder, outputs)
    val (htlcTimeoutTxs, htlcSuccessTxs) = Transactions.makeHtlcTxs(commitTx.tx, remoteParams.dustLimit, remoteRevocationPubkey, localParams.toSelfDelay,
      remoteDelayedPaymentPubkey, spec.feeratePerKw, outputs, channelVersion.commitmentFormat)
    (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
  }
}
