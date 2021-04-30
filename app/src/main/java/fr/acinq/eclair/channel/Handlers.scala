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
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.{ChannelNormal, LNParams}
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Transaction}
import fr.acinq.eclair.blockchain.{PublishAsap, WatchConfirmed, WatchSpent}
import fr.acinq.eclair.blockchain.fee.OnChainFeeConf
import fr.acinq.eclair.channel.Helpers.Closing
import scala.collection.immutable.Queue


trait Handlers { me: ChannelNormal =>
  def doPublish(closingTx: Transaction): Unit = {
    val replyEvent: BitcoinEvent = BITCOIN_TX_CONFIRMED(closingTx)
    chainWallet.watcher ! WatchConfirmed(receiver, closingTx, LNParams.minDepthBlocks, replyEvent)
    chainWallet.watcher ! PublishAsap(closingTx)
  }

  def publishIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(PublishAsap).foreach(event => chainWallet.watcher ! event)

  def watchConfirmedIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(BITCOIN_TX_CONFIRMED).foreach { replyEvent =>
      chainWallet.watcher ! WatchConfirmed(receiver, replyEvent.tx, LNParams.minDepthBlocks, replyEvent)
    }

  def watchSpentIfNeeded(parentTx: Transaction, txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(_.txIn.head.outPoint.index.toInt).foreach { outPointIndex =>
      chainWallet.watcher ! WatchSpent(receiver, parentTx, outPointIndex, BITCOIN_OUTPUT_SPENT)
    }

  def doPublish(lcp: LocalCommitPublished): Unit = {
    publishIfNeeded(List(lcp.commitTx) ++ lcp.claimMainDelayedOutputTx ++ lcp.htlcSuccessTxs ++ lcp.htlcTimeoutTxs ++ lcp.claimHtlcDelayedTxs, lcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(lcp.commitTx) ++ lcp.claimMainDelayedOutputTx ++ lcp.claimHtlcDelayedTxs, lcp.irrevocablySpent)
    watchSpentIfNeeded(lcp.commitTx, lcp.htlcSuccessTxs ++ lcp.htlcTimeoutTxs, lcp.irrevocablySpent)
  }

  def doPublish(rcp: RemoteCommitPublished): Unit = {
    publishIfNeeded(rcp.claimMainOutputTx ++ rcp.claimHtlcSuccessTxs ++ rcp.claimHtlcTimeoutTxs, rcp.irrevocablySpent)
    watchSpentIfNeeded(rcp.commitTx, rcp.claimHtlcTimeoutTxs ++ rcp.claimHtlcSuccessTxs, rcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(rcp.commitTx) ++ rcp.claimMainOutputTx, rcp.irrevocablySpent)
  }

  def doPublish(rcp: RevokedCommitPublished): Unit = {
    publishIfNeeded(rcp.claimMainOutputTx ++ rcp.mainPenaltyTx ++ rcp.htlcPenaltyTxs ++ rcp.claimHtlcDelayedPenaltyTxs, rcp.irrevocablySpent)
    watchSpentIfNeeded(rcp.commitTx, rcp.mainPenaltyTx ++ rcp.htlcPenaltyTxs, rcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(rcp.commitTx) ++ rcp.claimMainOutputTx, rcp.irrevocablySpent)
  }

  def handleNormalSync(d: DATA_NORMAL, channelReestablish: ChannelReestablish): Unit = {
    var sendQueue = Queue.empty[LightningMessage]
    channelReestablish match {
      case ChannelReestablish(_, _, nextRemoteRevocationNumber, yourLastPerCommitmentSecret, _) if !Helpers.checkLocalCommit(d, nextRemoteRevocationNumber) =>
        // if next_remote_revocation_number is greater than our local commitment index, it means that either we are using an outdated commitment, or they are lying
        // but first we need to make sure that the last per_commitment_secret that they claim to have received from us is correct for that next_remote_revocation_number minus 1
        if (d.commitments.localParams.keys.commitmentSecret(nextRemoteRevocationNumber - 1) == yourLastPerCommitmentSecret) {
          // their data checks out, we indeed seem to be using an old revoked commitment, and must absolutely *NOT* publish it, because that would be a cheating attempt and they
          // would punish us by taking all the funds in the channel
          val error = Error(d.channelId, "please publish your local commitment")
          StoreBecomeSend(DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(d.commitments, channelReestablish), CLOSING, error)
        } else {
          // they lied! the last per_commitment_secret they claimed to have received from us is invalid
          throw new RuntimeException
        }
      case ChannelReestablish(_, nextLocalCommitmentNumber, _, _, _) if !Helpers.checkRemoteCommit(d, nextLocalCommitmentNumber) =>
        // if next_local_commit_number is more than one more our remote commitment index, it means that either we are using an outdated commitment, or they are lying
        // there is no way to make sure that they are saying the truth, the best thing to do is ask them to publish their commitment right now
        // maybe they will publish their commitment, in that case we need to remember their commitment point in order to be able to claim our outputs
        // not that if they don't comply, we could publish our own commitment (it is not stale, otherwise we would be in the case above)
        val error = Error(d.channelId, "please publish your local commitment")
        StoreBecomeSend(DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(d.commitments, channelReestablish), CLOSING, error)
      case _ =>
        // normal case, our data is up-to-date
        if (channelReestablish.nextLocalCommitmentNumber == 1 && d.commitments.localCommit.index == 0) {
          // If next_local_commitment_number is 1 in both the channel_reestablish it sent and received, then the node MUST retransmit funding_locked, otherwise it MUST NOT
          val nextPerCommitmentPoint = d.commitments.localParams.keys.commitmentPoint(index = 1L)
          val fundingLocked = FundingLocked(d.commitments.channelId, nextPerCommitmentPoint)
          sendQueue = sendQueue :+ fundingLocked
        }

        val (commitments1, sendQueue1) = handleSync(channelReestablish, d)
        sendQueue = sendQueue ++ sendQueue1

        // BOLT 2: A node if it has sent a previous shutdown MUST retransmit shutdown.
        d.localShutdown.foreach(localShutdown => sendQueue = sendQueue :+ localShutdown)
        BECOME(d.copy(commitments = commitments1), OPEN)
        SEND(sendQueue:_*)
    }
  }

  def handleNegotiationsSync(d: DATA_NEGOTIATING, conf: OnChainFeeConf): Unit = {
    if (d.commitments.localParams.isFunder) {
      // we could use the last closing_signed we sent, but network fees may have changed while we were offline so it is better to restart from scratch
      val (closingTx, closingSigned) = Closing.makeFirstClosingTx(d.commitments, d.localShutdown.scriptPubKey, d.remoteShutdown.scriptPubKey, conf.feeEstimator, conf.feeTargets)
      StoreBecomeSend(d.copy(closingTxProposed = d.closingTxProposed :+ List(ClosingTxProposed(closingTx.tx, closingSigned))), OPEN, d.localShutdown, closingSigned)
    } else {
      // we start a new round of negotiation
      val closingTxProposed1 = if (d.closingTxProposed.last.isEmpty) d.closingTxProposed else d.closingTxProposed :+ Nil
      StoreBecomeSend(d.copy(closingTxProposed = closingTxProposed1), OPEN, d.localShutdown)
    }
  }

  def handleSync(channelReestablish: ChannelReestablish, d: HasNormalCommitments): (NormalCommits, Queue[LightningMessage]) = {
    var sendQueue = Queue.empty[LightningMessage]
    // first we clean up unacknowledged updates
    val commitments1 = d.commitments.copy(
      localChanges = d.commitments.localChanges.copy(proposed = Nil),
      remoteChanges = d.commitments.remoteChanges.copy(proposed = Nil),
      localNextHtlcId = d.commitments.localNextHtlcId - d.commitments.localChanges.proposed.collect { case u: UpdateAddHtlc => u }.size,
      remoteNextHtlcId = d.commitments.remoteNextHtlcId - d.commitments.remoteChanges.proposed.collect { case u: UpdateAddHtlc => u }.size)

    def resendRevocation: Unit = {
      // let's see the state of remote sigs
      if (commitments1.localCommit.index == channelReestablish.nextRemoteRevocationNumber) {
        // nothing to do
      } else if (commitments1.localCommit.index == channelReestablish.nextRemoteRevocationNumber + 1) {
        // our last revocation got lost, let's resend it
        val localPerCommitmentSecret = commitments1.localParams.keys.commitmentSecret(d.commitments.localCommit.index - 1)
        val localNextPerCommitmentPoint = commitments1.localParams.keys.commitmentPoint(d.commitments.localCommit.index + 1)
        val revocation = RevokeAndAck(channelId = commitments1.channelId, perCommitmentSecret = localPerCommitmentSecret, nextPerCommitmentPoint = localNextPerCommitmentPoint)
        sendQueue = sendQueue :+ revocation
      } else throw new RuntimeException
    }

    // re-sending sig/rev (in the right order)
    commitments1.remoteNextCommitInfo match {
      case Left(waitingForRevocation) if waitingForRevocation.nextRemoteCommit.index + 1 == channelReestablish.nextLocalCommitmentNumber =>
        // we had sent a new sig and were waiting for their revocation
        // they had received the new sig but their revocation was lost during the disconnection
        // they will send us the revocation, nothing to do here
        resendRevocation
      case Left(waitingForRevocation) if waitingForRevocation.nextRemoteCommit.index == channelReestablish.nextLocalCommitmentNumber =>
        // we had sent a new sig and were waiting for their revocation
        // they didn't receive the new sig because of the disconnection
        // we just resend the same updates and the same sig

        val revWasSentLast = commitments1.localCommit.index > waitingForRevocation.sentAfterLocalCommitIndex
        if (!revWasSentLast) resendRevocation

        commitments1.localChanges.signed.foreach(revocation => sendQueue :+= revocation)
        sendQueue = sendQueue :+ waitingForRevocation.sent

        if (revWasSentLast) resendRevocation
      case Right(_) if commitments1.remoteCommit.index + 1 == channelReestablish.nextLocalCommitmentNumber =>
        // there wasn't any sig in-flight when the disconnection occurred
        resendRevocation
      case _ => throw new RuntimeException
    }

    (commitments1, sendQueue)
  }

  def handleRemoteShutdown(d: DATA_NORMAL, remote: Shutdown, conf: OnChainFeeConf): (HasNormalCommitments, List[ChannelMessage]) = {
    if (!Closing.isValidFinalScriptPubkey(remote.scriptPubKey)) {
      throw new RuntimeException
    } else if (d.commitments.remoteHasUnsignedOutgoingHtlcs) {
      throw new RuntimeException
    } else if (d.commitments.remoteHasUnsignedOutgoingUpdateFee) {
      throw new RuntimeException
    } else if (d.commitments.localHasUnsignedOutgoingHtlcs) { // do we have unsigned outgoing htlcs?
      require(d.localShutdown.isEmpty, "can't have pending unsigned outgoing htlcs after having sent Shutdown")
      // are we in the middle of a signature?
      d.commitments.remoteNextCommitInfo match {
        case Left(waitForRevocation) =>
          // yes, let's just schedule a new signature ASAP, which will include all pending unsigned changes
          val commitments1 = d.commitments.copy(remoteNextCommitInfo = Left(waitForRevocation.copy(reSignAsap = true)))
          // in the meantime we won't send new changes
          (d.copy(commitments = commitments1, remoteShutdown = Some(remote)), Nil)
        case Right(_) =>
          // in the meantime we won't send new changes
          (d.copy(remoteShutdown = Some(remote)), Nil)
      }
    } else {
      maybeStartNegotiations(d, remote, conf)
    }
  }

  def maybeStartNegotiations(d: DATA_NORMAL, remote: Shutdown, conf: OnChainFeeConf): (HasNormalCommitments, List[ChannelMessage]) = {
    // so we don't have any unsigned outgoing htlcs
    val (localShutdown, sendList) = d.localShutdown match {
      case Some(localShutdown) =>
        (localShutdown, Nil)
      case None =>
        val localShutdown = Shutdown(d.channelId, d.commitments.localParams.defaultFinalScriptPubKey)
        // we need to send our shutdown if we didn't previously
        (localShutdown, localShutdown :: Nil)
    }
    // are there pending signed htlcs on either changes? we need to have received their last revocation!
    if (d.commitments.hasNoPendingHtlcsOrFeeUpdate) {
      // there are no pending signed changes, let's go directly to NEGOTIATING
      if (d.commitments.localParams.isFunder) {
        // we are funder, need to initiate the negotiation by sending the first closing_signed
        val (closingTx, closingSigned) = Closing.makeFirstClosingTx(d.commitments, localShutdown.scriptPubKey, remote.scriptPubKey, conf.feeEstimator, conf.feeTargets)
        (DATA_NEGOTIATING(d.commitments, localShutdown, remote, List(List(ClosingTxProposed(closingTx.tx, closingSigned))), bestUnpublishedClosingTxOpt = None), sendList :+ closingSigned)
      } else {
        // we are fundee, will wait for their closing_signed
        (DATA_NEGOTIATING(d.commitments, localShutdown, remote, closingTxProposed = List(Nil), bestUnpublishedClosingTxOpt = None), sendList)
      }
    } else {
      // there are some pending signed changes, we need to wait for them to be settled (fail/fulfill htlcs and sign fee updates)
      (d.copy(localShutdown = Some(localShutdown), remoteShutdown = Some(remote)), sendList)
    }
  }

  def handleMutualClose(closingTx: Transaction, d: Either[DATA_NEGOTIATING, DATA_CLOSING]): Unit = {
    val nextData = d match {
      case Left(negotiating) => DATA_CLOSING(negotiating.commitments, fundingTx = None, System.currentTimeMillis, negotiating.closingTxProposed.flatten.map(_.unsignedTx), closingTx :: Nil)
      case Right(closing) => closing.copy(mutualClosePublished = closing.mutualClosePublished :+ closingTx)
    }

    BECOME(STORE(nextData), CLOSING)
    doPublish(closingTx)
  }

  def handleNegotiations(d: DATA_NEGOTIATING, m: ClosingSigned, conf: OnChainFeeConf): Unit = {
    val signedClosingTx = Closing.checkClosingSignature(d.commitments, d.localShutdown.scriptPubKey, d.remoteShutdown.scriptPubKey, m.feeSatoshis, m.signature)
    if (d.closingTxProposed.last.lastOption.map(_.localClosingSigned.feeSatoshis).contains(m.feeSatoshis) || d.closingTxProposed.flatten.size >= LNParams.maxNegotiationIterations) {
      handleMutualClose(signedClosingTx, Left(d.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx))))
    } else {
      // if we are fundee and we were waiting for them to send their first closing_signed, we don't have a lastLocalClosingFee, so we compute a firstClosingFee
      val lastLocalClosingFee = d.closingTxProposed.last.lastOption.map(_.localClosingSigned.feeSatoshis)
      val nextClosingFee = if (d.commitments.localCommit.spec.toLocal == 0.msat) {
        // if we have nothing at stake there is no need to negotiate and we accept their fee right away
        m.feeSatoshis
      } else {
        Closing.nextClosingFee(localClosingFee = lastLocalClosingFee.getOrElse(Closing.firstClosingFee(d.commitments, d.localShutdown.scriptPubKey,
          d.remoteShutdown.scriptPubKey, conf.feeEstimator, conf.feeTargets)), remoteClosingFee = m.feeSatoshis)
      }
      val (closingTx, closingSigned) = Closing.makeClosingTx(d.commitments, d.localShutdown.scriptPubKey, d.remoteShutdown.scriptPubKey, nextClosingFee)
      if (lastLocalClosingFee.contains(nextClosingFee)) {
        // next computed fee is the same than the one we previously sent (probably because of rounding), let's close now
        handleMutualClose(signedClosingTx, Left(d.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx))))
      } else if (nextClosingFee == m.feeSatoshis) {
        // we have converged!
        val closingTxProposed1 = d.closingTxProposed match {
          case previousNegotiations :+ currentNegotiation => previousNegotiations :+ (currentNegotiation :+ ClosingTxProposed(closingTx.tx, closingSigned))
        }
        handleMutualClose(signedClosingTx, Left(d.copy(closingTxProposed = closingTxProposed1, bestUnpublishedClosingTxOpt = Some(signedClosingTx))))
        SEND(closingSigned)
      } else {
        val closingTxProposed1 = d.closingTxProposed match {
          case previousNegotiations :+ currentNegotiation => previousNegotiations :+ (currentNegotiation :+ ClosingTxProposed(closingTx.tx, closingSigned))
        }
        StoreBecomeSend(d.copy(closingTxProposed = closingTxProposed1, bestUnpublishedClosingTxOpt = Some(signedClosingTx)), OPEN, closingSigned)
      }
    }
  }
}