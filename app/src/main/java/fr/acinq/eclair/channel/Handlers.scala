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


trait Handlers { me: ChannelNormal =>
  def doPublish(closingTx: Transaction): Unit = {
    val replyEvent: BitcoinEvent = BITCOIN_TX_CONFIRMED(closingTx)
    chainWallet.watcher ! WatchConfirmed(receiver, closingTx, replyEvent, LNParams.minDepthBlocks)
    chainWallet.watcher ! PublishAsap(closingTx)
  }

  def publishIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(PublishAsap).foreach(event => chainWallet.watcher ! event)

  // Watch utxos only we can spend
  def watchConfirmedIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(BITCOIN_TX_CONFIRMED).foreach { replyEvent =>
      chainWallet.watcher ! WatchConfirmed(receiver, replyEvent.tx, replyEvent, LNParams.minDepthBlocks)
    }

  // Watch utxos that both we and peer can spend
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
        val first = Closing.firstClosingFee(d.commitments, d.localShutdown.scriptPubKey, d.remoteShutdown.scriptPubKey, conf)
        Closing.nextClosingFee(localClosingFee = lastLocalClosingFee.getOrElse(first), remoteClosingFee = m.feeSatoshis)
      }
      val (closingTx, closingSigned) = Closing.makeClosingTx(d.commitments, d.localShutdown.scriptPubKey, d.remoteShutdown.scriptPubKey, nextClosingFee)
      if (lastLocalClosingFee.contains(nextClosingFee)) {
        // next computed fee is the same than the one we previously sent (probably because of rounding), let's close now
        val d1 = d.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx))
        handleMutualClose(signedClosingTx, Left(d1))
      } else if (nextClosingFee == m.feeSatoshis) {
        // we have converged!
        val closingTxProposed1 = d.closingTxProposed match {
          case previousNegotiations :+ currentNegotiation => previousNegotiations :+ (currentNegotiation :+ ClosingTxProposed(closingTx.tx, closingSigned))
        }
        val d1 = d.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx), closingTxProposed = closingTxProposed1)
        handleMutualClose(signedClosingTx, Left(d1))
        SEND(closingSigned)
      } else {
        val closingTxProposed1 = d.closingTxProposed match {
          case previousNegotiations :+ currentNegotiation => previousNegotiations :+ (currentNegotiation :+ ClosingTxProposed(closingTx.tx, closingSigned))
        }
        StoreBecomeSend(d.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx), closingTxProposed = closingTxProposed1), OPEN, closingSigned)
      }
    }
  }

  def handleMutualClose(closingTx: Transaction, d: Either[DATA_NEGOTIATING, DATA_CLOSING]): Unit = {
    val nextData = d match {
      case Left(negotiating) => DATA_CLOSING(negotiating.commitments, System.currentTimeMillis, negotiating.closingTxProposed.flatten.map(_.unsignedTx), closingTx :: Nil)
      case Right(closing) => closing.copy(mutualClosePublished = closing.mutualClosePublished :+ closingTx)
    }

    BECOME(STORE(nextData), CLOSING)
    doPublish(closingTx)
  }
}