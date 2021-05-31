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

package fr.acinq.eclair.blockchain.electrum

import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.BroadcastTransaction
import fr.acinq.bitcoin.{ByteVector32, OP_PUSHDATA, OP_RETURN, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.blockchain.{EclairWallet, MakeFundingTxResponse, OnChainBalance, TxAndFee}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{ActorRef, ActorSystem}

import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.addressToPublicKeyScript
import scodec.bits.ByteVector
import akka.pattern.ask


class ElectrumEclairWallet(val wallet: ActorRef, chainHash: ByteVector32)(implicit system: ActorSystem, ec: ExecutionContext, timeout: akka.util.Timeout) extends EclairWallet {

  override def getBalance: Future[OnChainBalance] = (wallet ? GetBalance).mapTo[GetBalanceResponse].map {
    balance => OnChainBalance(balance.confirmed, balance.unconfirmed)
  }

  override def getReceiveAddresses: Future[Address2PrivKey] = (wallet ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse].map(_.a2p)

  override def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse] =
    getBalance.flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        val sendAllCommand = SendAll(pubkeyScript, Nil, feeRatePerKw, TxIn.SEQUENCE_FINAL)
        (wallet ? sendAllCommand).mapTo[SendAllResponse].map(_.result).map {
          case Some(res) => MakeFundingTxResponse(res.tx, 0, res.fee)
          case None => throw new RuntimeException
        }

      case _ =>
        val txOut = TxOut(amount, pubkeyScript)
        val tx = Transaction(version = 2, txIn = Nil, txOut = txOut :: Nil, lockTime = 0)
        val completeTxCommand = CompleteTransaction(tx, feeRatePerKw, TxIn.SEQUENCE_FINAL)
        (wallet ? completeTxCommand).mapTo[CompleteTransactionResponse].map(_.result).map {
          case Some(res) => MakeFundingTxResponse(res.tx, 0, res.fee)
          case None => throw new RuntimeException
        }
    }

  override def commit(tx: Transaction): Future[Boolean] =
    (wallet ? BroadcastTransaction(tx)) flatMap {
      case ElectrumClient.BroadcastTransactionResponse(_, None) =>
        // tx broadcast successfully: commit tx
        (wallet ? CommitTransaction(tx)).mapTo[Boolean]
      case ElectrumClient.BroadcastTransactionResponse(_, errorOpt) if errorOpt.exists(_.message contains "transaction already in block chain") =>
        // tx was already in the blockchain, that's weird but it is OK
        (wallet ? CommitTransaction(tx)).mapTo[Boolean]
      case ElectrumClient.BroadcastTransactionResponse(_, errorOpt) if errorOpt.isDefined =>
        // tx broadcast definitely failed
        Future(false)
      case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, _) =>
        // tx broadcast definitely failed
        Future(false)
    }

  private val emptyUtxo: ByteVector => TxOut = TxOut(Satoshi(0L), _: ByteVector)

  override def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val preimageTxOuts = preimages.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(emptyUtxo)
    val sendAll = SendAll(Script write addressToPublicKeyScript(address, chainHash), preimageTxOuts.toList, feeRatePerKw, OPT_IN_FULL_RBF)
    (wallet ? sendAll).mapTo[CompleteTransactionResponse].map(_.result.get)
  }

  override def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, chainHash)

    getBalance.flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        val sendAll = SendAll(publicKeyScript, Nil, feeRatePerKw, OPT_IN_FULL_RBF)
        (wallet ? sendAll).mapTo[SendAllResponse].map(_.result.get)

      case _ =>
        val txOut = TxOut(amount, publicKeyScript)
        val tx = Transaction(version = 2, txIn = Nil, txOut = txOut :: Nil, lockTime = 0)
        val completeTx = CompleteTransaction(tx, feeRatePerKw, sequenceFlag = OPT_IN_FULL_RBF)
        (wallet ? completeTx).mapTo[CompleteTransactionResponse].map(_.result.get)
    }
  }

  override def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent] = for {
    response <- (wallet ? IsDoubleSpent(tx)).mapTo[IsDoubleSpentResponse]
  } yield (response.depth, response.isDoubleSpent)
}
