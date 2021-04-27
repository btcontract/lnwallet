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
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.addressToPublicKeyScript
import akka.pattern.ask

import fr.acinq.bitcoin.{ByteVector32, OP_PUSHDATA, OP_RETURN, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.blockchain.{EclairWallet, MakeFundingTxResponse, OnChainBalance}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{ActorRef, ActorSystem}
import scodec.bits.ByteVector


class ElectrumEclairWallet(val wallet: ActorRef, chainHash: ByteVector32)(implicit system: ActorSystem, ec: ExecutionContext, timeout: akka.util.Timeout) extends EclairWallet {

  override def getBalance: Future[OnChainBalance] = (wallet ? GetBalance).mapTo[GetBalanceResponse].map(balance => OnChainBalance(balance.confirmed, balance.unconfirmed))

  override def getReceiveAddresses: Future[Address2PrivKey] = (wallet ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse].map(_.a2p)

  override def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse] = {
    getBalance.flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        (wallet ? SendAll(pubkeyScript, feeRatePerKw, TxIn.SEQUENCE_FINAL)).mapTo[SendAllResponse].map {
          case SendAllResponse(Some((tx1, fee1))) => MakeFundingTxResponse(tx1, 0, fee1)
          case SendAllResponse(None) => throw new RuntimeException
        }

      case _ =>
        val tx = Transaction(version = 2, txIn = Nil, txOut = TxOut(amount, pubkeyScript) :: Nil, lockTime = 0)
        (wallet ? CompleteTransaction(tx, feeRatePerKw, TxIn.SEQUENCE_FINAL)).mapTo[CompleteTransactionResponse].map {
          case CompleteTransactionResponse(Some((tx1, fee1))) => MakeFundingTxResponse(tx1, 0, fee1)
          case CompleteTransactionResponse(None) => throw new RuntimeException
        }
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

  override def sendPreimageBroadcast(preimages: Set[ByteVector32], feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val txOuts = preimages.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(TxOut(Satoshi(0L), _))
    val rbfRequest = CompleteTransaction(Transaction(version = 2, txIn = Nil, txOut = txOuts.toList, lockTime = 0), feeRatePerKw, OPT_IN_FULL_RBF)

    (wallet ? rbfRequest).mapTo[CompleteTransactionResponse].map {
      case CompleteTransactionResponse(Some(completeSignedTxAndFee)) => completeSignedTxAndFee
      case CompleteTransactionResponse(None) => throw new RuntimeException
    }
  }

  override def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val publicKeyScript = Script.write(addressToPublicKeyScript(address, chainHash))

    getBalance.flatMap {
      case chainBalance if chainBalance.totalBalance == amount =>
        (wallet ? SendAll(publicKeyScript, feeRatePerKw, OPT_IN_FULL_RBF)).mapTo[SendAllResponse].map {
          case SendAllResponse(Some(completeSignedTxAndFee)) => completeSignedTxAndFee
          case SendAllResponse(None) => throw new RuntimeException
        }

      case _ =>
        val tx = Transaction(version = 2, txIn = Nil, txOut = TxOut(amount, publicKeyScript) :: Nil, lockTime = 0)
        (wallet ? CompleteTransaction(tx, feeRatePerKw, OPT_IN_FULL_RBF)).mapTo[CompleteTransactionResponse].map {
          case CompleteTransactionResponse(Some(completeSignedTxAndFee)) => completeSignedTxAndFee
          case CompleteTransactionResponse(None) => throw new RuntimeException
        }
    }
  }

  override def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent] = for {
    response <- (wallet ? IsDoubleSpent(tx)).mapTo[IsDoubleSpentResponse]
  } yield (response.depth, response.isDoubleSpent)
}
