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

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.BroadcastTransaction
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.addressToPublicKeyScript
import akka.pattern.ask

import fr.acinq.bitcoin.{ByteVector32, Crypto, OP_PUSHDATA, OP_RETURN, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.blockchain.{EclairWallet, MakeFundingTxResponse, OnChainBalance}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{ActorRef, ActorSystem}
import scodec.bits.ByteVector


class ElectrumEclairWallet(val wallet: ActorRef, chainHash: ByteVector32)(implicit system: ActorSystem, ec: ExecutionContext, timeout: akka.util.Timeout) extends EclairWallet {

  override def getBalance: Future[OnChainBalance] = (wallet ? GetBalance).mapTo[GetBalanceResponse].map(balance => OnChainBalance(balance.confirmed, balance.unconfirmed))

  override def getReceiveAddresses: Future[Addresses] = (wallet ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse].map(_.addresses.toList)

  override def getReceivePubkey(receiveAddress: Option[String] = None): Future[Crypto.PublicKey] = Future failed new RuntimeException("Not implemented")

  override def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse] = {
    val tx = Transaction(version = 2, txIn = Nil, txOut = TxOut(amount, pubkeyScript) :: Nil, lockTime = 0)
    val nonRbfRequest = CompleteTransaction(tx, feeRatePerKw, TxIn.SEQUENCE_FINAL)

    (wallet ? nonRbfRequest).mapTo[CompleteTransactionResponse].map {
      case CompleteTransactionResponse(tx1, fee1, None) => MakeFundingTxResponse(tx1, 0, fee1)
      case CompleteTransactionResponse(_, _, Some(error)) => throw error
    }
  }

  override def commit(tx: Transaction): Future[Boolean] =
    (wallet ? BroadcastTransaction(tx)) flatMap {
      case ElectrumClient.BroadcastTransactionResponse(_, None) =>
        //tx broadcast successfully: commit tx
        wallet ? CommitTransaction(tx)
      case ElectrumClient.BroadcastTransactionResponse(_, errorOpt) if errorOpt.exists(_.message contains "transaction already in block chain") =>
        // tx was already in the blockchain, that's weird but it is OK
        wallet ? CommitTransaction(tx)
      case ElectrumClient.BroadcastTransactionResponse(_, errorOpt) if errorOpt.isDefined =>
        //tx broadcast failed: cancel tx
        wallet ? CancelTransaction(tx)
      case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, _) =>
        //tx broadcast failed: cancel tx
        wallet ? CancelTransaction(tx)
    } map {
      case CommitTransactionResponse(_) => true
      case CancelTransactionResponse(_) => false
    }

  override def sendPreimageBroadcast(preimage: ByteVector32, feeRatePerKw: FeeratePerKw): Future[CompleteTransactionResponse] = {
    val publicKeyScript = Script.write(OP_RETURN :: OP_PUSHDATA(preimage.bytes) :: Nil)
    val tx = Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(0L), publicKeyScript) :: Nil, lockTime = 0)
    val rbfRequest = CompleteTransaction(tx, feeRatePerKw, OPT_IN_FULL_RBF)
    (wallet ? rbfRequest).mapTo[CompleteTransactionResponse]
  }

  override def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[CompleteTransactionResponse] = {
    val publicKeyScript = Script.write(addressToPublicKeyScript(address, chainHash))
    val tx = Transaction(version = 2, txIn = Nil, txOut = TxOut(amount, publicKeyScript) :: Nil, lockTime = 0)
    val rbfRequest = CompleteTransaction(tx, feeRatePerKw, OPT_IN_FULL_RBF)
    (wallet ? rbfRequest).mapTo[CompleteTransactionResponse]
  }

  override def sendPaymentAll(address: String, feeRatePerKw: FeeratePerKw): Future[SendAllResponse] = {
    val publicKeyScript = Script.write(addressToPublicKeyScript(address, chainHash))
    val rbfRequest = SendAll(publicKeyScript, feeRatePerKw, OPT_IN_FULL_RBF)
    (wallet ? rbfRequest).mapTo[SendAllResponse]
  }

  override def rollback(tx: Transaction): Future[Boolean] = (wallet ? CancelTransaction(tx)).map(_ => true)

  override def doubleSpent(tx: Transaction): Future[Boolean] = (wallet ? IsDoubleSpent(tx)).mapTo[IsDoubleSpentResponse].map(_.isDoubleSpent)
}
