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

package fr.acinq.eclair.blockchain

import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction, TxIn}
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPrivateKey
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import scala.concurrent.Future
import scodec.bits.ByteVector


object EclairWallet {
  type TxAndFee = (Transaction, Satoshi)
  type DepthAndDoubleSpent = (Long, Boolean)
  type Address2PrivKey = Map[String, ExtendedPrivateKey]
  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 4
}

trait EclairWallet {
  def getBalance: Future[OnChainBalance]

  def getReceiveAddresses: Future[Address2PrivKey]

  def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse]

  def sendPreimageBroadcast(preimages: Set[ByteVector32], feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def commit(tx: Transaction): Future[Boolean]

  def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent]
}

case class OnChainBalance(confirmed: Satoshi, unconfirmed: Satoshi) {
  val totalBalance: Satoshi = confirmed + unconfirmed
}

case class MakeFundingTxResponse(fundingTx: Transaction, fundingTxOutputIndex: Int, fee: Satoshi) {
  val fundingPubkeyScript: ByteVector = fundingTx.txOut(fundingTxOutputIndex).publicKeyScript
  val fundingAmount: Satoshi = fundingTx.txOut(fundingTxOutputIndex).amount
}
