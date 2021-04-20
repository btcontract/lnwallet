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
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.bitcoin.Crypto.PublicKey
import scala.concurrent.Future
import scodec.bits.ByteVector

/**
 * Created by PM on 06/07/2017.
 */
object EclairWallet {
  type Addresses = List[String]
  type TxAndFee = (Transaction, Satoshi)
  type DepthAndDoubleSpent = (Long, Boolean)
  final val OPT_IN_FULL_RBF = TxIn.SEQUENCE_FINAL - 2
  final val MAX_RECEIVE_ADDRESSES = 4
}

trait EclairWallet {
  def getBalance: Future[OnChainBalance]

  def getReceiveAddresses: Future[Addresses]

  def getReceivePubkey(receiveAddress: Option[String] = None): Future[PublicKey]

  def makeFundingTx(pubkeyScript: ByteVector, amount: Satoshi, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse]

  def makeAllFundingTx(pubkeyScript: ByteVector, feeRatePerKw: FeeratePerKw): Future[MakeFundingTxResponse]

  /**
   * Committing *must* include publishing the transaction on the network.
   *
   * We need to be very careful here, we don't want to consider a commit 'failed' if we are not absolutely sure that the
   * funding tx won't end up on the blockchain: if that happens and we have cancelled the channel, then we would lose our
   * funds!
   *
   * @return true if success
   *         false IF AND ONLY IF *HAS NOT BEEN PUBLISHED* otherwise funds are at risk!!!
   */
  def commit(tx: Transaction): Future[Boolean]

  def sendPreimageBroadcast(preimages: Set[ByteVector32], feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def sendPaymentAll(address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee]

  def rollback(tx: Transaction): Future[Boolean]

  def doubleSpent(tx: Transaction): Future[DepthAndDoubleSpent]
}

final case class OnChainBalance(confirmed: Satoshi, unconfirmed: Satoshi)

final case class MakeFundingTxResponse(fundingTx: Transaction, fundingTxOutputIndex: Int, fee: Satoshi)
