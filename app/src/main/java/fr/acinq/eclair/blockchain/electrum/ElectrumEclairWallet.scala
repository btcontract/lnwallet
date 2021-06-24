package fr.acinq.eclair.blockchain.electrum

import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.bitcoin.{ByteVector32, OP_PUSHDATA, OP_RETURN, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.eclair.blockchain.{EclairWallet, MakeFundingTxResponse, OnChainBalance, TxAndFee}
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{ActorRef, ActorSystem}

import fr.acinq.eclair.blockchain.electrum.ElectrumClient.BroadcastTransaction
import fr.acinq.eclair.blockchain.electrum.db.CompleteChainWalletInfo
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.addressToPublicKeyScript
import scodec.bits.ByteVector
import akka.util.Timeout
import akka.pattern.ask


case class ElectrumEclairWallet(wallet: ActorRef, ewt: ElectrumWalletType, info: CompleteChainWalletInfo)(implicit system: ActorSystem, ec: ExecutionContext, timeout: Timeout) extends EclairWallet {
  override def getReceiveAddresses: Future[Address2PubKey] = (wallet ? GetCurrentReceiveAddresses).mapTo[GetCurrentReceiveAddressesResponse].map(_.address2PubKey)
  private def isInChain(error: fr.acinq.eclair.blockchain.bitcoind.rpc.Error): Boolean = error.message.toLowerCase contains "already in block chain"
  private def emptyUtxo(pubKeyScript: ByteVector): TxOut = TxOut(Satoshi(0L), pubKeyScript)

  override def getBalance: Future[OnChainBalance] = (wallet ? GetBalance).mapTo[GetBalanceResponse].map {
    balance => OnChainBalance(balance.confirmed, balance.unconfirmed)
  }

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

  override def commit(tx: Transaction): Future[Boolean] = {
    val broadcast = BroadcastTransaction(tx)
    val commit = CommitTransaction(tx)

    (wallet ? broadcast).flatMap {
      case ElectrumClient.BroadcastTransactionResponse(_, None) => (wallet ? commit).mapTo[Boolean]
      case res: ElectrumClient.BroadcastTransactionResponse if res.error.exists(isInChain) => (wallet ? commit).mapTo[Boolean]
      case res: ElectrumClient.BroadcastTransactionResponse if res.error.isDefined => Future(false)
      case ElectrumClient.ServerError(_: ElectrumClient.BroadcastTransaction, _) => Future(false)
    }
  }

  override def sendPreimageBroadcast(preimages: Set[ByteVector32], address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val preimageTxOuts = preimages.toList.map(_.bytes).map(OP_PUSHDATA.apply).grouped(2).map(OP_RETURN :: _).map(Script.write).map(emptyUtxo).toList
    val sendAll = SendAll(Script write addressToPublicKeyScript(address, ewt.chainHash), preimageTxOuts, feeRatePerKw, OPT_IN_FULL_RBF)
    (wallet ? sendAll).mapTo[CompleteTransactionResponse].map(_.result.get)
  }

  override def sendPayment(amount: Satoshi, address: String, feeRatePerKw: FeeratePerKw): Future[TxAndFee] = {
    val publicKeyScript = Script write addressToPublicKeyScript(address, ewt.chainHash)

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
