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

import ElectrumWallet._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._

import scala.util.{Success, Try}
import akka.actor.{ActorRef, FSM, PoisonPill}
import fr.acinq.eclair.blockchain.electrum.db.WalletDb
import fr.acinq.eclair.blockchain.bitcoind.rpc.Error
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Transactions
import fr.acinq.eclair.blockchain.TxAndFee
import fr.acinq.bitcoin.Crypto.PublicKey
import Blockchain.RETARGETING_PERIOD
import scala.annotation.tailrec
import scodec.bits.ByteVector


class ElectrumWallet(seed: ByteVector, client: ActorRef, params: ElectrumWallet.WalletParameters) extends FSM[ElectrumWallet.State, ElectrumWallet.Data] {

  val master: ExtendedPrivateKey = DeterministicWallet.generate(seed)

  val accountMaster: ExtendedPrivateKey = accountKey(master, params.chainHash)

  val changeMaster: ExtendedPrivateKey = changeKey(master, params.chainHash)

  client ! ElectrumClient.AddStatusListener(self)

  def persist(chunk: Seq[Blockchain.BlockIndex] = Nil): Unit = {
    val headers = for (blockIndex <- chunk) yield blockIndex.header
    params.walletDb.addHeaders(chunk.head.height, headers)
  }

  def persistAndNotify(data: Data): Data = {
    val isSame = data.lastReadyMessage.contains(data.readyMessage)
    if (isSame) data else doPersist(data)
  }

  def doPersist(data: Data): Data = {
    params.walletDb persist PersistentData(data)
    context.system.eventStream publish data.readyMessage
    data.copy(lastReadyMessage = data.readyMessage.asSome)
  }

  def loadData: Data = {
    val blockchain = params.chainHash match {
      case Block.RegtestGenesisBlock.hash => Blockchain.fromGenesisBlock(Block.RegtestGenesisBlock.hash, Block.RegtestGenesisBlock.header)
      case _ => Blockchain.fromCheckpoints(checkpoints = CheckPoint.load(params.chainHash, params.walletDb), chainhash = params.chainHash)
    }

    val headers = params.walletDb.getHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, Int.MaxValue)
    val blockchain1 = Blockchain.addHeadersChunk(blockchain, blockchain.checkpoints.size * RETARGETING_PERIOD, headers)

    params.walletDb.readPersistentData.map { persisted =>
      val firstAccountKeys = for (idx <- 0 until persisted.accountKeysCount) yield derivePrivateKey(accountMaster, idx)
      val firstChangeKeys = for (idx <- 0 until persisted.changeKeysCount) yield derivePrivateKey(changeMaster, idx)

      Data(blockchain1, firstAccountKeys.toVector, firstChangeKeys.toVector, status = persisted.status,
        transactions = persisted.transactions, heights = persisted.heights, history = persisted.history,
        proofs = persisted.proofs, pendingHistoryRequests = Set.empty, pendingHeadersRequests = Set.empty,
        pendingTransactionRequests = Set.empty, pendingTransactions = persisted.pendingTransactions,
        lastReadyMessage = None)
    } getOrElse {
      val firstAccountKeys = for (idx <- 0 until params.swipeRange) yield derivePrivateKey(accountMaster, idx)
      val firstChangeKeys = for (idx <- 0 until params.swipeRange) yield derivePrivateKey(changeMaster, idx)
      Data(blockchain1, firstAccountKeys.toVector, firstChangeKeys.toVector)
    }
  }

  startWith(DISCONNECTED, loadData)

  when(DISCONNECTED) {
    case Event(_: ElectrumClient.ElectrumReady, data) =>
      client ! ElectrumClient.HeaderSubscription(self)
      goto(WAITING_FOR_TIP) using data
  }

  when(WAITING_FOR_TIP) {
    case Event(ElectrumClient.HeaderSubscriptionResponse(height, _), data) if height < data.blockchain.height =>
      goto(DISCONNECTED) replying PoisonPill

    case Event(_: ElectrumClient.HeaderSubscriptionResponse, data) if data.blockchain.bestchain.isEmpty =>
      client ! ElectrumClient.GetHeaders(data.blockchain.checkpoints.size * RETARGETING_PERIOD, RETARGETING_PERIOD)
      goto(SYNCING) using data

    case Event(ElectrumClient.HeaderSubscriptionResponse(_, header), data) if header == data.blockchain.tip.header =>
      for (key <- data.accountKeys) client ! ElectrumClient.ScriptHashSubscription(computeScriptHashFromPublicKey(key.publicKey), self)
      for (key <- data.changeKeys) client ! ElectrumClient.ScriptHashSubscription(computeScriptHashFromPublicKey(key.publicKey), self)
      goto(RUNNING) using persistAndNotify(data)

    case Event(_: ElectrumClient.HeaderSubscriptionResponse, data) =>
      client ! ElectrumClient.GetHeaders(data.blockchain.tip.height + 1, RETARGETING_PERIOD)
      goto(SYNCING) using data
  }

  when(SYNCING) {
    case Event(ElectrumClient.GetHeadersResponse(_, Nil, _), data) =>
      for (key <- data.accountKeys) client ! ElectrumClient.ScriptHashSubscription(computeScriptHashFromPublicKey(key.publicKey), self)
      for (key <- data.changeKeys) client ! ElectrumClient.ScriptHashSubscription(computeScriptHashFromPublicKey(key.publicKey), self)
      goto(RUNNING) using persistAndNotify(data)

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), data) =>
      Try apply Blockchain.addHeaders(data.blockchain, start, headers) map { blockchain1 =>
        val (blockchain2, saveBlockIndexes) = Blockchain.optimize(blockchain1)
        client ! GetHeaders(blockchain2.tip.height + 1, RETARGETING_PERIOD)
        saveBlockIndexes.grouped(RETARGETING_PERIOD).foreach(persist)
        goto(SYNCING) using data.copy(blockchain = blockchain2)
      } getOrElse goto(DISCONNECTED) replying PoisonPill

    case Event(_: ElectrumClient.HeaderSubscriptionResponse, _) =>
      // Ignore this, request header chunks until server has nothing to send
      stay
  }

  when(RUNNING) {
    case Event(ElectrumClient.HeaderSubscriptionResponse(_, header), data) if data.blockchain.tip.header == header =>
      stay

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), data) =>
      val difficulty = Blockchain.getDifficulty(data.blockchain, height, params.walletDb)
      val isDifficultyValid = difficulty.forall(header.bits.==)

      if (isDifficultyValid) {
        Try apply Blockchain.addHeader(data.blockchain, height, header) map { blockchain1 =>
          val (blockchain2, saveBlockIndexes) = Blockchain.optimize(blockchain1)
          saveBlockIndexes.grouped(RETARGETING_PERIOD).foreach(persist)
          val data1 = data.copy(blockchain = blockchain2)
          stay using persistAndNotify(data1)
        } getOrElse stay replying PoisonPill
      } else stay replying PoisonPill

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if data.status.get(scriptHash).contains(status) =>
      val missing = data.history.getOrElse(scriptHash, Nil).map(_.txHash).filterNot(data.transactions.contains).toSet -- data.pendingTransactionRequests
      val data1 = data.copy(pendingHistoryRequests = data.pendingTransactionRequests ++ missing)
      for (txid <- missing) client ! GetTransaction(txid)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, _), data) if !data.accountKeyMap.contains(scriptHash) && !data.changeKeyMap.contains(scriptHash) =>
      // Our wallet does not contain a key related to this script hash
      stay

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if status.isEmpty =>
      val status1 = data.status.updated(scriptHash, status)
      val data1 = data.copy(status = status1)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) =>
      val data1 = data.copy(status = data.status.updated(scriptHash, status), pendingHistoryRequests = data.pendingHistoryRequests + scriptHash)
      val alreadyHasStatus = data.status.contains(Try(ByteVector32 fromValidHex status) getOrElse ByteVector32.Zeroes)
      client ! ElectrumClient.GetScriptHashHistory(scriptHash)

      if (alreadyHasStatus) stay using persistAndNotify(data1) else {
        val (data2, key) = if (data.changeKeyMap contains scriptHash) {
          val nextChangeChildNumber = data.changeKeys.last.path.lastChildNumber + 1
          val newKey = derivePrivateKey(changeMaster, nextChangeChildNumber)
          data1.copy(changeKeys = data.changeKeys :+ newKey) -> newKey
        } else {
          val nextAccountChildNumber = data.accountKeys.last.path.lastChildNumber + 1
          val newKey = derivePrivateKey(accountMaster, nextAccountChildNumber)
          data1.copy(accountKeys = data.accountKeys :+ newKey) -> newKey
        }

        val newKeyScriptHash = computeScriptHashFromPublicKey(key.publicKey)
        client ! ElectrumClient.ScriptHashSubscription(newKeyScriptHash, self)
        stay using persistAndNotify(data2)
      }

    case Event(ElectrumClient.GetScriptHashHistoryResponse(scriptHash, items), data) =>
      // If we don't already have a header at this height, or a pending request to download the header chunk it's in, download this header chunk
      // We don't have this header because it's most likely older than our current checkpoint, downloading the whole header is quick and easy to verify

      val shadowItems = for {
        existingItems <- data.history.get(scriptHash).toList
        item <- existingItems if !items.exists(_.txHash == item.txHash)
      } yield item

      val items0 = items ++ shadowItems
      val pendingHeadersRequests1 = collection.mutable.HashSet.empty[GetHeaders]
      pendingHeadersRequests1 ++= data.pendingHeadersRequests

      def downloadHeadersIfMissing(txid: ByteVector32, height: Int): Unit = {
        val isHeaderMissing = data.blockchain.getHeader(height).orElse(params.walletDb getHeader height).isEmpty
        val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
        val isAlreadyRequested = pendingHeadersRequests1.contains(request)
        client ! GetMerkle(txid, height)

        if (isHeaderMissing && !isAlreadyRequested) {
          pendingHeadersRequests1.add(request)
          client ! request
        }
      }

      val (heights1, pendingTransactionRequests1) = items0.foldLeft(data.heights -> data.pendingTransactionRequests) {
        case (heights ~ hashes, item) if !data.transactions.contains(item.txHash) && !data.pendingTransactionRequests.contains(item.txHash) =>
          client ! GetTransaction(item.txHash)
          if (item.height > 0) downloadHeadersIfMissing(item.txHash, item.height)
          (heights.updated(item.txHash, item.height), hashes + item.txHash)
        case (heights ~ hashes, item) =>
          (heights.updated(item.txHash, item.height), hashes)
      }

      heights1 foreach { case (txid, height) =>
        data.heights.get(txid) match {
          case None if height > 0 => downloadHeadersIfMissing(txid, height.toInt)
          case Some(prevHeight) if prevHeight != height && height > 0 => downloadHeadersIfMissing(txid, height.toInt)
          case Some(prevHeight) if prevHeight == height && height > 0 && !data.proofs.contains(txid) => downloadHeadersIfMissing(txid, height.toInt)
          case _ =>
        }
      }

      val data1 = data.copy(heights = heights1,
        history = data.history.updated(scriptHash, items0),
        pendingHistoryRequests = data.pendingHistoryRequests - scriptHash,
        pendingTransactionRequests = pendingTransactionRequests1,
        pendingHeadersRequests = pendingHeadersRequests1.toSet)

      stay using persistAndNotify(data1)

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), data) =>
      val result = Try apply Blockchain.addHeadersChunk(data.blockchain, start, headers)

      result map { blockchain1 =>
        params.walletDb.addHeaders(start, headers)
        stay using data.copy(blockchain = blockchain1)
      } getOrElse {
        // Peer has sent us some invalid headers
        goto(DISCONNECTED) using data replying PoisonPill
      }

    case Event(GetTransactionResponse(tx, contextOpt), data) =>
      val data1 = data.copy(pendingTransactionRequests = data.pendingTransactionRequests - tx.txid)

      data.computeTransactionDelta(tx) map { case (received, sent, feeOpt) =>
        context.system.eventStream publish data.transactionReceived(tx, feeOpt, received, sent)
        for (pendingTx <- data.pendingTransactions) self ! GetTransactionResponse(pendingTx, contextOpt)
        val data2 = data1.copy(transactions = data.transactions.updated(tx.txid, tx), pendingTransactions = Nil)
        stay using persistAndNotify(data2)
      } getOrElse {
        // Missing parents, we could not connect this transaction so proceed without it
        val data2 = data1.copy(pendingTransactions = data.pendingTransactions :+ tx)
        stay using persistAndNotify(data2)
      }

    case Event(ServerError(req: GetTransaction, _), data) if data.pendingTransactionRequests.contains(req.txid) =>
      goto(DISCONNECTED) replying PoisonPill

    case Event(merkleResponse @ GetMerkleResponse(txid, _, height, _, _), data) =>
      data.blockchain.getHeader(height).orElse(params.walletDb getHeader height) match {
        case Some(responseBlockHeader) if responseBlockHeader.hashMerkleRoot == merkleResponse.root =>
          val proofs1 = if (data isAlien txid) data.proofs else data.proofs.updated(txid, merkleResponse)
          stay using data.copy(proofs = proofs1)

        case None =>
          // this is probably because the tx is old and within our checkpoints => request the whole header chunk
          val headerRequest = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
          val alreadyHaveRequest = data.pendingHeadersRequests.contains(headerRequest)

          if (!alreadyHaveRequest) {
            val pending1 = data.pendingHeadersRequests + headerRequest
            val data1 = data.copy(pendingHeadersRequests = pending1)
            client ! headerRequest
            self ! merkleResponse
            stay using data1
          } else stay

        case _ =>
          val data1 = data.copy(transactions = data.transactions - txid)
          stay using data1 replying PoisonPill
      }

    case Event(bc: ElectrumClient.BroadcastTransaction, _) =>
      client forward bc
      stay

    case Event(CommitTransaction(tx), data) =>
      val data1: Data = data.commitTransaction(tx)
      val (received, sent, feeOpt) = data.computeTransactionDelta(tx).get
      context.system.eventStream publish data1.transactionReceived(tx, feeOpt, received, sent)
      stay using persistAndNotify(data1) replying true
  }

  whenUnhandled {
    case Event(IsDoubleSpent(tx), data) =>
      val depth = data.computeTransactionDepth(tx.txid)
      val txInSet = tx.txIn.map(_.outPoint).toSet

      val isDoubleSpent = for {
        (txid, height) <- data.heights if computeDepth(data.blockchain.height, height) >= 1
        tx1 <- data.transactions.get(txid) if tx1.txIn.map(_.outPoint).toSet.intersect(txInSet).nonEmpty
      } yield tx1.txid != tx.txid

      // Whether we can another confirmed tx which spends the same input
      stay replying IsDoubleSpentResponse(tx, depth, isDoubleSpent exists identity)

    case Event(ElectrumClient.ElectrumDisconnected, data) =>
      // remove status for each script hash for which we have pending requests
      // this will make us query script hash history for these script hashes again when we reconnect
      goto(DISCONNECTED) using data.copy(status = data.status -- data.pendingHistoryRequests,
        pendingHistoryRequests = Set.empty, pendingTransactionRequests = Set.empty,
        pendingHeadersRequests = Set.empty, lastReadyMessage = None)

    case Event(GetCurrentReceiveAddresses, data) => stay replying GetCurrentReceiveAddressesResponse(data.currentReceiveAddresses)

    case Event(GetBalance, data) => stay replying data.balance

    case Event(CompleteTransaction(tx, feeRatePerKw, sequenceFlag), data) =>
      Try(data.completeTransaction(tx, feeRatePerKw, params.dustLimit, params.allowSpendUnconfirmed, sequenceFlag)) match {
        case Success(txAndFee) => stay replying CompleteTransactionResponse(txAndFee.asSome)
        case _ => stay replying CompleteTransactionResponse(None)
      }

    case Event(SendAll(publicKeyScript, extraUtxos, feeRatePerKw, sequenceFlag), data) =>
      Try(data.spendAll(publicKeyScript, extraUtxos, feeRatePerKw, params.dustLimit, sequenceFlag)) match {
        case Success(txAndFee) => stay replying SendAllResponse(txAndFee.asSome)
        case _ => stay replying SendAllResponse(None)
      }

    case Event(ElectrumClient.BroadcastTransaction(tx), _) =>
      val notConnected = Error(code = -1, "wallet is not connected").asSome
      stay replying ElectrumClient.BroadcastTransactionResponse(tx, notConnected)
  }

  initialize
}

object ElectrumWallet {
  case class WalletParameters(chainHash: ByteVector32, walletDb: WalletDb, dustLimit: Satoshi, swipeRange: Int, allowSpendUnconfirmed: Boolean)

  sealed trait State
  case object DISCONNECTED extends State
  case object WAITING_FOR_TIP extends State
  case object SYNCING extends State
  case object RUNNING extends State

  sealed trait Request
  sealed trait Response

  case object GetBalance extends Request
  case class GetBalanceResponse(confirmed: Satoshi, unconfirmed: Satoshi) extends Response {
    val totalBalance: Satoshi = confirmed + unconfirmed
  }

  case object GetCurrentReceiveAddresses extends Request
  case class GetCurrentReceiveAddressesResponse(a2p: Address2PrivKey) extends Response

  case class CompleteTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class CompleteTransactionResponse(result: Option[TxAndFee] = None) extends Response

  case class SendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request
  case class SendAllResponse(result: Option[TxAndFee] = None) extends Response

  case class CommitTransaction(tx: Transaction) extends Request

  case class SendTransaction(tx: Transaction) extends Request
  case class SendTransactionReponse(tx: Transaction) extends Response

  case object InsufficientFunds extends Response
  case class AmountBelowDustLimit(dustLimit: Satoshi) extends Response

  case class IsDoubleSpent(tx: Transaction) extends Request
  case class IsDoubleSpentResponse(tx: Transaction, depth: Long, isDoubleSpent: Boolean) extends Response

  sealed trait WalletEvent
  case class TransactionReceived(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, walletAddreses: List[String], feeOpt: Option[Satoshi] = None) extends WalletEvent
  case class WalletReady(confirmedBalance: Satoshi, unconfirmedBalance: Satoshi, height: Long, timestamp: Long) extends WalletEvent

  def segwitAddress(key: ExtendedPrivateKey, chainHash: ByteVector32): String = computeBIP84Address(key.publicKey, chainHash)

  def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2wpkh(key)

  def computeScriptHashFromPublicKey(key: PublicKey): ByteVector32 = {
    val serializedPubKeyScript = Script write computePublicKeyScript(key)
    Crypto.sha256(serializedPubKeyScript).reverse
  }

  def accountPath(chainHash: ByteVector32): List[Long] = chainHash match {
    case Block.RegtestGenesisBlock.hash => hardened(84) :: hardened(1) :: hardened(0) :: Nil
    case Block.TestnetGenesisBlock.hash => hardened(84) :: hardened(1) :: hardened(0) :: Nil
    case Block.LivenetGenesisBlock.hash => hardened(84) :: hardened(0) :: hardened(0) :: Nil
    case _ => throw new RuntimeException
  }

  def accountKey(master: ExtendedPrivateKey, chainHash: ByteVector32): ExtendedPrivateKey =
    DeterministicWallet.derivePrivateKey(master, accountPath(chainHash) ::: 0L :: Nil)

  def changeKey(master: ExtendedPrivateKey, chainHash: ByteVector32): ExtendedPrivateKey =
    DeterministicWallet.derivePrivateKey(master, accountPath(chainHash) ::: 1L :: Nil)

  def totalAmount(utxos: Seq[Utxo] = Nil): Satoshi = Satoshi(utxos.map(_.item.value).sum)

  def computeFee(weight: Int, feeRatePerKw: Long): Satoshi = Satoshi(weight * feeRatePerKw / 1000)

  def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try(PublicKey(txIn.witness.stack.last)).toOption

  def computeDepth(currentHeight: Long, txHeight: Long): Long = if (txHeight <= 0) 0 else currentHeight - txHeight + 1

  case class Utxo(key: ExtendedPrivateKey, item: ElectrumClient.UnspentItem) {
    def outPoint: OutPoint = item.outPoint
  }

  case class Data(blockchain: Blockchain, accountKeys: Vector[ExtendedPrivateKey], changeKeys: Vector[ExtendedPrivateKey],
                  status: Map[ByteVector32, String] = Map.empty, transactions: Map[ByteVector32, Transaction] = Map.empty, heights: Map[ByteVector32, Int] = Map.empty,
                  history: Map[ByteVector32, PersistentData.TransactionHistoryItemList] = Map.empty, proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty,
                  pendingHistoryRequests: Set[ByteVector32] = Set.empty, pendingTransactionRequests: Set[ByteVector32] = Set.empty,
                  pendingHeadersRequests: Set[GetHeaders] = Set.empty, pendingTransactions: List[Transaction] = Nil,
                  lastReadyMessage: Option[WalletReady] = None) {

    private val unused = new String

    val toAddress: ExtendedPrivateKey => String = segwitAddress(_, blockchain.chainHash)

    lazy val accountKeyMap: Map[ByteVector32, ExtendedPrivateKey] = accountKeys.map(key => computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

    lazy val changeKeyMap: Map[ByteVector32, ExtendedPrivateKey] = changeKeys.map(key => computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

    private lazy val firstUnusedAccountKeys = accountKeys.view.filter(key => status get computeScriptHashFromPublicKey(key.publicKey) contains unused).take(MAX_RECEIVE_ADDRESSES)

    private lazy val firstUnusedChangeKeys = changeKeys.find(key => status get computeScriptHashFromPublicKey(key.publicKey) contains unused)

    private lazy val publicScriptMap = (accountKeys ++ changeKeys).map { key =>
      val script = computePublicKeyScript(key.publicKey)
      Script.write(script) -> key
    }.toMap

    lazy val utxos: Seq[Utxo] = history.keys.toSeq.flatMap(getUtxos)

    def isReady(swipeRange: Int): Boolean = status.values.count(_.isEmpty) >= swipeRange * 2 && pendingTransactionRequests.isEmpty && pendingHistoryRequests.isEmpty

    def readyMessage: WalletReady = WalletReady(balance.confirmed, balance.unconfirmed, blockchain.tip.height, blockchain.tip.header.time)

    def isAlien(txid: ByteVector32): Boolean = !transactions.contains(txid) && !pendingTransactionRequests.contains(txid) && !pendingTransactions.exists(_.txid == txid)

    def currentReceiveAddresses: Address2PrivKey = {
      val privateKeys = if (firstUnusedAccountKeys.isEmpty) accountKeys.take(MAX_RECEIVE_ADDRESSES) else firstUnusedAccountKeys
      privateKeys.map(toAddress).zip(privateKeys).toMap
    }

    def currentChangeKey: ExtendedPrivateKey = firstUnusedChangeKeys.getOrElse(changeKeys.head)

    def isMine(txIn: TxIn): Boolean = extractPubKeySpentFrom(txIn).map(computePublicKeyScript).map(Script.write).exists(publicScriptMap.contains)

    def isSpend(txIn: TxIn, scriptHash: ByteVector32): Boolean = extractPubKeySpentFrom(txIn).exists(pub => computeScriptHashFromPublicKey(pub) == scriptHash)

    def isReceive(txOut: TxOut, scriptHash: ByteVector32): Boolean = publicScriptMap.get(txOut.publicKeyScript).exists(key => computeScriptHashFromPublicKey(key.publicKey) == scriptHash)

    def isMine(txOut: TxOut): Boolean = publicScriptMap.contains(txOut.publicKeyScript)

    def computeTransactionDepth(txid: ByteVector32): Long = heights.get(txid).map(height => if (height > 0) computeDepth(blockchain.tip.height, height) else 0).getOrElse(0)

    def accountOrChangeKey(scriptHash: ByteVector32): ExtendedPrivateKey = accountKeyMap.get(scriptHash) match { case None => changeKeyMap(scriptHash) case Some(key) => key }

    def getUtxos(scriptHash: ByteVector32): Seq[Utxo] =
      history.get(scriptHash) match {
        case Some(Nil) => Nil
        case None => Nil

        case Some(historyItems) =>
          // This is the private key for this script hash
          val key = accountOrChangeKey(scriptHash)

          val unspents = for {
            item <- historyItems
            tx <- transactions.get(item.txHash).toList
            (txOut, index) <- tx.txOut.zipWithIndex if isReceive(txOut, scriptHash)
            unspent = ElectrumClient.UnspentItem(item.txHash, index, txOut.amount.toLong, item.height)
          } yield Utxo(key, unspent)

          // Find all transactions that send to or receive from this script hash
          val txs = historyItems.flatMap(transactions get _.txHash).flatMap(_.txIn).map(_.outPoint)
          // Because we may have unconfirmed UTXOs that are spend by unconfirmed transactions
          unspents.filterNot(utxo => txs contains utxo.outPoint)
      }

    def calculateBalance(scriptHash: ByteVector32): (Satoshi, Satoshi) =
      history.get(scriptHash) match {
        case Some(Nil) => (0.sat, 0.sat)
        case None => (0.sat, 0.sat)

        case Some(items) =>
          val (confirmedItems, unconfirmedItems) = items.partition(_.height > 0)
          val confirmedTxs = confirmedItems.map(_.txHash).flatMap(transactions.get)
          val unconfirmedTxs = unconfirmedItems.map(_.txHash).flatMap(transactions.get)

          def findOurSpentOutputs(txs: Seq[Transaction] = Nil): Seq[TxOut] = for {
            input <- txs.flatMap(_.txIn) if isSpend(input, scriptHash)
            transaction <- transactions.get(input.outPoint.txid)
          } yield transaction.txOut(input.outPoint.index.toInt)

          val confirmedSpents = findOurSpentOutputs(confirmedTxs)
          val unconfirmedSpents = findOurSpentOutputs(unconfirmedTxs)

          val received = isReceive(_: TxOut, scriptHash)
          val confirmedReceived = confirmedTxs.flatMap(_.txOut).filter(received)
          val unconfirmedReceived = unconfirmedTxs.flatMap(_.txOut).filter(received)

          val confirmedBalance = confirmedReceived.map(_.amount).sum - confirmedSpents.map(_.amount).sum
          val unconfirmedBalance = unconfirmedReceived.map(_.amount).sum - unconfirmedSpents.map(_.amount).sum
          (confirmedBalance, unconfirmedBalance)
      }

    lazy val balance: GetBalanceResponse = {
      val startState = GetBalanceResponse(0L.sat, 0L.sat)
      (accountKeyMap.keys ++ changeKeyMap.keys).map(calculateBalance).foldLeft(startState) {
        case (GetBalanceResponse(confirmed, unconfirmed), confirmed1 ~ unconfirmed1) =>
          GetBalanceResponse(confirmed + confirmed1, unconfirmed + unconfirmed1)
      }
    }

    def transactionReceived(tx: Transaction, feeOpt: Option[Satoshi], received: Satoshi, sent: Satoshi): TransactionReceived = {
      val walletAddresses = tx.txOut.filter(isMine).map(_.publicKeyScript).flatMap(publicScriptMap.get).map(toAddress)
      TransactionReceived(tx, computeTransactionDepth(tx.txid), received, sent, walletAddresses.toList, feeOpt)
    }

    type SatOpt = Option[Satoshi]
    type ReceivedSentFee = (Satoshi, Satoshi, SatOpt)
    def computeTransactionDelta(tx: Transaction): Option[ReceivedSentFee] = {
      // Computes the effect of this transaction on the wallet
      val ourInputs = tx.txIn.filter(isMine)

      val missingParent = ourInputs.exists { txIn =>
        !transactions.contains(txIn.outPoint.txid)
      }

      if (missingParent) None else {
        val received = tx.txOut.filter(isMine).map(_.amount).sum
        val sent = ourInputs.map(txIn => transactions(txIn.outPoint.txid) txOut txIn.outPoint.index.toInt).map(_.amount).sum
        val feeOpt = if (ourInputs.size == tx.txIn.size) Some(sent - tx.txOut.map(_.amount).sum) else None
        (received, sent, feeOpt).asSome
      }
    }

    def addUtxosWithDummySig(tx: Transaction, usableUtxos: Seq[Utxo], sequenceFlag: Long): Transaction = {
      // Returns a tx where all utxos have been added as inputs, signed with dummy invalid signatures

      val txIn1 = for {
        utxo <- usableUtxos
        witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
      } yield TxIn(utxo.outPoint, signatureScript = ByteVector.empty, sequenceFlag, witness)
      tx.copy(txIn = txIn1)
    }

    def completeTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, allowSpendUnconfirmed: Boolean, sequenceFlag: Long): TxAndFee = {
      val usable = if (allowSpendUnconfirmed) utxos.sortBy(_.item.value) else utxos.filter(_.item.height > 0).sortBy(_.item.value)
      val amount = tx.txOut.map(_.amount).sum

      def computeFee(candidates: Seq[Utxo], change: Option[TxOut] = None): Satoshi = {
        val tx1 = addUtxosWithDummySig(tx, usableUtxos = candidates, sequenceFlag = sequenceFlag)
        val weight = change.map(tx1.addOutput).getOrElse(tx1).weight(Protocol.PROTOCOL_VERSION)
        Transactions.weight2fee(feeRatePerKw, weight)
      }

      val dummyScript = computePublicKeyScript(currentChangeKey.publicKey)
      val dummyChange = TxOut(Satoshi(0), dummyScript)

      @tailrec
      def loop(current: Seq[Utxo], remaining: Seq[Utxo] = Nil): (Seq[Utxo], Option[TxOut]) = totalAmount(current) match {
        case total if total - computeFee(current, None) < amount && remaining.isEmpty => throw new RuntimeException("Insufficient funds")
        case total if total - computeFee(current, None) < amount => loop(remaining.head +: current, remaining.tail)

        case total if total - computeFee(current, None) <= amount + dustLimit => (current, None)
        case total if total - computeFee(current, dummyChange.asSome) <= amount + dustLimit && remaining.isEmpty => (current, None)
        case total if total - computeFee(current, dummyChange.asSome) <= amount + dustLimit => loop(remaining.head +: current, remaining.tail)
        case total => (current, dummyChange.copy(amount = total - computeFee(current, dummyChange.asSome) - amount).asSome)
      }

      val (selected, changeOpt) = loop(Seq.empty, usable)
      val tx1 = addUtxosWithDummySig(tx, selected, sequenceFlag)
      val tx2 = changeOpt.map(tx1.addOutput).getOrElse(tx1)
      val tx3 = signTransaction(tx2)

      val fee = selected.map(_.item.value.sat).sum - tx3.txOut.map(_.amount).sum
      require(tx.txIn.isEmpty, "Cannot complete a tx that already has inputs")
      require(amount > dustLimit, "Amount to send is below dust limit")
      TxAndFee(tx3, fee)
    }

    def signTransaction(tx: Transaction): Transaction = {
      // This will throw if UTXO related to input is not found

      val txIn1 = for {
        (txIn, idx) <- tx.txIn.zipWithIndex
        utxo = utxos.find(_.outPoint == txIn.outPoint).get
        pay2pkh = Script.pay2pkh(pubKey = utxo.key.publicKey)
        sig = Transaction.signInput(tx, idx, pay2pkh, SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, utxo.key.privateKey)
      } yield txIn.copy(witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil), signatureScript = ByteVector.empty)
      tx.copy(txIn = txIn1)
    }

    def commitTransaction(tx: Transaction): Data = {
      // Remove all our utxos spent by this tx, call this method if the tx was broadcast successfully.
      // Since we base our utxos computation on the history from server, we need to update the history right away if we want to be able to build chained unconfirmed transactions.
      // A few seconds later electrum will notify us and the entry will be overwritten. Note that we need to take into account both inputs and outputs, because there may be change.
      val scripts = tx.txIn.filter(isMine).flatMap(extractPubKeySpentFrom).map(computeScriptHashFromPublicKey) ++ tx.txOut.filter(isMine).map(_.publicKeyScript).map(computeScriptHash)

      val history2 =
        scripts.foldLeft(history) {
          case (history1, scriptHash) =>
            val entry = history1.get(scriptHash) match {
              case Some(items) if items.map(_.txHash).contains(tx.txid) => items
              case Some(items) => TransactionHistoryItem(0, tx.txid) :: items
              case None => TransactionHistoryItem(0, tx.txid) :: Nil
            }

            history1.updated(scriptHash, entry)
        }

      copy(transactions = transactions + (tx.txid -> tx), heights = heights + (tx.txid -> 0), history = history2)
    }

    def spendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long): TxAndFee = {
      val tx1 = addUtxosWithDummySig(Transaction(version = 2, Nil, TxOut(balance.totalBalance, publicKeyScript) :: extraUtxos, lockTime = 0), utxos, sequenceFlag)
      val fee = Transactions.weight2fee(weight = tx1.weight(Protocol.PROTOCOL_VERSION), feeratePerKw = feeRatePerKw)
      require(balance.totalBalance - fee > dustLimit, "Resulting tx amount to send is below dust limit")
      val tx2 = tx1.copy(txOut = TxOut(balance.totalBalance - fee, publicKeyScript) :: extraUtxos)
      TxAndFee(signTransaction(tx2), fee)
    }

    def spendAll(publicKeyScript: Seq[ScriptElt], extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long): TxAndFee =
      spendAll(Script.write(publicKeyScript), extraUtxos, feeRatePerKw, dustLimit, sequenceFlag)
  }

  case class PersistentData(accountKeysCount: Int, changeKeysCount: Int, status: Map[ByteVector32, String], transactions: Map[ByteVector32, Transaction],
                            heights: Map[ByteVector32, Int], history: Map[ByteVector32, PersistentData.TransactionHistoryItemList],
                            proofs: Map[ByteVector32, GetMerkleResponse], pendingTransactions: List[Transaction] = Nil)

  object PersistentData {
    type TransactionHistoryItemList = List[ElectrumClient.TransactionHistoryItem]
    def apply(d: Data) = new PersistentData(d.accountKeys.length, d.changeKeys.length, d.status, d.transactions, d.heights, d.history, d.proofs, d.pendingTransactions)
  }
}
