package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.electrum.db.{HeaderDb, WalletDb}
import akka.actor.{ActorRef, FSM, PoisonPill}
import scala.util.{Success, Try}

import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.bitcoind.rpc.Error
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Transactions
import fr.acinq.eclair.blockchain.TxAndFee
import Blockchain.RETARGETING_PERIOD
import scala.collection.immutable
import scala.annotation.tailrec
import scodec.bits.ByteVector


class ElectrumWallet(client: ActorRef, chainSync: ActorRef, params: WalletParameters, ewt: ElectrumWalletType) extends FSM[State, ElectrumData] {

  def persistAndNotify(data: ElectrumData): ElectrumData = {
    if (data.lastReadyMessage contains data.currentReadyMessage) return data
    val data1 = data.copy(lastReadyMessage = data.currentReadyMessage.asSome)
    params.walletDb.persist(data1.toPersistent, data1.balance.totalBalance, ewt.xPub.publicKey)
    context.system.eventStream.publish(data1.currentReadyMessage)
    data1
  }

  context.system.eventStream.subscribe(channel = classOf[Blockchain], subscriber = self)

  client ! ElectrumClient.AddStatusListener(self)

  startWith(DISCONNECTED, null)

  when(DISCONNECTED) {
    case Event(raw: ByteVector, null) =>
      // Serialized data may become with much usage
      // Deserialzie it in this dedicated thread to not slow down UI
      val persisted = persistentDataCodec.decode(raw.toBitVector).require.value
      val blockChain = Blockchain(ewt.chainHash, checkpoints = Vector.empty, headersMap = Map.empty, bestchain = Vector.empty)
      val firstAccountKeys = for (idx <- 0 until persisted.accountKeysCount) yield derivePublicKey(ewt.accountMaster, idx)
      val firstChangeKeys = for (idx <- 0 until persisted.changeKeysCount) yield derivePublicKey(ewt.changeMaster, idx)

      val data1 = ElectrumData(ewt, blockChain, firstAccountKeys.toVector, firstChangeKeys.toVector, persisted.status, persisted.transactions,
        persisted.heights, persisted.history, persisted.proofs, pendingHistoryRequests = Set.empty, pendingHeadersRequests = Set.empty,
        pendingTransactionRequests = Set.empty, pendingTransactions = persisted.pendingTransactions)
      stay using data1

    case Event(blockchain1: Blockchain, data) =>
      for (key <- data.accountKeys) client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(key.publicKey), self)
      for (key <- data.changeKeys) client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(key.publicKey), self)
      val data1 = data.copy(blockchain = blockchain1)
      goto(RUNNING) using persistAndNotify(data1)
  }

  when(RUNNING) {
    case Event(blockchain1: Blockchain, data) =>
      val data1 = data.copy(blockchain = blockchain1)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if data.status.get(scriptHash).contains(status) =>
      val missing = data.history.getOrElse(scriptHash, Nil).map(_.txHash).filterNot(data.transactions.contains).toSet -- data.pendingTransactionRequests
      val data1 = data.copy(pendingHistoryRequests = data.pendingTransactionRequests ++ missing)
      for (txid <- missing) client ! GetTransaction(txid)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if !data.accountKeyMap.contains(scriptHash) && !data.changeKeyMap.contains(scriptHash) =>
      log.warning(s"Received status $status for scriptHash $scriptHash which does not match any of our keys")
      stay

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) if status.isEmpty =>
      val status1 = data.status.updated(scriptHash, status)
      val data1 = data.copy(status = status1)
      stay using persistAndNotify(data1)

    case Event(ElectrumClient.ScriptHashSubscriptionResponse(scriptHash, status), data) =>
      val data1 = data.copy(status = data.status.updated(scriptHash, status), pendingHistoryRequests = data.pendingHistoryRequests + scriptHash)
      val statusBytes = Try(ByteVector32 fromValidHex status).getOrElse(ByteVector32.Zeroes)
      client ! ElectrumClient.GetScriptHashHistory(scriptHash)

      data.status.contains(statusBytes) match {
        case false if data.changeKeyMap.contains(scriptHash) =>
          val newKey = derivePublicKey(ewt.changeMaster, data.changeKeys.last.path.lastChildNumber + 1)
          client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(newKey.publicKey), self)
          val data2 = data1.copy(changeKeys = data1.changeKeys :+ newKey)
          stay using persistAndNotify(data2)

        case false =>
          val newKey = derivePublicKey(ewt.accountMaster, data.accountKeys.last.path.lastChildNumber + 1)
          client ! ElectrumClient.ScriptHashSubscription(ewt.computeScriptHashFromPublicKey(newKey.publicKey), self)
          val data2 = data1.copy(accountKeys = data1.accountKeys :+ newKey)
          stay using persistAndNotify(data2)

        case true =>
          // This is not a new scriptHash
          stay using persistAndNotify(data1)
      }

    case Event(ElectrumClient.GetScriptHashHistoryResponse(scriptHash, items), data) =>
      val pendingHeadersRequests1 = collection.mutable.HashSet.empty[GetHeaders]
      pendingHeadersRequests1 ++= data.pendingHeadersRequests

      val shadowItems = for {
        existingItems <- data.history.get(scriptHash).toList
        item <- existingItems if !items.exists(_.txHash == item.txHash)
      } yield item

      val items1 = items ++ shadowItems

      def downloadHeadersIfMissing(height: Int): Unit = {
        if (data.blockchain.getHeader(height).orElse(params.headerDb getHeader height).isEmpty) {
          // we don't have this header because it is older than our checkpoints => request the entire chunk
          val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)
          if (pendingHeadersRequests1 contains request) return
          pendingHeadersRequests1.add(request)
          chainSync ! request
        }
      }

      def process(txid: ByteVector32, height: Int): Unit = {
        downloadHeadersIfMissing(height)
        client ! GetMerkle(txid, height)
      }

      val (heights1, pendingTransactionRequests1) = items1.foldLeft(data.heights -> data.pendingTransactionRequests) {
        case (heights ~ hashes, item) if !data.transactions.contains(item.txHash) && !data.pendingTransactionRequests.contains(item.txHash) =>
          // We retrieve the tx if we don't have it and haven't yet requested it
          client ! GetTransaction(item.txHash)
          if (item.height > 0) process(item.txHash, item.height)
          (heights.updated(item.txHash, item.height), hashes + item.txHash)

        case (heights ~ hashes, item) =>
          // Otherwise we just update the height
          (heights.updated(item.txHash, item.height), hashes)
      }

      heights1.collect {
        case (txid, height1) =>
          (data.heights.get(txid), height1) match {
            case (None, height) if height > 0 => process(txid, height.toInt)
            case (Some(previousHeight), height) if previousHeight != height => if (height > 0) process(txid, height.toInt)
            case (Some(previousHeight), height) if previousHeight == height && height > 0 && !data.proofs.contains(txid) => process(txid, height.toInt)
            case _ => // Nothing to do
          }
      }

      val data1 = data.copy(heights = heights1,
        history = data.history.updated(scriptHash, items1),
        pendingHistoryRequests = data.pendingHistoryRequests - scriptHash,
        pendingTransactionRequests = pendingTransactionRequests1,
        pendingHeadersRequests = pendingHeadersRequests1.toSet)
      stay using persistAndNotify(data1)

    case Event(GetTransactionResponse(tx, contextOpt), data) =>
      val data1 = data.copy(pendingTransactionRequests = data.pendingTransactionRequests - tx.txid)

      data.computeTransactionDelta(tx) map { case (received, sent, feeOpt) =>
        for (pendingTx <- data.pendingTransactions) self ! GetTransactionResponse(pendingTx, contextOpt)
        context.system.eventStream publish data.transactionReceived(tx, feeOpt, received, sent, ewt.xPub)
        val data2 = data1.copy(transactions = data.transactions.updated(tx.txid, tx), pendingTransactions = Nil)
        stay using persistAndNotify(data2)
      } getOrElse {
        // We are currently missing parents for this transaction
        val data2 = data1.copy(pendingTransactions = data.pendingTransactions :+ tx)
        stay using persistAndNotify(data2)
      }

    case Event(ServerError(GetTransaction(txid, _), error), data) if data.pendingTransactionRequests.contains(txid) =>
      log.error(s"Electrum server cannot find history for txid $txid with error $error")
      goto(DISCONNECTED) replying PoisonPill

    case Event(response @ GetMerkleResponse(txid, _, height, _, _), data) =>
      val request = GetHeaders(height / RETARGETING_PERIOD * RETARGETING_PERIOD, RETARGETING_PERIOD)

      data.blockchain.getHeader(height).orElse(params.headerDb getHeader height) match {
        case Some(header) if header.hashMerkleRoot == response.root && data.isTxKnown(txid) =>
          log.info(s"Transaction $txid has been successfully verified")
          val proofs1 = data.proofs.updated(txid, response)
          stay using data.copy(proofs = proofs1)

        case Some(header) if header.hashMerkleRoot == response.root => stay
        case None if data.pendingHeadersRequests.contains(request) => stay

        case None =>
          self ! response
          chainSync ! request
          val pendingHeadersRequest1 = data.pendingHeadersRequests + request
          stay using data.copy(pendingHeadersRequests = pendingHeadersRequest1)

        case _ =>
          log.error(s"Electrum server sent an invalid proof for txid: $txid")
          stay using data.copy(transactions = data.transactions - txid) replying PoisonPill
      }

    case Event(bc: ElectrumClient.BroadcastTransaction, _) =>
      client forward bc
      stay

    case Event(commit: CommitTransaction, data) =>
      val data1 = data.commitTransaction(commit.tx)
      // We use the initial state to compute the effect of the tx
      val (received, sent, fee) = data.computeTransactionDelta(commit.tx).get
      // We notify here because the tx won't be downloaded again (it has been added to the state at commit)
      context.system.eventStream publish data1.transactionReceived(commit.tx, fee, received, sent, ewt.xPub)
      stay using persistAndNotify(data1) replying true
  }

  whenUnhandled {
    case Event(IsDoubleSpent(tx), data) =>
      val doubleSpendTrials: immutable.Iterable[Boolean] = for {
        (txid, height) <- data.heights if computeDepth(data.blockchain.height, height) > 1
        spendingTx <- data.transactions.get(txid) if spendingTx.txid != tx.txid
      } yield doubleSpend(spendingTx, tx)

      val depth = data.computeTransactionDepth(tx.txid)
      val isDoubleSpent = doubleSpendTrials.exists(identity)
      stay replying IsDoubleSpentResponse(tx, depth, isDoubleSpent)

    case Event(GetCurrentReceiveAddresses, data) => stay replying GetCurrentReceiveAddressesResponse(data.currentReceiveAddresses)

    case Event(ElectrumClient.ElectrumDisconnected, data) => goto(DISCONNECTED) using data.reset

    case Event(GetBalance, data) => stay replying data.balance

    case Event(CompleteTransaction(tx, feeRatePerKw, sequenceFlag), data) =>
      Try apply data.completeTransaction(tx, feeRatePerKw, params.dustLimit, params.allowSpendUnconfirmed, sequenceFlag) match {
        case Success(txAndFee) => stay replying CompleteTransactionResponse(txAndFee.asSome)
        case _ => stay replying CompleteTransactionResponse(None)
      }

    case Event(SendAll(publicKeyScript, extraUtxos, feeRatePerKw, sequenceFlag), data) =>
      Try apply data.spendAll(publicKeyScript, extraUtxos, feeRatePerKw, params.dustLimit, sequenceFlag) match {
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
  type TransactionHistoryItemList = List[ElectrumClient.TransactionHistoryItem]

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

  case class GetCurrentReceiveAddressesResponse(address2PubKey: Address2PubKey) extends Response

  case class CompleteTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request

  case class CompleteTransactionResponse(result: Option[TxAndFee] = None) extends Response

  case class SendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, sequenceFlag: Long) extends Request

  case class SendAllResponse(result: Option[TxAndFee] = None) extends Response

  case class CommitTransaction(tx: Transaction) extends Request

  case class SendTransaction(tx: Transaction) extends Request

  case class SendTransactionReponse(tx: Transaction) extends Response

  case class AmountBelowDustLimit(dustLimit: Satoshi) extends Response

  case class IsDoubleSpent(tx: Transaction) extends Request

  case class IsDoubleSpentResponse(tx: Transaction, depth: Long, isDoubleSpent: Boolean) extends Response

  sealed trait WalletEvent {
    def sameXpub(that: ElectrumEclairWallet): Boolean = that.ewt.xPub == xPub
    val xPub: ExtendedPublicKey
  }

  case class TransactionReceived(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, walletAddreses: List[String], xPub: ExtendedPublicKey, feeOpt: Option[Satoshi] = None) extends WalletEvent

  case class WalletReady(confirmedBalance: Satoshi, unconfirmedBalance: Satoshi, height: Long, allConfirmed: Boolean, xPub: ExtendedPublicKey) extends WalletEvent

  def doubleSpend(tx1: Transaction, tx2: Transaction): Boolean = tx1.txIn.map(_.outPoint).toSet.intersect(tx2.txIn.map(_.outPoint).toSet).nonEmpty

  def computeDepth(currentHeight: Long, txHeight: Long): Long = if (txHeight <= 0L) 0L else currentHeight - txHeight + 1L

  def computeFee(weight: Int, feeRatePerKw: Long): Satoshi = Satoshi(weight * feeRatePerKw / 1000L)

  def totalAmount(utxos: Seq[Utxo] = Nil): Satoshi = utxos.map(_.item.value).sum.sat
}

case class Utxo(key: ExtendedPublicKey, item: ElectrumClient.UnspentItem)

case class AccountAndXPrivKey(xPriv: ExtendedPrivateKey, master: ExtendedPrivateKey)

case class WalletParameters(headerDb: HeaderDb, walletDb: WalletDb, dustLimit: Satoshi, swipeRange: Int, allowSpendUnconfirmed: Boolean) {
  lazy val emptyPersistentData: PersistentData = PersistentData(accountKeysCount = swipeRange, changeKeysCount = swipeRange)
  lazy val emptyPersistentDataBytes: ByteVector = persistentDataCodec.encode(emptyPersistentData).require.toByteVector
}

case class ElectrumData(ewt: ElectrumWalletType, blockchain: Blockchain, accountKeys: Vector[ExtendedPublicKey], changeKeys: Vector[ExtendedPublicKey], status: Map[ByteVector32, String] = Map.empty,
                        transactions: Map[ByteVector32, Transaction] = Map.empty, heights: Map[ByteVector32, Int] = Map.empty, history: Map[ByteVector32, TransactionHistoryItemList] = Map.empty,
                        proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty, pendingHistoryRequests: Set[ByteVector32] = Set.empty, pendingTransactionRequests: Set[ByteVector32] = Set.empty,
                        pendingHeadersRequests: Set[GetHeaders] = Set.empty, pendingTransactions: List[Transaction] = Nil, lastReadyMessage: Option[WalletReady] = None) {

  lazy val accountKeyMap: Map[ByteVector32, ExtendedPublicKey] = accountKeys.map(key => ewt.computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

  lazy val changeKeyMap: Map[ByteVector32, ExtendedPublicKey] = changeKeys.map(key => ewt.computeScriptHashFromPublicKey(key.publicKey) -> key).toMap

  lazy val currentReadyMessage: WalletReady = WalletReady(balance.confirmed, balance.unconfirmed, blockchain.tip.height, heights.values.forall(_ > 0), ewt.xPub)

  private lazy val firstUnusedAccountKeys = accountKeys.view.filter(key => status get ewt.computeScriptHashFromPublicKey(key.publicKey) contains new String).take(MAX_RECEIVE_ADDRESSES)

  private lazy val firstUnusedChangeKeys = changeKeys.find(key => status get ewt.computeScriptHashFromPublicKey(key.publicKey) contains new String)

  private lazy val publicScriptMap = (accountKeys ++ changeKeys).map { key => Script.write(ewt computePublicKeyScript key.publicKey) -> key }.toMap

  lazy val utxos: Seq[Utxo] = history.keys.toList.flatMap(getUtxos)

  def currentReceiveAddresses: Address2PubKey = {
    val privateKeys = if (firstUnusedAccountKeys.isEmpty) accountKeys.take(MAX_RECEIVE_ADDRESSES) else firstUnusedAccountKeys
    privateKeys.map(ewt.textAddress).zip(privateKeys).toMap
  }

  // Remove status for each script hash for which we have pending requests, this will make us query script hash history for these script hashes again when we reconnect
  def reset: ElectrumData = copy(status = status -- pendingHistoryRequests, pendingHistoryRequests = Set.empty, pendingTransactionRequests = Set.empty, pendingHeadersRequests = Set.empty, lastReadyMessage = None)

  def isTxKnown(txid: ByteVector32): Boolean = transactions.contains(txid) || pendingTransactionRequests.contains(txid) || pendingTransactions.exists(_.txid == txid)

  def isMine(txIn: TxIn): Boolean = ewt.extractPubKeySpentFrom(txIn).map(ewt.computePublicKeyScript).map(Script.write).exists(publicScriptMap.contains)

  def isSpend(txIn: TxIn, scriptHash: ByteVector32): Boolean = ewt.extractPubKeySpentFrom(txIn).exists(pub => ewt.computeScriptHashFromPublicKey(pub) == scriptHash)

  def isReceive(txOut: TxOut, scriptHash: ByteVector32): Boolean = publicScriptMap.get(txOut.publicKeyScript).exists(key => ewt.computeScriptHashFromPublicKey(key.publicKey) == scriptHash)

  def isMine(txOut: TxOut): Boolean = publicScriptMap.contains(txOut.publicKeyScript)

  def computeTransactionDepth(txid: ByteVector32): Long = heights.get(txid).map(height => if (height > 0) computeDepth(blockchain.tip.height, height) else 0).getOrElse(0)

  def accountOrChangeKey(scriptHash: ByteVector32): ExtendedPublicKey = accountKeyMap.get(scriptHash) match { case None => changeKeyMap(scriptHash) case Some(key) => key }

  def toPersistent: PersistentData = PersistentData(accountKeys.length, changeKeys.length, status, transactions, heights, history, proofs, pendingTransactions)

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
        unspents.filterNot(utxo => txs contains utxo.item.outPoint)
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

  def transactionReceived(tx: Transaction, feeOpt: Option[Satoshi], received: Satoshi, sent: Satoshi, xPub: ExtendedPublicKey): TransactionReceived = {
    val walletAddresses = tx.txOut.filter(isMine).map(_.publicKeyScript).flatMap(publicScriptMap.get).map(ewt.textAddress).toList
    TransactionReceived(tx, computeTransactionDepth(tx.txid), received, sent, walletAddresses, xPub, feeOpt)
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

  def completeTransaction(tx: Transaction, feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, allowSpendUnconfirmed: Boolean, sequenceFlag: Long): TxAndFee = {
    val usable = if (allowSpendUnconfirmed) utxos.sortBy(_.item.value) else utxos.filter(_.item.height > 0).sortBy(_.item.value)
    val amount = tx.txOut.map(_.amount).sum

    def computeFee(candidates: Seq[Utxo], change: Option[TxOut] = None): Satoshi = {
      val tx1 = ewt.addUtxosWithDummySig(usableUtxos = candidates, tx, sequenceFlag = sequenceFlag)
      val weight = change.map(tx1.addOutput).getOrElse(tx1).weight(Protocol.PROTOCOL_VERSION)
      Transactions.weight2fee(feeRatePerKw, weight)
    }

    val changeKey = firstUnusedChangeKeys.getOrElse(changeKeys.head)
    val changeScript = ewt.computePublicKeyScript(changeKey.publicKey)
    val changeTxOut = TxOut(Satoshi(0), changeScript)

    @tailrec
    def loop(current: Seq[Utxo], remaining: Seq[Utxo] = Nil): (Seq[Utxo], Option[TxOut]) = totalAmount(current) match {
      case total if total - computeFee(current, None) < amount && remaining.isEmpty => throw new RuntimeException("Insufficient funds")
      case total if total - computeFee(current, None) < amount => loop(remaining.head +: current, remaining.tail)

      case total if total - computeFee(current, None) <= amount + dustLimit => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amount + dustLimit && remaining.isEmpty => (current, None)
      case total if total - computeFee(current, changeTxOut.asSome) <= amount + dustLimit => loop(remaining.head +: current, remaining.tail)
      case total => (current, changeTxOut.copy(amount = total - computeFee(current, changeTxOut.asSome) - amount).asSome)
    }

    val (selected, changeOpt) = loop(Seq.empty, usable)
    val tx1 = ewt.addUtxosWithDummySig(selected, tx, sequenceFlag)
    val tx2 = changeOpt.map(tx1.addOutput).getOrElse(tx1)
    val tx3 = ewt.signTransaction(utxos, tx2)

    val fee = selected.map(_.item.value.sat).sum - tx3.txOut.map(_.amount).sum
    require(tx.txIn.isEmpty, "Cannot complete a tx that already has inputs")
    require(amount > dustLimit, "Amount to send is below dust limit")
    TxAndFee(tx3, fee)
  }

  def commitTransaction(tx: Transaction): ElectrumData = {
    // Remove all our utxos spent by this tx, call this method if the tx was broadcast successfully.
    // Since we base our utxos computation on the history from server, we need to update the history right away if we want to be able to build chained unconfirmed transactions.
    // A few seconds later electrum will notify us and the entry will be overwritten. Note that we need to take into account both inputs and outputs, because there may be change.
    val incomingScripts = tx.txIn.filter(isMine).flatMap(ewt.extractPubKeySpentFrom).map(ewt.computeScriptHashFromPublicKey)
    val outgoingScripts = tx.txOut.filter(isMine).map(_.publicKeyScript).map(computeScriptHash)
    val scripts = incomingScripts ++ outgoingScripts

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

    copy(transactions = transactions.updated(tx.txid, tx),
      heights = heights.updated(tx.txid, 0),
      history = history2)
  }

  def spendAll(publicKeyScript: ByteVector, extraUtxos: List[TxOut], feeRatePerKw: FeeratePerKw, dustLimit: Satoshi, sequenceFlag: Long): TxAndFee = {
    val tx1 = ewt.addUtxosWithDummySig(utxos, Transaction(version = 2, Nil, TxOut(balance.totalBalance, publicKeyScript) :: extraUtxos, lockTime = 0), sequenceFlag)
    val fee = Transactions.weight2fee(weight = tx1.weight(Protocol.PROTOCOL_VERSION), feeratePerKw = feeRatePerKw)
    require(balance.totalBalance - fee > dustLimit, "Resulting tx amount to send is below dust limit")
    val tx2 = tx1.copy(txOut = TxOut(balance.totalBalance - fee, publicKeyScript) :: extraUtxos)
    TxAndFee(ewt.signTransaction(utxos, tx2), fee)
  }
}

case class PersistentData(accountKeysCount: Int, changeKeysCount: Int,
                          status: Map[ByteVector32, String] = Map.empty, transactions: Map[ByteVector32, Transaction] = Map.empty,
                          heights: Map[ByteVector32, Int] = Map.empty, history: Map[ByteVector32, TransactionHistoryItemList] = Map.empty,
                          proofs: Map[ByteVector32, GetMerkleResponse] = Map.empty, pendingTransactions: List[Transaction] = Nil)
