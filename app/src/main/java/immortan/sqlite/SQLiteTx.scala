package immortan.sqlite

import spray.json._
import fr.acinq.eclair._
import immortan.utils.ImplicitJsonFormats._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._

import java.lang.{Long => JLong}
import immortan.{TxDescription, TxInfo}
import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import scala.util.Try


case class TxSummary(fees: MilliSatoshi, received: MilliSatoshi, sent: MilliSatoshi, count: Long)

class SQLiteTx(db: DBInterface) {
  def updStatus(txid: ByteVector32, depth: Long, isDoubleSpent: Boolean): Unit = {
    db.change(TxTable.updStatusSql, depth: JLong, if (isDoubleSpent) 1L: JLong else 0L: JLong, txid.toHex)
  }

  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)

  def txSummary: Try[TxSummary] = db.select(TxTable.selectSummarySql).headTry { rc =>
    TxSummary(fees = MilliSatoshi(rc long 0), received = MilliSatoshi(rc long 1), sent = MilliSatoshi(rc long 2), count = rc long 3)
  }

  def putTx(event: TransactionReceived, isIncoming: Long, description: TxDescription, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit =
    db.change(TxTable.newSql, event.tx.toString, event.tx.txid.toHex, event.depth: JLong, event.received.toLong: JLong, event.sent.toLong: JLong,
      event.feeOpt.map(_.toLong: JLong).getOrElse(0L: JLong), System.currentTimeMillis: JLong /* FIRST SEEN */, description.toJson.compactPrint,
      balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint, isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)

  def toTxInfo(rc: RichCursor): TxInfo =
    TxInfo(rc string TxTable.rawTx, rc string TxTable.txid, rc long TxTable.depth, Satoshi(rc long TxTable.receivedSat),
      Satoshi(rc long TxTable.sentSat), Satoshi(rc long TxTable.feeSat), rc long TxTable.firstSeen, rc string TxTable.description,
      MilliSatoshi(rc long TxTable.balanceMsat), rc string TxTable.fiatRates, rc long TxTable.incoming, rc long TxTable.doubleSpent)
}