package immortan.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import immortan.{TxDescription, TxInfo}
import java.lang.{Long => JLong}

import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import scala.util.Try


case class TxSummary(fees: MilliSatoshi, received: MilliSatoshi, sent: MilliSatoshi, count: Long)

class SQLiteTx(db: DBInterface) {
  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)

  def addSearchableTransaction(search: String, txid: ByteVector32): Unit = db.change(TxTable.newVirtualSql, search, txid.toHex)

  def searchTransactions(rawSearchQuery: String): RichCursor = db.search(TxTable.searchSql, rawSearchQuery)

  def updStatus(txid: ByteVector32, depth: Long, isDoubleSpent: Boolean): Unit = db.change(TxTable.updStatusSql, depth: JLong, if (isDoubleSpent) 1L: JLong else 0L: JLong, txid.toHex)

  def txSummary: Try[TxSummary] = db.select(TxTable.selectSummarySql).headTry { rc =>
    TxSummary(fees = MilliSatoshi(rc long 0), received = MilliSatoshi(rc long 1), sent = MilliSatoshi(rc long 2), count = rc long 3)
  }

  def replaceTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, feeOpt: Option[Satoshi],
                description: TxDescription, isIncoming: Long, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit =
    db txWrap {
      db.change(TxTable.deleteSql, tx.txid.toHex)
      db.change(TxTable.newSql, tx.toString, tx.txid.toHex, depth: JLong, received.toLong: JLong, sent.toLong: JLong,
        feeOpt.map(_.toLong: JLong).getOrElse(0L: JLong), System.currentTimeMillis: JLong, description.toJson.compactPrint,
        balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint, isIncoming: JLong, 0L: JLong /* NOT DOUBLE SPENT YET */)
    }

  def toTxInfo(rc: RichCursor): TxInfo =
    TxInfo(rc string TxTable.rawTx, rc string TxTable.txid, rc long TxTable.depth, Satoshi(rc long TxTable.receivedSat),
      Satoshi(rc long TxTable.sentSat), Satoshi(rc long TxTable.feeSat), rc long TxTable.firstSeen, rc string TxTable.description,
      MilliSatoshi(rc long TxTable.balanceMsat), rc string TxTable.fiatRates, rc long TxTable.incoming, rc long TxTable.doubleSpent)
}