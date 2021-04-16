package immortan.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import immortan.{TxDescription, TxInfo}
import java.lang.{Long => JLong}

import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi
import scala.util.Try


case class TxSummary(fees: MilliSatoshi, received: MilliSatoshi, sent: MilliSatoshi, count: Long)

class SQLiteTx(db: DBInterface) {
  def updConfidence(txid: ByteVector32, depth: Long): Unit = db.change(TxTable.updDepthSql, txid.toHex, depth: JLong)

  def updDoubleSpent(txid: ByteVector32): Unit = db.change(TxTable.updDoubleSpentSql, txid.toHex, 1L: JLong)

  def listRecentTxs(limit: Int): RichCursor = db.select(TxTable.selectRecentSql, limit.toString)

  def txSummary: Try[TxSummary] = db.select(TxTable.selectSummarySql).headTry { rc =>
    TxSummary(fees = MilliSatoshi(rc long 0), received = MilliSatoshi(rc long 1), sent = MilliSatoshi(rc long 2), count = rc long 3)
  }

  def putTx(event: TransactionReceived, description: TxDescription, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit =
    db.change(TxTable.newSql, event.tx.toString, event.tx.txid.toHex, event.depth: JLong, event.received.toLong: JLong, event.sent.toLong: JLong,
      event.feeOpt.map(_.toLong: JLong).getOrElse(0L: JLong), System.currentTimeMillis: JLong /* FIRST SEEN */, description.toJson.compactPrint,
      balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint, if (event.received >= event.sent) 1L: JLong else 0L: JLong /* INCOMING? */,
      0L: JLong /* NOT DOUBLE SPENT YET */)

  def toTxInfo(rc: RichCursor): TxInfo =
    TxInfo(rc string TxTable.rawTx, rc string TxTable.txid, rc long TxTable.depth, MilliSatoshi(rc long TxTable.receivedMsat),
      MilliSatoshi(rc long TxTable.sentMsat), MilliSatoshi(rc long TxTable.feeMsat), rc long TxTable.firstSeen, rc string TxTable.description,
      MilliSatoshi(rc long TxTable.balanceMsat), rc string TxTable.fiatRates, rc long TxTable.incoming, rc long TxTable.doubleSpent)
}