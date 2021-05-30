package immortan.sqlite

import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import java.lang.{Long => JLong}
import scala.util.Try


case class ChannelTxFeesSummary(fees: Satoshi, count: Long)

class SQLiteChannelTxFees(db: DBInterface) {
  def add(feePaid: Satoshi, txid: ByteVector32): Unit =
    db.change(ChannelTxFeesTable.newSql, txid.toHex, feePaid.toLong: JLong)

  def txSummary: Try[ChannelTxFeesSummary] = db.select(ChannelTxFeesTable.selectSummarySql).headTry { rc =>
    ChannelTxFeesSummary(fees = Satoshi(rc long 0), count = rc long 1)
  }
}
