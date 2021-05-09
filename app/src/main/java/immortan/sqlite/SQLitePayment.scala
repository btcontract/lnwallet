package immortan.sqlite

import immortan._
import spray.json._
import fr.acinq.eclair._
import immortan.utils.ImplicitJsonFormats._
import java.lang.{Long => JLong}

import fr.acinq.eclair.transactions.RemoteFulfill
import fr.acinq.eclair.payment.PaymentRequest
import fr.acinq.eclair.wire.FullPaymentTag
import immortan.utils.PaymentRequestExt
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.bitcoin.ByteVector32
import scala.util.Try


case class RelaySummary(relayed: MilliSatoshi, earned: MilliSatoshi, count: Long)

case class PaymentSummary(fees: MilliSatoshi, received: MilliSatoshi, sent: MilliSatoshi, count: Long)

class SQLitePayment(db: DBInterface, preimageDb: DBInterface) extends PaymentBag {
  def getPaymentInfo(paymentHash: ByteVector32): Try[PaymentInfo] = db.select(PaymentTable.selectByHashSql, paymentHash.toHex).headTry(toPaymentInfo)

  def getPreimage(hash: ByteVector32): Try[ByteVector32] = preimageDb.select(PreimageTable.selectByHashSql, hash.toHex).headTry(_ string PreimageTable.preimage).map(ByteVector32.fromValidHex)

  def addSearchablePayment(search: String, paymentHash: ByteVector32): Unit = db.change(PaymentTable.newVirtualSql, search, paymentHash.toHex)

  def searchPayments(rawSearchQuery: String): RichCursor = db.search(PaymentTable.searchSql, rawSearchQuery)

  def listRecentPayments(limit: Int): RichCursor = {
    val failedHidingThreshold = System.currentTimeMillis - 60 * 60 * 24 * 1000L
    val expiryHidingThreshold = System.currentTimeMillis - PaymentRequest.OUR_EXPIRY_SECONDS * 1000L
    db.select(PaymentTable.selectRecentSql, expiryHidingThreshold.toString, failedHidingThreshold.toString, limit.toString)
  }

  def listRecentRelays(limit: Int): RichCursor = db.select(RelayTable.selectRecentSql, limit.toString)

  def setPreimage(paymentHash: ByteVector32, preimage: ByteVector32): Unit = preimageDb.change(PreimageTable.newSql, paymentHash.toHex, preimage.toHex)

  def updAbortedOutgoing(paymentHash: ByteVector32): Unit = db.change(PaymentTable.updStatusSql, PaymentStatus.ABORTED, paymentHash.toHex)

  def updOkOutgoing(fulfill: RemoteFulfill, fee: MilliSatoshi): Unit = {
    db.change(PaymentTable.updOkOutgoingSql, fulfill.preimage.toHex, fee.toLong: JLong, fulfill.ourAdd.paymentHash.toHex)
    ChannelMaster.preimageObtainedStream.onNext(fulfill.ourAdd.paymentHash)
  }

  def updOkIncoming(receivedAmount: MilliSatoshi, paymentHash: ByteVector32): Unit = {
    db.change(PaymentTable.updOkIncomingSql, receivedAmount.toLong: JLong, System.currentTimeMillis: JLong, paymentHash.toHex)
    ChannelMaster.preimageRevealedStream.onNext(paymentHash)
  }

  def addRelayedPreimageInfo(fullTag: FullPaymentTag, preimage: ByteVector32, relayed: MilliSatoshi, earned: MilliSatoshi): Unit =
    db.change(RelayTable.newSql, fullTag.paymentHash.toHex, fullTag.paymentSecret.toHex, preimage.toHex, System.currentTimeMillis: JLong, relayed.toLong: JLong, earned.toLong: JLong)

  def replaceOutgoingPayment(prex: PaymentRequestExt, description: PaymentDescription, action: Option[PaymentAction],
                             finalAmount: MilliSatoshi, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, chainFee: MilliSatoshi): Unit =
    db txWrap {
      db.change(PaymentTable.deleteSql, prex.pr.paymentHash.toHex)
      db.change(PaymentTable.newSql, prex.raw, ChannelMaster.NO_PREIMAGE.toHex, PaymentStatus.PENDING, System.currentTimeMillis: JLong, description.toJson.compactPrint,
        action.map(_.toJson.compactPrint).getOrElse(new String), prex.pr.paymentHash.toHex, prex.pr.paymentSecret.get.toHex, 0L: JLong /* RECEIVED = 0 MSAT */,
        finalAmount.toLong: JLong /* SENT IS KNOWN */, 0L: JLong /* FEE IS UNCERTAIN YET */, balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint,
        chainFee.toLong: JLong, 0: java.lang.Integer /* INCOMING = 0 */)
    }

  def replaceIncomingPayment(prex: PaymentRequestExt, preimage: ByteVector32, description: PaymentDescription,
                             balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, chainFee: MilliSatoshi): Unit =
    db txWrap {
      db.change(PaymentTable.deleteSql, prex.pr.paymentHash.toHex)
      db.change(PaymentTable.newSql, prex.raw, preimage.toHex, PaymentStatus.PENDING, System.currentTimeMillis: JLong, description.toJson.compactPrint, new String /* NO ACTION */,
        prex.pr.paymentHash.toHex, prex.pr.paymentSecret.get.toHex, prex.pr.amount.getOrElse(0L.msat).toLong: JLong /* MUST COME FROM PR! NO EXACT AMOUNT IF RECEIVED = 0 */,
        0L: JLong /* SENT = 0 MSAT, NOTHING TO SEND */, 0L: JLong /* NO FEE FOR INCOMING PAYMENT */, balanceSnap.toLong: JLong, fiatRateSnap.toJson.compactPrint,
        chainFee.toLong: JLong, 1: java.lang.Integer /* INCOMING = 1 */)
    }

  def paymentSummary: Try[PaymentSummary] = db.select(PaymentTable.selectSummarySql).headTry { rc =>
    PaymentSummary(fees = MilliSatoshi(rc long 0), received = MilliSatoshi(rc long 1), sent = MilliSatoshi(rc long 2), count = rc long 3)
  }

  def relaySummary: Try[RelaySummary] = db.select(RelayTable.selectSummarySql).headTry { rc =>
    RelaySummary(relayed = MilliSatoshi(rc long 0), earned = MilliSatoshi(rc long 1), count = rc long 2)
  }

  def toPaymentInfo(rc: RichCursor): PaymentInfo =
    PaymentInfo(rc string PaymentTable.pr, ByteVector32.fromValidHex(rc string PaymentTable.preimage), rc string PaymentTable.status, rc long PaymentTable.seenAt,
      rc string PaymentTable.description, rc string PaymentTable.action, ByteVector32.fromValidHex(rc string PaymentTable.hash), ByteVector32.fromValidHex(rc string PaymentTable.secret),
      MilliSatoshi(rc long PaymentTable.receivedMsat), MilliSatoshi(rc long PaymentTable.sentMsat), MilliSatoshi(rc long PaymentTable.feeMsat), MilliSatoshi(rc long PaymentTable.balanceMsat),
      rc string PaymentTable.fiatRates, MilliSatoshi(rc long PaymentTable.chainFee), rc long PaymentTable.incoming)

  def toRelayedPreimageInfo(rc: RichCursor): RelayedPreimageInfo =
    RelayedPreimageInfo(rc string RelayTable.hash, rc string RelayTable.preimage,
      MilliSatoshi(rc long RelayTable.relayed), MilliSatoshi(rc long RelayTable.earned),
      rc long RelayTable.seenAt)
}