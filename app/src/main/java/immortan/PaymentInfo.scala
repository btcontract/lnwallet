package immortan

import immortan.utils.ImplicitJsonFormats._
import fr.acinq.eclair.channel.{DATA_CLOSING, DATA_NEGOTIATING, HasNormalCommitments}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import immortan.crypto.Tools.{Bytes, Fiat2Btc, SEPARATOR}
import immortan.utils.{LNUrl, PaymentRequestExt}
import immortan.fsm.IncomingPaymentProcessor
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.MilliSatoshi
import scodec.bits.ByteVector
import immortan.utils.uri.Uri
import java.util.Date
import scala.util.Try


object PaymentInfo {
  final val NOT_SENDABLE_CHAIN_DISCONNECT = 0
  final val NOT_SENDABLE_LOW_FUNDS = 1
  final val NOT_SENDABLE_IN_FLIGHT = 2
  final val NOT_SENDABLE_SUCCESS = 3
}

object PaymentStatus {
  final val INIT = "state-init"
  final val PENDING = "state-pending"
  final val ABORTED = "state-aborted"
  final val SUCCEEDED = "state-succeeded"
}

sealed trait TransactionDetails {
  lazy val date: Date = new Date(seenAt)
  val seenAt: Long
}

case class PaymentInfo(prString: String, preimage: ByteVector32, status: String, seenAt: Long, descriptionString: String,
                       actionString: String, paymentHash: ByteVector32, paymentSecret: ByteVector32, received: MilliSatoshi,
                       sent: MilliSatoshi, fee: MilliSatoshi, balanceSnapshot: MilliSatoshi, fiatRatesString: String,
                       chainFee: MilliSatoshi, incoming: Long) extends TransactionDetails {

  val isIncoming: Boolean = 1 == incoming
  val tag: Int = if (isIncoming) PaymentTagTlv.FINAL_INCOMING else PaymentTagTlv.LOCALLY_SENT
  val fullTag: FullPaymentTag = FullPaymentTag(paymentHash, paymentSecret, tag)

  lazy val prExt: PaymentRequestExt = PaymentRequestExt.fromRaw(prString)
  lazy val description: PaymentDescription = to[PaymentDescription](descriptionString)
  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
  lazy val action: PaymentAction = to[PaymentAction](actionString)

  def msatRatio(fsm: IncomingPaymentProcessor): Long = Try(fsm.lastAmountIn)
    .map(collected => received.toLong * 100D / collected.toLong)
    .map(receivedRatio => receivedRatio.toLong)
    .getOrElse(0L)
}

// Payment actions

sealed trait PaymentAction {
  val domain: Option[String]
  val finalMessage: String
}

case class MessageAction(domain: Option[String], message: String) extends PaymentAction {
  val finalMessage = s"<br>${message take 144}"
}

case class UrlAction(domain: Option[String], description: String, url: String) extends PaymentAction {
  val finalMessage = s"<br>${description take 144}<br><br><font color=#0000FF><tt>$url</tt></font><br>"
  require(domain.forall(url.contains), "Payment action domain mismatch")
  val uri: Uri = LNUrl.checkHost(url)
}

case class AESAction(domain: Option[String], description: String, ciphertext: String, iv: String) extends PaymentAction {
  val ciphertextBytes: Bytes = ByteVector.fromValidBase64(ciphertext).take(1024 * 4).toArray // up to ~2kb of encrypted data
  val ivBytes: Bytes = ByteVector.fromValidBase64(iv).take(24).toArray // 16 bytes
  val finalMessage = s"<br>${description take 144}"
}

// Payment descriptions

sealed trait PaymentDescription {
  val desc: Option[String]
  val invoiceText: String
  val queryText: String
}

case class PlainDescription(invoiceText: String) extends PaymentDescription {
  val desc: Option[String] = Some(invoiceText).filterNot(_.isEmpty)
  val queryText: String = invoiceText
}

case class PlainMetaDescription(invoiceText: String, meta: String) extends PaymentDescription {
  val desc: Option[String] = Some(meta).filterNot(_.isEmpty) orElse Some(invoiceText).filterNot(_.isEmpty)
  val queryText: String = s"$invoiceText $meta"
}

// Relayed preimages

case class RelayedPreimageInfo(paymentHashString: String, paymentSecretString: String, preimageString: String,
                               relayed: MilliSatoshi, earned: MilliSatoshi, seenAt: Long) extends TransactionDetails {

  lazy val preimage: ByteVector32 = ByteVector32.fromValidHex(preimageString)
  lazy val paymentHash: ByteVector32 = ByteVector32.fromValidHex(paymentHashString)
  lazy val paymentSecret: ByteVector32 = ByteVector32.fromValidHex(paymentSecretString)
  lazy val fullTag: FullPaymentTag = FullPaymentTag(paymentHash, paymentSecret, PaymentTagTlv.TRAMPLOINE_ROUTED)
}

// Tx descriptions

case class TxInfo(txString: String, txidString: String, depth: Long, receivedSat: Satoshi, sentSat: Satoshi, feeSat: Satoshi,
                  seenAt: Long, descriptionString: String, balanceSnapshot: MilliSatoshi, fiatRatesString: String,
                  incoming: Long, doubleSpent: Long) extends TransactionDetails {

  val isIncoming: Boolean = 1L == incoming
  val isDoubleSpent: Boolean = 1L == doubleSpent
  val isDeeplyBuried: Boolean = depth >= LNParams.minDepthBlocks
  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
  lazy val description: TxDescription = to[TxDescription](descriptionString)
  lazy val txid: ByteVector32 = ByteVector32.fromValidHex(txidString)
  lazy val tx: Transaction = Transaction.read(txString)
}

sealed trait TxDescription {
  def queryText(txid: ByteVector32): String
}

case class PlainTxDescription(addresses: List[String], label: Option[String] = None) extends TxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + addresses.mkString(SEPARATOR) + SEPARATOR + label.getOrElse(new String)
}

sealed trait ChanTxDescription extends TxDescription {
  def nodeId: PublicKey
}

case class OpReturnTxDescription(nodeId: PublicKey, preimage: ByteVector32) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString + SEPARATOR + preimage.toHex
}

case class ChanFundingTxDescription(nodeId: PublicKey) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString
}

case class ChanRefundingTxDescription(nodeId: PublicKey) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString
}

case class HtlcClaimTxDescription(nodeId: PublicKey) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString
}

case class PenaltyTxDescription(nodeId: PublicKey) extends ChanTxDescription {
  def queryText(txid: ByteVector32): String = txid.toHex + SEPARATOR + nodeId.toString
}

object TxDescription {
  def define(chans: Iterable[Channel], walletAddresses: List[String], tx: Transaction): TxDescription =
    defineChannelRelation(chans, tx) getOrElse PlainTxDescription(walletAddresses)

  def defineChannelRelation(chans: Iterable[Channel], tx: Transaction): Option[TxDescription] = chans.map(_.data).collectFirst {
    case hasCommits: HasNormalCommitments if hasCommits.commitments.commitInput.outPoint.txid == tx.txid => ChanFundingTxDescription(hasCommits.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.revokedCommitPublished.flatMap(_.penaltyTxs).exists(_.txid == tx.txid) => PenaltyTxDescription(closing.commitments.remoteInfo.nodeId)
    case negs: DATA_NEGOTIATING if negs.closingTxProposed.flatten.exists(_.unsignedTx.txid == tx.txid) => ChanRefundingTxDescription(negs.commitments.remoteInfo.nodeId)
    case negs: DATA_NEGOTIATING if negs.bestUnpublishedClosingTxOpt.exists(_.txid == tx.txid) => ChanRefundingTxDescription(negs.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.balanceLeftoverRefunds.exists(_.txid == tx.txid) => ChanRefundingTxDescription(closing.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.paymentLeftoverRefunds.exists(_.txid == tx.txid) => HtlcClaimTxDescription(closing.commitments.remoteInfo.nodeId)
  }
}