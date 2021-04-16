package immortan

import immortan.utils.ImplicitJsonFormats._
import immortan.crypto.Tools.{Bytes, Fiat2Btc}
import fr.acinq.eclair.channel.{DATA_CLOSING, DATA_NEGOTIATING, HasNormalCommitments}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import fr.acinq.eclair.payment.PaymentRequest
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.MilliSatoshi
import scodec.bits.ByteVector
import immortan.utils.uri.Uri
import immortan.utils.LNUrl


object PaymentInfo {
  final val SENDABLE = 0
  final val NOT_SENDABLE_CHAIN_DISCONNECT = 1
  final val NOT_SENDABLE_LOW_FUNDS = 2
  final val NOT_SENDABLE_IN_FLIGHT = 3
  final val NOT_SENDABLE_SUCCESS = 4
}

object PaymentStatus {
  final val INIT = "state-init"
  final val PENDING = "state-pending"
  final val ABORTED = "state-aborted"
  final val SUCCEEDED = "state-succeeded"
}

sealed trait TransactionDetails {
  val seenAt: Long
}

case class PaymentInfo(prString: String, preimage: ByteVector32, status: String, seenAt: Long, descriptionString: String,
                       actionString: String, paymentHash: ByteVector32, paymentSecret: ByteVector32, received: MilliSatoshi,
                       sent: MilliSatoshi, fee: MilliSatoshi, balanceSnapshot: MilliSatoshi, fiatRatesString: String,
                       chainFee: MilliSatoshi, incoming: Long) extends TransactionDetails {

  val isIncoming: Boolean = 1 == incoming
  val tag: Int = if (isIncoming) PaymentTagTlv.FINAL_INCOMING else PaymentTagTlv.LOCALLY_SENT
  val fullTag: FullPaymentTag = FullPaymentTag(paymentHash, paymentSecret, tag)

  lazy val description: PaymentDescription = to[PaymentDescription](descriptionString)
  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
  lazy val action: PaymentAction = to[PaymentAction](actionString)
  lazy val pr: PaymentRequest = PaymentRequest.read(prString)
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
  val invoiceText: String
  val queryText: String
}

case class PlainDescription(invoiceText: String) extends PaymentDescription {
  val queryText: String = invoiceText
}

case class PlainMetaDescription(invoiceText: String, meta: String) extends PaymentDescription {
  val queryText: String = s"$invoiceText $meta"
}

case class SwapInDescription(invoiceText: String, txid: String, nodeId: PublicKey) extends PaymentDescription {
  val queryText: String = s"$invoiceText $txid ${nodeId.toString}"
}

case class SwapOutDescription(invoiceText: String, btcAddress: String, chainFee: Satoshi, nodeId: PublicKey) extends PaymentDescription {
  val queryText: String = s"$invoiceText $btcAddress ${nodeId.toString}"
}

// Relayed preimages

case class RelayedPreimageInfo(paymentHashString: String, preimageString: String, relayed: MilliSatoshi,
                               earned: MilliSatoshi, seenAt: Long) extends TransactionDetails {

  lazy val paymentHash: ByteVector32 = ByteVector32.fromValidHex(paymentHashString)
  lazy val preimage: ByteVector32 = ByteVector32.fromValidHex(preimageString)
}

// Tx descriptions

case class TxInfo(txString: String, txidString: String, depth: Long, receivedMsat: MilliSatoshi, sentMsat: MilliSatoshi,
                  feeMsat: MilliSatoshi, seenAt: Long, descriptionString: String, balanceSnapshot: MilliSatoshi,
                  fiatRatesString: String, incoming: Long, doubleSpent: Long) extends TransactionDetails {

  val isIncoming: Boolean = 1L == incoming
  val isDoubleSpent: Boolean = 1L == doubleSpent
  val isConfirmed: Boolean = depth >= LNParams.minDepthBlocks
  lazy val fiatRateSnapshot: Fiat2Btc = to[Fiat2Btc](fiatRatesString)
  lazy val description: TxDescription = to[TxDescription](descriptionString)
  lazy val txid: ByteVector32 = ByteVector32.fromValidHex(txidString)
}

sealed trait TxDescription

case class PlainTxDescription(label: Option[String] = None) extends TxDescription

sealed trait ChanTxDescription extends TxDescription { def nodeId: PublicKey }

case class OpReturnTxDescription(nodeId: PublicKey, preimage: ByteVector32) extends ChanTxDescription

case class ChanFundingTxDescription(nodeId: PublicKey) extends ChanTxDescription

case class ChanRefundingTxDescription(nodeId: PublicKey) extends ChanTxDescription

case class CommitClaimTxDescription(nodeId: PublicKey) extends ChanTxDescription

case class HtlcClaimTxDescription(nodeId: PublicKey) extends ChanTxDescription

case class PenaltyTxDescription(nodeId: PublicKey) extends ChanTxDescription

object TxDescription {
  def defineDescription(chans: Iterable[Channel], tx: Transaction): TxDescription =
    defineChannelRelation(chans, tx) getOrElse PlainTxDescription(None)

  def defineChannelRelation(chans: Iterable[Channel], tx: Transaction): Option[TxDescription] = chans.map(_.data).collectFirst {
    case hasCommits: HasNormalCommitments if hasCommits.commitments.commitInput.outPoint.txid == tx.txid => ChanFundingTxDescription(hasCommits.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.revokedCommitPublished.flatMap(_.penaltyTxs).exists(_.txid == tx.txid) => PenaltyTxDescription(closing.commitments.remoteInfo.nodeId)
    case negs: DATA_NEGOTIATING if negs.closingTxProposed.flatten.exists(_.unsignedTx.txid == tx.txid) => ChanRefundingTxDescription(negs.commitments.remoteInfo.nodeId)
    case negs: DATA_NEGOTIATING if negs.bestUnpublishedClosingTxOpt.exists(_.txid == tx.txid) => ChanRefundingTxDescription(negs.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.mutualCloseProposed.exists(_.txid == tx.txid) => ChanRefundingTxDescription(closing.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.secondTierTxs.exists(_.txid == tx.txid) => HtlcClaimTxDescription(closing.commitments.remoteInfo.nodeId)
    case closing: DATA_CLOSING if closing.commitTxes.exists(_.txid == tx.txid) => CommitClaimTxDescription(closing.commitments.remoteInfo.nodeId)
  }
}