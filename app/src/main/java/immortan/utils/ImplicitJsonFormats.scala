package immortan.utils

import immortan._
import spray.json._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import immortan.sqlite.{PaymentSummary, RelaySummary, TxSummary}
import immortan.utils.FiatRates.{BitpayItemList, CoinGeckoItemMap}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import scodec.bits.BitVector


object ImplicitJsonFormats extends DefaultJsonProtocol {
  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  val json2String: JsValue => String = (_: JsValue).convertTo[String]
  final val TAG = "tag"

  def writeExt[T](ext: (String, JsValue), base: JsValue): JsObject = JsObject(base.asJsObject.fields + ext)

  def taggedJsonFmt[T](base: JsonFormat[T], tag: String): JsonFormat[T] = new JsonFormat[T] {
    def write(unserialized: T): JsValue = writeExt(TAG -> JsString(tag), base write unserialized)
    def read(serialized: JsValue): T = base read serialized
  }

  def json2BitVec(json: JsValue): Option[BitVector] = BitVector fromHex json2String(json)

  def sCodecJsonFmt[T](codec: scodec.Codec[T] = null): JsonFormat[T] = new JsonFormat[T] {
    def read(serialized: JsValue): T = codec.decode(json2BitVec(serialized).get).require.value
    def write(unserialized: T): JsValue = codec.encode(unserialized).require.toHex.toJson
  }

  implicit val publicKeyFmt: JsonFormat[PublicKey] = sCodecJsonFmt(publicKey)

  implicit val milliSatoshiFmt: JsonFormat[MilliSatoshi] = sCodecJsonFmt(millisatoshi)

  implicit val byteVector32Fmt: JsonFormat[ByteVector32] = sCodecJsonFmt(bytes32)

  implicit val satoshiFmt: JsonFormat[Satoshi] = sCodecJsonFmt(satoshi)

  // Tx description

  implicit object TxDescriptionFmt extends JsonFormat[TxDescription] {
    def write(internal: TxDescription): JsValue = internal match {
      case paymentDescription: PlainTxDescription => paymentDescription.toJson
      case paymentDescription: ChanFundingTxDescription => paymentDescription.toJson
      case paymentDescription: CommitClaimTxDescription => paymentDescription.toJson
      case paymentDescription: HtlcClaimTxDescription => paymentDescription.toJson
      case paymentDescription: PenaltyTxDescription => paymentDescription.toJson
      case _ => throw new Exception
    }

    def read(raw: JsValue): TxDescription = raw.asJsObject.fields(TAG) match {
      case JsString("PlainTxDescription") => raw.convertTo[PlainTxDescription]
      case JsString("ChanFundingTxDescription") => raw.convertTo[ChanFundingTxDescription]
      case JsString("CommitClaimTxDescription") => raw.convertTo[CommitClaimTxDescription]
      case JsString("HtlcClaimTxDescription") => raw.convertTo[HtlcClaimTxDescription]
      case JsString("PenaltyTxDescription") => raw.convertTo[PenaltyTxDescription]
      case _ => throw new Exception
    }
  }

  implicit val plainTxDescriptionFmt: JsonFormat[PlainTxDescription] = taggedJsonFmt(jsonFormat[String,
    PlainTxDescription](PlainTxDescription.apply, "txid"), tag = "PlainTxDescription")

  implicit val chanFundingTxDescriptionFmt: JsonFormat[ChanFundingTxDescription] = taggedJsonFmt(jsonFormat[String, String, Long,
    ChanFundingTxDescription](ChanFundingTxDescription.apply, "txid", "nodeId", "sid"), tag = "ChanFundingTxDescription")

  implicit val commitClaimTxDescriptionFmt: JsonFormat[CommitClaimTxDescription] = taggedJsonFmt(jsonFormat[String, String, Long,
    CommitClaimTxDescription](CommitClaimTxDescription.apply, "txid", "nodeId", "sid"), tag = "CommitClaimTxDescription")

  implicit val htlcClaimTxDescriptionFmt: JsonFormat[HtlcClaimTxDescription] = taggedJsonFmt(jsonFormat[String, String, Long,
    HtlcClaimTxDescription](HtlcClaimTxDescription.apply, "txid", "nodeId", "sid"), tag = "HtlcClaimTxDescription")

  implicit val penaltyTxDescriptionFmt: JsonFormat[PenaltyTxDescription] = taggedJsonFmt(jsonFormat[String, String, Long,
    PenaltyTxDescription](PenaltyTxDescription.apply, "txid", "nodeId", "sid"), tag = "PenaltyTxDescription")

  // Last seen ready event

  implicit val walletReadyFmt: JsonFormat[WalletReady] = jsonFormat[Satoshi, Satoshi, Long, Long, WalletReady](WalletReady.apply, "confirmedBalance", "unconfirmedBalance", "height", "timestamp")

  // Payment description

  implicit object PaymentDescriptionFmt extends JsonFormat[PaymentDescription] {
    def write(internal: PaymentDescription): JsValue = internal match {
      case paymentDescription: PlainDescription => paymentDescription.toJson
      case paymentDescription: PlainMetaDescription => paymentDescription.toJson
      case paymentDescription: SwapInDescription => paymentDescription.toJson
      case paymentDescription: SwapOutDescription => paymentDescription.toJson
      case _ => throw new Exception
    }

    def read(raw: JsValue): PaymentDescription = raw.asJsObject.fields(TAG) match {
      case JsString("PlainDescription") => raw.convertTo[PlainDescription]
      case JsString("PlainMetaDescription") => raw.convertTo[PlainMetaDescription]
      case JsString("SwapInDescription") => raw.convertTo[SwapInDescription]
      case JsString("SwapOutDescription") => raw.convertTo[SwapOutDescription]
      case _ => throw new Exception
    }
  }

  implicit val plainDescriptionFmt: JsonFormat[PlainDescription] = taggedJsonFmt(jsonFormat[String,
    PlainDescription](PlainDescription.apply, "invoiceText"), tag = "PlainDescription")

  implicit val plainMetaDescriptionFmt: JsonFormat[PlainMetaDescription] = taggedJsonFmt(jsonFormat[String, String,
    PlainMetaDescription](PlainMetaDescription.apply, "invoiceText", "meta"), tag = "PlainMetaDescription")

  implicit val swapInDescriptionFmt: JsonFormat[SwapInDescription] = taggedJsonFmt(jsonFormat[String, String, Long, PublicKey,
    SwapInDescription](SwapInDescription.apply, "invoiceText", "txid", "internalId", "nodeId"), tag = "SwapInDescription")

  implicit val swapOutDescriptionFmt: JsonFormat[SwapOutDescription] = taggedJsonFmt(jsonFormat[String, String, Satoshi, PublicKey,
    SwapOutDescription](SwapOutDescription.apply, "invoiceText", "btcAddress", "chainFee", "nodeId"), tag = "SwapOutDescription")

  // Payment action

  implicit object PaymentActionFmt extends JsonFormat[PaymentAction] {
    def write(internal: PaymentAction): JsValue = internal match {
      case paymentAction: MessageAction => paymentAction.toJson
      case paymentAction: UrlAction => paymentAction.toJson
      case paymentAction: AESAction => paymentAction.toJson
      case _ => throw new Exception
    }

    def read(raw: JsValue): PaymentAction = raw.asJsObject.fields(TAG) match {
      case JsString("message") => raw.convertTo[MessageAction]
      case JsString("aes") => raw.convertTo[AESAction]
      case JsString("url") => raw.convertTo[UrlAction]
      case _ => throw new Exception
    }
  }

  implicit val aesActionFmt: JsonFormat[AESAction] = taggedJsonFmt(jsonFormat[Option[String], String, String, String, AESAction](AESAction.apply, "domain", "description", "ciphertext", "iv"), tag = "aes")

  implicit val messageActionFmt: JsonFormat[MessageAction] = taggedJsonFmt(jsonFormat[Option[String], String, MessageAction](MessageAction.apply, "domain", "message"), tag = "message")

  implicit val urlActionFmt: JsonFormat[UrlAction] = taggedJsonFmt(jsonFormat[Option[String], String, String, UrlAction](UrlAction.apply, "domain", "description", "url"), tag = "url")

  // Summaries

  implicit val relaySummaryFmt: JsonFormat[RelaySummary] = jsonFormat[MilliSatoshi, MilliSatoshi, Long, RelaySummary](RelaySummary.apply, "relayed", "earned", "count")

  implicit val paymentSummaryFmt: JsonFormat[PaymentSummary] = jsonFormat[MilliSatoshi, MilliSatoshi, MilliSatoshi, Long, PaymentSummary](PaymentSummary.apply, "fees", "received", "sent", "count")

  implicit val txSummaryFmt: JsonFormat[TxSummary] = jsonFormat[MilliSatoshi, MilliSatoshi, MilliSatoshi, Long, TxSummary](TxSummary.apply, "fees", "received", "sent", "count")

  // LNURL

  implicit object LNUrlDataFmt extends JsonFormat[LNUrlData] {
    def write(unserialized: LNUrlData): JsValue = throw new RuntimeException
    def read(serialized: JsValue): LNUrlData = serialized.asJsObject fields TAG match {
      case JsString("hostedChannelRequest") => serialized.convertTo[HostedChannelRequest]
      case JsString("channelRequest") => serialized.convertTo[NormalChannelRequest]
      case JsString("withdrawRequest") => serialized.convertTo[WithdrawRequest]
      case JsString("payRequest") => serialized.convertTo[PayRequest]
      case _ => throw new Exception
    }
  }

  // Note: tag on these MUST start with lower case because it is defined that way on protocol level

  implicit val normalChannelRequestFmt: JsonFormat[NormalChannelRequest] = taggedJsonFmt(jsonFormat[String, String, String,
    NormalChannelRequest](NormalChannelRequest.apply, "uri", "callback", "k1"), tag = "channelRequest")

  implicit val hostedChannelRequestFmt: JsonFormat[HostedChannelRequest] = taggedJsonFmt(jsonFormat[String, Option[String], String,
    HostedChannelRequest](HostedChannelRequest.apply, "uri", "alias", "k1"), tag = "hostedChannelRequest")

  implicit val withdrawRequestFmt: JsonFormat[WithdrawRequest] = taggedJsonFmt(jsonFormat[String, String, Long, String, Option[Long],
    WithdrawRequest](WithdrawRequest.apply, "callback", "k1", "maxWithdrawable", "defaultDescription", "minWithdrawable"), tag = "withdrawRequest")

  implicit val payRequestFmt: JsonFormat[PayRequest] = taggedJsonFmt(jsonFormat[String, Long, Long, String, Option[Int],
    PayRequest](PayRequest.apply, "callback", "maxSendable", "minSendable", "metadata", "commentAllowed"), tag = "payRequest")

  implicit val payRequestFinalFmt: JsonFormat[PayRequestFinal] = jsonFormat[Option[PaymentAction], Option[Boolean], List[String], String,
    PayRequestFinal](PayRequestFinal.apply, "successAction", "disposable", "routes", "pr")

  // Fiat feerates

  implicit val fiatRatesInfoFmt: JsonFormat[FiatRatesInfo] = jsonFormat[Fiat2Btc, Fiat2Btc, Long, FiatRatesInfo](FiatRatesInfo.apply, "rates", "oldRates", "stamp")

  implicit val blockchainInfoItemFmt: JsonFormat[BlockchainInfoItem] = jsonFormat[Double, BlockchainInfoItem](BlockchainInfoItem.apply, "last")

  implicit val bitpayItemFmt: JsonFormat[BitpayItem] = jsonFormat[String, Double, BitpayItem](BitpayItem.apply, "code", "rate")

  implicit val coinGeckoItemFmt: JsonFormat[CoinGeckoItem] = jsonFormat[Double, CoinGeckoItem](CoinGeckoItem.apply, "value")

  implicit val coinGeckoFmt: JsonFormat[CoinGecko] = jsonFormat[CoinGeckoItemMap, CoinGecko](CoinGecko.apply, "rates")

  implicit val bitpayFmt: JsonFormat[Bitpay] = jsonFormat[BitpayItemList, Bitpay](Bitpay.apply, "data")

  // Chain feerates

  implicit val bitGoFeeRateStructureFmt: JsonFormat[BitGoFeeRateStructure] = jsonFormat[Map[String, Long], Long, BitGoFeeRateStructure](BitGoFeeRateStructure.apply, "feeByBlockTarget", "feePerKb")

  implicit val earnDotComFeeRateItemFmt: JsonFormat[EarnDotComFeeRateItem] = jsonFormat[Long, Long, Long, Long, Long, EarnDotComFeeRateItem](EarnDotComFeeRateItem.apply, "minFee", "maxFee", "memCount", "minDelay", "maxDelay")

  implicit val earnDotComFeeRateStructureFmt: JsonFormat[EarnDotComFeeRateStructure] = jsonFormat[List[EarnDotComFeeRateItem], EarnDotComFeeRateStructure](EarnDotComFeeRateStructure.apply, "fees")
}
