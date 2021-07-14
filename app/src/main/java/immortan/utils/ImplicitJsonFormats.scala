package immortan.utils

import immortan._
import spray.json._
import fr.acinq.eclair.blockchain.fee._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.LightningMessageCodecs._

import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import immortan.utils.PayRequest.{AdditionalRoute, TagAndContent}
import immortan.utils.FiatRates.{BitpayItemList, CoinGeckoItemMap}
import fr.acinq.eclair.blockchain.electrum.db.{ChainWalletInfo, SigningWallet, WatchingWallet}
import fr.acinq.eclair.wire.ChannelCodecs.extendedPublicKeyCodec
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.eclair.wire.ChannelUpdate
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import immortan.fsm.SplitInfo
import scodec.bits.BitVector


object ImplicitJsonFormats extends DefaultJsonProtocol {
  val json2String: JsValue => String = (_: JsValue).convertTo[String]

  final val TAG = "tag"

  def writeExt[T](ext: (String, JsValue), base: JsValue): JsObject = JsObject(base.asJsObject.fields + ext)

  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]

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

  implicit val byteVector32Fmt: JsonFormat[ByteVector32] = sCodecJsonFmt(bytes32)

  implicit val channelUpdateFmt: JsonFormat[ChannelUpdate] = sCodecJsonFmt(channelUpdateCodec)

  implicit val milliSatoshiFmt: JsonFormat[MilliSatoshi] = jsonFormat[Long, MilliSatoshi](MilliSatoshi.apply, "underlying")

  implicit val satoshiFmt: JsonFormat[Satoshi] = jsonFormat[Long, Satoshi](Satoshi.apply, "underlying")

  implicit val extendedPublicKeyFmt: JsonFormat[ExtendedPublicKey] = sCodecJsonFmt(extendedPublicKeyCodec)

  // Chain wallet types

  implicit object ChainWalletInfoFmt extends JsonFormat[ChainWalletInfo] {
    def read(raw: JsValue): ChainWalletInfo = raw.asJsObject.fields(TAG) match {
      case JsString("WatchingWallet") => raw.convertTo[WatchingWallet]
      case JsString("SigningWallet") => raw.convertTo[SigningWallet]
      case _ => throw new Exception
    }

    def write(internal: ChainWalletInfo): JsValue = internal match {
      case walletInfo: WatchingWallet => walletInfo.toJson
      case walletInfo: SigningWallet => walletInfo.toJson
      case _ => throw new Exception
    }
  }

  implicit val signingWalletFmt: JsonFormat[SigningWallet] = taggedJsonFmt(jsonFormat[String, Boolean,
    SigningWallet](SigningWallet.apply, "walletType", "isRemovable"), tag = "SigningWallet")

  implicit val watchingWalletFmt: JsonFormat[WatchingWallet] = taggedJsonFmt(jsonFormat[String, ExtendedPublicKey, Boolean,
    WatchingWallet](WatchingWallet.apply, "walletType", "xPub", "isRemovable"), tag = "WatchingWallet")

  // Tx description

  implicit object TxDescriptionFmt extends JsonFormat[TxDescription] {
    def read(raw: JsValue): TxDescription = raw.asJsObject.fields(TAG) match {
      case JsString("PlainTxDescription") => raw.convertTo[PlainTxDescription]
      case JsString("OpReturnTxDescription") => raw.convertTo[OpReturnTxDescription]
      case JsString("ChanFundingTxDescription") => raw.convertTo[ChanFundingTxDescription]
      case JsString("ChanRefundingTxDescription") => raw.convertTo[ChanRefundingTxDescription]
      case JsString("HtlcClaimTxDescription") => raw.convertTo[HtlcClaimTxDescription]
      case JsString("PenaltyTxDescription") => raw.convertTo[PenaltyTxDescription]
      case _ => throw new Exception
    }

    def write(internal: TxDescription): JsValue = internal match {
      case paymentDescription: PlainTxDescription => paymentDescription.toJson
      case paymentDescription: OpReturnTxDescription => paymentDescription.toJson
      case paymentDescription: ChanFundingTxDescription => paymentDescription.toJson
      case paymentDescription: ChanRefundingTxDescription => paymentDescription.toJson
      case paymentDescription: HtlcClaimTxDescription => paymentDescription.toJson
      case paymentDescription: PenaltyTxDescription => paymentDescription.toJson
      case _ => throw new Exception
    }
  }

  implicit val plainTxDescriptionFmt: JsonFormat[PlainTxDescription] = taggedJsonFmt(jsonFormat[List[String], Option[String],
    PlainTxDescription](PlainTxDescription.apply, "addresses", "label"), tag = "PlainTxDescription")

  implicit val opReturnTxDescriptionFmt: JsonFormat[OpReturnTxDescription] = taggedJsonFmt(jsonFormat[PublicKey, ByteVector32, Option[String],
    OpReturnTxDescription](OpReturnTxDescription.apply, "nodeId", "preimage", "label"), tag = "OpReturnTxDescription")

  implicit val chanFundingTxDescriptionFmt: JsonFormat[ChanFundingTxDescription] = taggedJsonFmt(jsonFormat[PublicKey, Option[String],
    ChanFundingTxDescription](ChanFundingTxDescription.apply, "nodeId", "label"), tag = "ChanFundingTxDescription")

  implicit val chanRefundingTxDescriptionFmt: JsonFormat[ChanRefundingTxDescription] = taggedJsonFmt(jsonFormat[PublicKey, Option[String],
    ChanRefundingTxDescription](ChanRefundingTxDescription.apply, "nodeId", "label"), tag = "ChanRefundingTxDescription")

  implicit val htlcClaimTxDescriptionFmt: JsonFormat[HtlcClaimTxDescription] = taggedJsonFmt(jsonFormat[PublicKey, Option[String],
    HtlcClaimTxDescription](HtlcClaimTxDescription.apply, "nodeId", "label"), tag = "HtlcClaimTxDescription")

  implicit val penaltyTxDescriptionFmt: JsonFormat[PenaltyTxDescription] = taggedJsonFmt(jsonFormat[PublicKey, Option[String],
    PenaltyTxDescription](PenaltyTxDescription.apply, "nodeId", "label"), tag = "PenaltyTxDescription")

  // Payment description

  implicit object PaymentDescriptionFmt extends JsonFormat[PaymentDescription] {
    def read(raw: JsValue): PaymentDescription = raw.asJsObject.fields(TAG) match {
      case JsString("PlainMetaDescription") => raw.convertTo[PlainMetaDescription]
      case JsString("PlainDescription") => raw.convertTo[PlainDescription]
      case _ => throw new Exception
    }

    def write(internal: PaymentDescription): JsValue = internal match {
      case paymentDescription: PlainMetaDescription => paymentDescription.toJson
      case paymentDescription: PlainDescription => paymentDescription.toJson
      case _ => throw new Exception
    }
  }

  implicit val splitInfoFmt: JsonFormat[SplitInfo] =
    jsonFormat[MilliSatoshi, MilliSatoshi, SplitInfo](SplitInfo.apply, "totalSum", "myPart")

  implicit val plainDescriptionFmt: JsonFormat[PlainDescription] = taggedJsonFmt(jsonFormat[Option[SplitInfo], Option[String], String,
    PlainDescription](PlainDescription.apply, "split", "label", "invoiceText"), tag = "PlainDescription")

  implicit val plainMetaDescriptionFmt: JsonFormat[PlainMetaDescription] = taggedJsonFmt(jsonFormat[Option[SplitInfo], Option[String], String, String,
    PlainMetaDescription](PlainMetaDescription.apply, "split", "label", "invoiceText", "meta"), tag = "PlainMetaDescription")

  // Payment action

  implicit object PaymentActionFmt extends JsonFormat[PaymentAction] {
    def read(raw: JsValue): PaymentAction = raw.asJsObject.fields(TAG) match {
      case JsString("message") => raw.convertTo[MessageAction]
      case JsString("aes") => raw.convertTo[AESAction]
      case JsString("url") => raw.convertTo[UrlAction]
      case _ => throw new Exception
    }

    def write(internal: PaymentAction): JsValue = internal match {
      case paymentAction: MessageAction => paymentAction.toJson
      case paymentAction: UrlAction => paymentAction.toJson
      case paymentAction: AESAction => paymentAction.toJson
      case _ => throw new Exception
    }
  }

  implicit val aesActionFmt: JsonFormat[AESAction] = taggedJsonFmt(jsonFormat[Option[String], String, String, String, AESAction](AESAction.apply, "domain", "description", "ciphertext", "iv"), tag = "aes")

  implicit val messageActionFmt: JsonFormat[MessageAction] = taggedJsonFmt(jsonFormat[Option[String], String, MessageAction](MessageAction.apply, "domain", "message"), tag = "message")

  implicit val urlActionFmt: JsonFormat[UrlAction] = taggedJsonFmt(jsonFormat[Option[String], String, String, UrlAction](UrlAction.apply, "domain", "description", "url"), tag = "url")

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

  implicit val payRequestFinalFmt: JsonFormat[PayRequestFinal] =
    jsonFormat[Option[PaymentAction], Option[Boolean], List[AdditionalRoute], String,
      PayRequestFinal](PayRequestFinal.apply, "successAction", "disposable", "routes", "pr")

  implicit val payRequestMetaFmt: JsonFormat[PayRequestMeta] = jsonFormat[List[TagAndContent], PayRequestMeta](PayRequestMeta.apply, "records")

  // Fiat feerates

  implicit val blockchainInfoItemFmt: JsonFormat[BlockchainInfoItem] = jsonFormat[Double, BlockchainInfoItem](BlockchainInfoItem.apply, "last")

  implicit val bitpayItemFmt: JsonFormat[BitpayItem] = jsonFormat[String, Double, BitpayItem](BitpayItem.apply, "code", "rate")

  implicit val coinGeckoItemFmt: JsonFormat[CoinGeckoItem] = jsonFormat[Double, CoinGeckoItem](CoinGeckoItem.apply, "value")

  implicit val coinGeckoFmt: JsonFormat[CoinGecko] = jsonFormat[CoinGeckoItemMap, CoinGecko](CoinGecko.apply, "rates")

  implicit val bitpayFmt: JsonFormat[Bitpay] = jsonFormat[BitpayItemList, Bitpay](Bitpay.apply, "data")

  implicit val fiatRatesInfoFmt: JsonFormat[FiatRatesInfo] = jsonFormat[Fiat2Btc, Fiat2Btc, Long, FiatRatesInfo](FiatRatesInfo.apply, "rates", "oldRates", "stamp")

  // Chain feerates

  implicit val bitGoFeeRateStructureFmt: JsonFormat[BitGoFeeRateStructure] =
    jsonFormat[Map[String, Long], Long, BitGoFeeRateStructure](BitGoFeeRateStructure.apply, "feeByBlockTarget", "feePerKb")

  implicit val earnDotComFeeRateItemFmt: JsonFormat[EarnDotComFeeRateItem] =
    jsonFormat[Long, Long, Long, Long, Long, EarnDotComFeeRateItem](EarnDotComFeeRateItem.apply, "minFee", "maxFee", "memCount", "minDelay", "maxDelay")

  implicit val earnDotComFeeRateStructureFmt: JsonFormat[EarnDotComFeeRateStructure] =
    jsonFormat[List[EarnDotComFeeRateItem], EarnDotComFeeRateStructure](EarnDotComFeeRateStructure.apply, "fees")

  implicit val feeratePerKBFmt: JsonFormat[FeeratePerKB] =
    jsonFormat[Satoshi, FeeratePerKB](FeeratePerKB.apply, "feerate")

  implicit val feeratesPerKBFmt: JsonFormat[FeeratesPerKB] =
    jsonFormat[FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB, FeeratePerKB,
      FeeratesPerKB](FeeratesPerKB.apply, "mempoolMinFee", "block_1", "blocks_2", "blocks_6", "blocks_12", "blocks_36", "blocks_72", "blocks_144", "blocks_1008")

  implicit val feeratePerKwFmt: JsonFormat[FeeratePerKw] =
    jsonFormat[Satoshi, FeeratePerKw](FeeratePerKw.apply, "feerate")

  implicit val feeratesPerKwFmt: JsonFormat[FeeratesPerKw] =
    jsonFormat[FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw, FeeratePerKw,
      FeeratesPerKw](FeeratesPerKw.apply, "mempoolMinFee", "block_1", "blocks_2", "blocks_6", "blocks_12", "blocks_36", "blocks_72", "blocks_144", "blocks_1008")

  implicit val feeRatesInfoFmt: JsonFormat[FeeRatesInfo] =
    jsonFormat[FeeratesPerKw, List[FeeratesPerKB], Long, FeeRatesInfo](FeeRatesInfo.apply, "smoothed", "history", "stamp")
}
