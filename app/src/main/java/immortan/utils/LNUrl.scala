package immortan.utils

import spray.json._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import immortan.utils.ImplicitJsonFormats._
import immortan.utils.PayRequest.{AdditionalRoute, PayMetaData}
import fr.acinq.eclair.router.{Announcements, RouteCalculation}
import immortan.{LNParams, PaymentAction, RemoteNodeInfo}
import fr.acinq.eclair.wire.{ChannelUpdate, NodeAddress}
import fr.acinq.bitcoin.{Bech32, ByteVector32, Crypto}
import fr.acinq.eclair.router.Graph.GraphStructure
import com.github.kevinsawicki.http.HttpRequest
import fr.acinq.eclair.payment.PaymentRequest
import fr.acinq.bitcoin.Crypto.PublicKey
import rx.lang.scala.Observable
import scodec.bits.ByteVector
import immortan.utils.uri.Uri
import scala.util.Try


object LNUrl {
  def fromBech32(bech32url: String): LNUrl = {
    val Tuple2(_, dataBody) = Bech32.decode(bech32url)
    val request = new String(Bech32.five2eight(dataBody), "UTF-8")
    LNUrl(request)
  }

  def checkHost(host: String): Uri = Uri.parse(host) match { case uri =>
    val isOnion = host.startsWith("http://") && uri.getHost.endsWith(NodeAddress.onionSuffix)
    val isSSLPlain = host.startsWith("https://") && !uri.getHost.endsWith(NodeAddress.onionSuffix)
    require(isSSLPlain || isOnion, "URI is neither Plain/HTTPS nor Onion/HTTP request")
    uri
  }

  def guardResponse(raw: String): String = {
    val validJson = Try(raw.parseJson.asJsObject.fields)
    val hasError = validJson.map(_ apply "reason").map(json2String)
    if (validJson.isFailure) throw new Exception(s"Invalid json from vendor: $raw")
    if (hasError.isSuccess) throw new Exception(s"Error message from vendor: ${hasError.get}")
    raw
  }

  def noRedirectGuardedGet(url: String): String =
    guardResponse(HttpRequest.get(url, false).connectTimeout(15000)
      .followRedirects(false).header("Connection", "close").body)

  def level2DataResponse(bld: Uri.Builder): Observable[String] = Rx.ioQueue.map { _ =>
    noRedirectGuardedGet(bld.build.toString)
  }
}

case class LNUrl(request: String) {
  val uri: Uri = LNUrl.checkHost(request)
  lazy val k1: Try[String] = Try(uri getQueryParameter "k1")
  lazy val isAuth: Boolean = Try(uri.getQueryParameter("tag").toLowerCase == "login").getOrElse(false)
  lazy val authAction: String = Try(uri.getQueryParameter("action").toLowerCase).getOrElse("login")

  lazy val fastWithdrawAttempt: Try[WithdrawRequest] = Try {
    require(uri getQueryParameter "tag" equals "withdrawRequest")
    WithdrawRequest(uri.getQueryParameter("callback"), uri.getQueryParameter("k1"),
      uri.getQueryParameter("maxWithdrawable").toLong, uri.getQueryParameter( "defaultDescription"),
      uri.getQueryParameter("minWithdrawable").toLong.asSome)
  }

  def level1DataResponse: Observable[LNUrlData] = Rx.ioQueue.map { _ =>
    val lnUrlData = to[LNUrlData](LNUrl noRedirectGuardedGet uri.toString)
    require(lnUrlData.checkAgainstParent(this), "1st/2nd level callback domain mismatch")
    lnUrlData
  }
}

sealed trait LNUrlData {
  def checkAgainstParent(lnUrl: LNUrl): Boolean = true
}

// LNURL-CHANNEL

sealed trait HasRemoteInfo {
  val remoteInfo: RemoteNodeInfo
  def cancel: Unit = none
}

case class HasRemoteInfoWrap(remoteInfo: RemoteNodeInfo) extends HasRemoteInfo

case class NormalChannelRequest(uri: String, callback: String, k1: String) extends LNUrlData with HasRemoteInfo {

  override def checkAgainstParent(lnUrl: LNUrl): Boolean = lnUrl.uri.getHost == callbackUri.getHost

  def requestChannel: Observable[String] = LNUrl.level2DataResponse {
    val withOurNodeId = callbackUri.buildUpon.appendQueryParameter("remoteid", remoteInfo.nodeSpecificPubKey.toString)
    withOurNodeId.appendQueryParameter("k1", k1).appendQueryParameter("private", "1")
  }

  override def cancel: Unit = LNUrl.level2DataResponse {
    val withOurNodeId = callbackUri.buildUpon.appendQueryParameter("remoteid", remoteInfo.nodeSpecificPubKey.toString)
    withOurNodeId.appendQueryParameter("k1", k1).appendQueryParameter("cancel", "1")
  }.foreach(none, none)

  val InputParser.nodeLink(nodeKey, hostAddress, portNumber) = uri

  val pubKey: PublicKey = PublicKey.fromBin(ByteVector fromValidHex nodeKey)

  val address: NodeAddress = NodeAddress.fromParts(hostAddress, portNumber.toInt)

  val remoteInfo: RemoteNodeInfo = RemoteNodeInfo(pubKey, address, hostAddress)

  val callbackUri: Uri = LNUrl.checkHost(callback)
}

case class HostedChannelRequest(uri: String, alias: Option[String], k1: String) extends LNUrlData with HasRemoteInfo {

  val secret: ByteVector32 = ByteVector32.fromValidHex(k1)

  val InputParser.nodeLink(nodeKey, hostAddress, portNumber) = uri

  val pubKey: PublicKey = PublicKey(ByteVector fromValidHex nodeKey)

  val address: NodeAddress = NodeAddress.fromParts(hostAddress, portNumber.toInt)

  val remoteInfo: RemoteNodeInfo = RemoteNodeInfo(pubKey, address, hostAddress)
}

// LNURL-WITHDRAW

case class WithdrawRequest(callback: String, k1: String, maxWithdrawable: Long, defaultDescription: String, minWithdrawable: Option[Long] = None) extends LNUrlData { me =>

  def requestWithdraw(ext: PaymentRequestExt): Observable[String] = LNUrl.level2DataResponse {
    callbackUri.buildUpon.appendQueryParameter("pr", ext.raw).appendQueryParameter("k1", k1)
  }

  override def checkAgainstParent(lnUrl: LNUrl): Boolean = lnUrl.uri.getHost == callbackUri.getHost

  val callbackUri: Uri = LNUrl.checkHost(callback)

  val minCanReceive: MilliSatoshi = minWithdrawable.map(_.msat).getOrElse(LNParams.minPayment).max(LNParams.minPayment)

  val descriptionOpt: Option[String] = Some(defaultDescription).map(trimmed).filter(_.nonEmpty)
  val brDescription: String = descriptionOpt.map(desc => s"<br><br>$desc").getOrElse(new String)
  val descriptionOrEmpty: String = descriptionOpt.getOrElse(new String)

  require(minCanReceive <= maxWithdrawable.msat, s"$maxWithdrawable is less than min $minCanReceive")
}

// LNURL-PAY

object PayRequest {
  type TagAndContent = List[String]
  type PayMetaData = List[TagAndContent]
  type KeyAndUpdate = (PublicKey, ChannelUpdate)
  type AdditionalRoute = List[KeyAndUpdate]

  def routeToHops(additionalRoute: AdditionalRoute = Nil): List[PaymentRequest.ExtraHop] = for {
    (startNodeId: PublicKey, channelUpdate: ChannelUpdate) <- additionalRoute
    signatureOk = Announcements.checkSig(channelUpdate)(startNodeId)
    _ = require(signatureOk, "Route contains an invalid update")
  } yield channelUpdate extraHop startNodeId
}

case class PayRequest(callback: String, maxSendable: Long, minSendable: Long, metadata: String, commentAllowed: Option[Int] = None) extends LNUrlData { me =>

  def requestFinal(comment: Option[String], amount: MilliSatoshi): Observable[String] = LNUrl.level2DataResponse {
    val base: Uri.Builder = callbackUri.buildUpon.appendQueryParameter("amount", amount.toLong.toString)
    comment match { case Some(text) => base.appendQueryParameter("comment", text) case _ => base }
  }

  override def checkAgainstParent(lnUrl: LNUrl): Boolean = lnUrl.uri.getHost == callbackUri.getHost

  def metaDataHash: ByteVector32 = Crypto.sha256(ByteVector view metadata.getBytes)

  val callbackUri: Uri = LNUrl.checkHost(callback)

  val decodedMetadata: PayMetaData = to[PayMetaData](metadata)

  val metaDataTexts: List[String] = decodedMetadata.collect { case List("text/plain", txt) => txt }

  require(metaDataTexts.size == 1, "There must be exactly one text/plain entry in metadata")

  require(minSendable <= maxSendable, s"max=$maxSendable while min=$minSendable")

  val metaDataTextPlain: String = trimmed(metaDataTexts.head)

  val metaDataImageBase64s: Seq[String] = for {
    List("image/png;base64" | "image/jpeg;base64", content) <- decodedMetadata
    _ = require(content.length <= 136536, s"Image is too heavy, base64 length=${content.length}")
  } yield content
}

case class PayRequestFinal(successAction: Option[PaymentAction], disposable: Option[Boolean], routes: List[AdditionalRoute], pr: String) extends LNUrlData {

  val additionalRoutes: Set[GraphStructure.GraphEdge] = RouteCalculation.makeExtraEdges(routes.map(PayRequest.routeToHops), prExt.pr.nodeId)

  lazy val prExt: PaymentRequestExt = PaymentRequestExt.fromRaw(pr)

  val isThrowAway: Boolean = disposable.getOrElse(true)
}