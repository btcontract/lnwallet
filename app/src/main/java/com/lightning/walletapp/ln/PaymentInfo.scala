package com.lightning.walletapp.ln

import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.crypto.Sphinx._
import com.lightning.walletapp.ln.RoutingInfoTag._
import com.lightning.walletapp.ln.wire.OnionTlv.{AmountToForward, OutgoingCltv, PaymentData}
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import scodec.Attempt


object PaymentInfo {
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3
  final val FROZEN = 4

  final val NOT_SENDABLE = 0
  final val SENDABLE_AIR = 1

  final val NOIMAGE = ByteVector.fromValidHex("3030303030303030")
  final val NOCHANID = ByteVector.fromValidHex("3131313131313131")
  final val REBALANCING = "Rebalancing"

  type FullOrEmptyRD = Either[RoutingData, RoutingData]
  type FailureDetails = (DecryptedFailurePacket, PaymentRoute)
  type FailuresVec = Vector[FailureDetails]

  // Stores a history of error responses from peers per each outgoing payment request
  var errors = Map.empty[ByteVector, FailuresVec] withDefaultValue Vector.empty
  private[this] var replacedChans = Set.empty[Long]

  def buildOnion(keys: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: ByteVector): PacketAndSecrets = {
    require(keys.size == payloads.size, "Count mismatch: there should be exactly as much payloads as node pubkeys")
    PaymentPacket.create(Tools.randomPrivKey, keys, for (payload <- payloads) yield payload.encode, assoc)
  }

  def useFirstRoute(rest: PaymentRouteVec, rd: RoutingData) =
    if (rest.isEmpty) Left(rd) else useRoute(rest.head, rest.tail, rd)

  def onChainThreshold = Scripts.weight2fee(LNParams.broadcaster.perKwSixSat, 750)

  def useRoute(route: PaymentRoute, rest: PaymentRouteVec, rd: RoutingData): FullOrEmptyRD = {
    val firstPayeeCltvAdjustedExpiry = LNParams.broadcaster.currentHeight + rd.pr.adjustedMinFinalCltvExpiry

    val payloadVec: FinalPayload = rd.pr.paymentSecretOpt match {
      case Some(paymentSecret) => FinalTlvPayload(TlvStream(Seq(AmountToForward(MilliSatoshi(rd.firstMsat)), OutgoingCltv(firstPayeeCltvAdjustedExpiry), PaymentData(paymentSecret, MilliSatoshi(rd.firstMsat))), Nil))
      case None => FinalLegacyPayload(rd.firstMsat, firstPayeeCltvAdjustedExpiry)
    }

    val finalHopState = Tuple4(payloadVec +: Vector.empty[PerHopPayload], Vector.empty[PublicKey], rd.firstMsat, firstPayeeCltvAdjustedExpiry)

    // Walk in reverse direction from receiver to sender and accumulate cltv deltas + fees
    val (allPayloads, nodeIds, lastMsat, lastExpiry) = route.reverse.foldLeft(finalHopState) { case (payloads, nodes, msat, expiry) \ hop =>
      (RelayLegacyPayload(hop.shortChannelId, msat, expiry) +: payloads, hop.nodeId +: nodes, hop.fee(msat) + msat, hop.cltvExpiryDelta + expiry)
    }

    val isCltvBreach = lastExpiry - LNParams.broadcaster.currentHeight > LNParams.maxCltvDelta
    val isOnChainCapBreach = rd.capFeeByOnChain && MilliSatoshi(lastMsat - rd.firstMsat) > onChainThreshold
    val rd1 = if (isOnChainCapBreach) rd.modify(_.expensiveScids).using(_ :+ route.maxBy(_ fee 10000000L).shortChannelId) else rd
    val isUnacceptableRoute = LNParams.isFeeBreach(route, rd.firstMsat, percent = 100L) || isOnChainCapBreach || isCltvBreach

    if (isUnacceptableRoute) useFirstRoute(rest, rd1) else {
      val onion = buildOnion(keys = nodeIds :+ rd1.pr.nodeId, payloads = allPayloads, assoc = rd1.pr.paymentHash)
      val rd2 = rd1.copy(routes = rest, usedRoute = route, onion = onion, lastMsat = lastMsat, lastExpiry = lastExpiry)
      Right(rd2)
    }
  }

  def without(routes: PaymentRouteVec, fun: Hop => Boolean) = routes.filterNot(_ exists fun)
  def failIncorrectDetails(packet: DecryptedPacket, msat: MilliSatoshi, add: UpdateAddHtlc): CMDFailHtlc =
    failHtlc(packet, IncorrectOrUnknownPaymentDetails(msat.toLong, LNParams.broadcaster.currentHeight), add)

  def failHtlc(packet: DecryptedPacket, msg: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = FailurePacket.create(packet.sharedSecret, msg), id = add.id)

  def withoutChan(shortId: Long, rd: RoutingData, span: Long, msat: Long) = {
    val routesWithoutBadChannels = without(rd.routes, _.shortChannelId == shortId)
    val blackListedChan = Tuple3(shortId.toString, span, msat)
    val rd1 = rd.copy(routes = routesWithoutBadChannels)
    Some(rd1) -> Vector(blackListedChan)
  }

  def withoutNodes(badNodes: PublicKeyVec, rd: RoutingData, span: Long) = {
    val routesWithoutBadNodes = without(rd.routes, badNodes contains _.nodeId)
    val blackListedNodes = for (node <- badNodes) yield (node.toString, span, 0L)
    val rd1 = rd.copy(routes = routesWithoutBadNodes)
    Some(rd1) -> blackListedNodes
  }

  def replaceChan(nodeKey: PublicKey, rd: RoutingData, upd: ChannelUpdate) = {
    // In some cases we can just replace a faulty hop with a supplied one
    // but only do this once per each channel to avoid infinite loops

    val rd1 = rd.copy(routes = rd.usedRoute.map {
      case keepMe if keepMe.nodeId != nodeKey => keepMe
      case _ => upd.toHop(nodeKey)
    } +: rd.routes)

    // Prevent endless loop by marking this channel
    replacedChans += upd.shortChannelId
    Some(rd1) -> Vector.empty
  }

  def parseFailureCutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    val parsed = FailurePacket.decrypt(fail.reason, rd.onion.sharedSecrets)

    parsed.foreach { details =>
      val record = details -> rd.usedRoute
      val withFailureAdded = errors(rd.pr.paymentHash) :+ record
      errors = errors.updated(rd.pr.paymentHash, withFailureAdded)
    }

    parsed.map {
      case DecryptedFailurePacket(nodeKey, _: Perm) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case DecryptedFailurePacket(nodeKey, ExpiryTooFar) if nodeKey == rd.pr.nodeId => None -> Vector.empty
      case DecryptedFailurePacket(nodeKey, u: ExpiryTooSoon) if !replacedChans.contains(u.update.shortChannelId) => replaceChan(nodeKey, rd, u.update)
      case DecryptedFailurePacket(nodeKey, u: FeeInsufficient) if !replacedChans.contains(u.update.shortChannelId) => replaceChan(nodeKey, rd, u.update)
      case DecryptedFailurePacket(nodeKey, u: IncorrectCltvExpiry) if !replacedChans.contains(u.update.shortChannelId) => replaceChan(nodeKey, rd, u.update)

      case DecryptedFailurePacket(nodeKey, u: Update) =>
        val isHonest = Announcements.checkSig(u.update, nodeKey)
        if (!isHonest) withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
        else rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(u.update.shortChannelId, rd, 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, PermanentNodeFailure) => withoutNodes(Vector(nodeKey), rd, 86400 * 7 * 1000)
      case DecryptedFailurePacket(nodeKey, RequiredNodeFeatureMissing) => withoutNodes(Vector(nodeKey), rd, 86400 * 1000)
      case DecryptedFailurePacket(nodeKey, _: BadOnion) => withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, UnknownNextPeer | PermanentChannelFailure) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 86400 * 7 * 1000, 0L)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

      case DecryptedFailurePacket(nodeKey, _) =>
        rd.usedRoute.collectFirst { case payHop if payHop.nodeId == nodeKey =>
          withoutChan(payHop.shortChannelId, rd, 180 * 1000, rd.firstMsat)
        } getOrElse withoutNodes(Vector(nodeKey), rd, 180 * 1000)

    } getOrElse {
      val cut = rd.usedRoute drop 1 dropRight 1
      withoutNodes(cut.map(_.nodeId), rd, 60 * 1000)
    }
  }

  // Once mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, loop: Boolean) =
    PaymentPacket.peel(nodeSecret, add.paymentHash, add.onionRoutingPacket) match {
      case Left(bad) => CMDFailMalformedHtlc(add.id, bad.onionHash, bad.code)
      case Right(pkt) if pkt.isLastPacket => doResolve(pkt, add, bag, loop)
      case Right(pkt) => failHtlc(pkt, UnknownNextPeer, add)
    }

  def doResolve(pkt: DecryptedPacket, add: UpdateAddHtlc, bag: PaymentInfoBag, loop: Boolean) =
    (LightningMessageCodecs.finalPerHopPayloadCodec decode pkt.payload.bits, bag getPaymentInfo add.paymentHash) match {
      case Attempt.Failure(err: LightningMessageCodecs.MissingRequiredTlv) \ _ => failHtlc(pkt, InvalidOnionPayload(err.tag, 0), add)
      case Attempt.Successful(payload) \ _ if payload.value.cltvExpiry != add.expiry => failHtlc(pkt, FinalIncorrectCltvExpiry(add.expiry), add)
      case attempt \ Success(info) if attempt.isSuccessful && info.pr.msatOrMin > add.amount => failIncorrectDetails(pkt, info.pr.msatOrMin, add)
      case attempt \ Success(info) if attempt.isSuccessful && info.pr.msatOrMin * 2 < add.amount => failIncorrectDetails(pkt, info.pr.msatOrMin, add)
      case attempt \ Success(info) if LNParams.broadcaster.currentHeight + PaymentRequest.cltvExpiryTag.expiryDelta > add.expiry => failIncorrectDetails(pkt, info.pr.msatOrMin, add)
      case attempt \ Success(info) if attempt.isSuccessful && !loop && info.incoming == 1 && info.status != PaymentInfo.SUCCESS => CMDFulfillHtlc(add, info.paymentPreimage)
      case attempt \ _ if attempt.isSuccessful => failIncorrectDetails(pkt, add.amount, add)
      case _ => failHtlc(pkt, InvalidOnionPayload(UInt64(0), 0), add)
    }
}

case class RoutingData(pr: PaymentRequest, routes: PaymentRouteVec, usedRoute: PaymentRoute,
                       onion: PacketAndSecrets, firstMsat: Long /* amount without off-chain fee */ ,
                       lastMsat: Long /* amount with off-chain fee added */ , lastExpiry: Long, callsLeft: Int,
                       useCache: Boolean, airLeft: Int, capFeeByOnChain: Boolean, expensiveScids: Vector[Long],
                       description: PaymentDescription, fromHostedOnly: Boolean, isRebalancing: Boolean) {

  // Empty used route means we're sending to peer and its nodeId should be our targetId
  def nextNodeId(route: PaymentRoute) = route.headOption.map(_.nodeId) getOrElse pr.nodeId
  lazy val queryText = s"${description.text} ${pr.nodeId.toString} ${pr.paymentHash.toHex}"
}

case class PaymentInfo(rawPr: String, hash: String, preimage: String,
                       incoming: Int, status: Int, stamp: Long, description: String,
                       firstMsat: Long, lastMsat: Long, lastExpiry: Long) {

  // Incoming `lastExpiry` becomes > 0 once reflexive
  val isLooper = incoming == 1 && lastExpiry != 0
  val firstSum = MilliSatoshi(firstMsat)

  // Decode on demand for performance
  lazy val pr = to[PaymentRequest](rawPr)
  lazy val paymentHash = ByteVector.fromValidHex(hash)
  lazy val paymentPreimage = ByteVector.fromValidHex(preimage)

  // Back compat: use default object if source is not json
  lazy val pd = try to[PaymentDescription](description) catch {
    case _: Throwable => PaymentDescription(None, description)
  }
}

trait PaymentInfoBag { me =>
  def extractPreimage(tx: Transaction)
  def getPaymentInfo(hash: ByteVector): Try[PaymentInfo]
  def updStatus(paymentStatus: Int, hash: ByteVector)
  def updOkOutgoing(m: UpdateFulfillHtlc)
  def updOkIncoming(m: UpdateAddHtlc)
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
  require(domain.forall(url.contains), "Action domain mismatch")
  require(url startsWith "https://", "Action url is not HTTPS")
}

case class AESAction(domain: Option[String], description: String, ciphertext: String, iv: String) extends PaymentAction {
  val ciphertextBytes = ByteVector.fromValidBase64(ciphertext).take(1024 * 4).toArray // up to ~2kb of encrypted data
  val ivBytes = ByteVector.fromValidBase64(iv).take(24).toArray // 16 bytes
  val finalMessage = s"<br>${description take 144}"
}

// Previously `PaymentInfo.description` was just raw text, now it's json
case class PaymentDescription(action: Option[PaymentAction], text: String)