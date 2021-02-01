package com.lightning.walletapp.ln.wire

import java.nio.ByteOrder._
import com.softwaremill.quicklens._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import fr.acinq.bitcoin.{Crypto, LexicographicalOrdering, MilliSatoshi, Protocol, Satoshi}
import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}
import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, PublicKey, Scalar}
import com.lightning.walletapp.lnutils.olympus.OlympusWrap.StringVec
import com.lightning.walletapp.ln.crypto.Sphinx.PaymentPacket
import com.lightning.walletapp.ln.crypto.Sphinx
import com.lightning.walletapp.ChannelManager
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import scodec.Attempt


trait LightningMessage { me =>
  def remote: LNDirectionalMessage = me -> false
  def local: LNDirectionalMessage = me -> true
}

trait RoutingMessage extends LightningMessage
trait ChannelSetupMessage extends LightningMessage
trait ChannelMessage extends LightningMessage { val channelId: ByteVector }
case class Init(globalFeatures: ByteVector, localFeatures: ByteVector) extends LightningMessage
case class Ping(pongLength: Int, data: ByteVector) extends LightningMessage
case class Pong(data: ByteVector) extends LightningMessage

// CHANNEL SETUP MESSAGES: open channels never get these

case class ChannelFlags(flags: Byte) {
  def isPublic = Features.isBitSet(0, flags)
  def isZeroConfSpendablePush = Features.isBitSet(3, flags)
}

case class OpenChannel(chainHash: ByteVector, temporaryChannelId: ByteVector, fundingSatoshis: Long, pushMsat: Long,
                       dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64, channelReserveSatoshis: Long, htlcMinimumMsat: Long,
                       feeratePerKw: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubkey: PublicKey, revocationBasepoint: Point,
                       paymentBasepoint: Point, delayedPaymentBasepoint: Point, htlcBasepoint: Point, firstPerCommitmentPoint: Point,
                       channelFlags: ChannelFlags) extends ChannelSetupMessage

case class AcceptChannel(temporaryChannelId: ByteVector, dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64,
                         channelReserveSatoshis: Long, htlcMinimumMsat: Long, minimumDepth: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                         fundingPubkey: PublicKey, revocationBasepoint: Point, paymentBasepoint: Point, delayedPaymentBasepoint: Point,
                         htlcBasepoint: Point, firstPerCommitmentPoint: Point) extends ChannelSetupMessage {

  lazy val dustLimitSat = Satoshi(dustLimitSatoshis)
}

case class FundingCreated(temporaryChannelId: ByteVector,
                          fundingTxid: ByteVector, fundingOutputIndex: Int,
                          signature: ByteVector) extends ChannelSetupMessage

case class FundingSigned(channelId: ByteVector, signature: ByteVector) extends ChannelSetupMessage

// CHANNEL MESSAGES

case class ClosingSigned(channelId: ByteVector, feeSatoshis: Long, signature: ByteVector) extends ChannelMessage
case class FundingLocked(channelId: ByteVector, nextPerCommitmentPoint: Point) extends ChannelMessage { me =>
  def nextPointRight = Right(nextPerCommitmentPoint)
  def some = Some(me)
}

case class Shutdown(channelId: ByteVector, scriptPubKey: ByteVector) extends ChannelMessage { me =>
  def some = Some(me)
}

case class UpdateAddHtlc(channelId: ByteVector, id: Long, amountMsat: Long, paymentHash: ByteVector, expiry: Long,
                         onionRoutingPacket: OnionRoutingPacket = Sphinx.emptyOnionPacket) extends ChannelMessage { me =>
  lazy val hash160 = Crypto.ripemd160(paymentHash)
  lazy val amount = MilliSatoshi(amountMsat)
}

case class UpdateFailHtlc(channelId: ByteVector, id: Long, reason: ByteVector) extends ChannelMessage
case class UpdateFailMalformedHtlc(channelId: ByteVector, id: Long, onionHash: ByteVector, failureCode: Int) extends ChannelMessage
case class UpdateFulfillHtlc(channelId: ByteVector, id: Long, paymentPreimage: ByteVector) extends ChannelMessage {
  val paymentHash = Crypto.sha256(paymentPreimage)
}

case class UpdateFee(channelId: ByteVector, feeratePerKw: Long) extends ChannelMessage
case class CommitSig(channelId: ByteVector, signature: ByteVector, htlcSignatures: List[ByteVector] = Nil) extends ChannelMessage
case class RevokeAndAck(channelId: ByteVector, perCommitmentSecret: Scalar, nextPerCommitmentPoint: Point) extends ChannelMessage

case class Error(channelId: ByteVector, data: ByteVector) extends ChannelMessage {
  def exception = new LightningException(Tools bin2readable data.toArray)
}

case class ChannelReestablish(channelId: ByteVector, nextLocalCommitmentNumber: Long,
                              nextRemoteRevocationNumber: Long, yourLastPerCommitmentSecret: Option[Scalar],
                              myCurrentPerCommitmentPoint: Option[Point] = None) extends ChannelMessage

// ROUTING MESSAGES: open channels never get these except for ChannelUpdate

case class AnnouncementSignatures(channelId: ByteVector,
                                  shortChannelId: Long, nodeSignature: ByteVector,
                                  bitcoinSignature: ByteVector) extends RoutingMessage

case class ChannelAnnouncement(nodeSignature1: ByteVector, nodeSignature2: ByteVector, bitcoinSignature1: ByteVector,
                               bitcoinSignature2: ByteVector, features: ByteVector, chainHash: ByteVector, shortChannelId: Long,
                               nodeId1: PublicKey, nodeId2: PublicKey, bitcoinKey1: PublicKey, bitcoinKey2: PublicKey,
                               unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage

// PAYMENT ROUTE INFO

case class ChannelUpdate(signature: ByteVector, chainHash: ByteVector, shortChannelId: Long, timestamp: Long,
                         messageFlags: Byte, channelFlags: Byte, cltvExpiryDelta: Int, htlcMinimumMsat: Long,
                         feeBaseMsat: Long, feeProportionalMillionths: Long, htlcMaximumMsat: Option[Long],
                         unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage { me =>

  def some = Some(me)
  def toHop(nodeId: PublicKey) = Hop(nodeId, shortChannelId, cltvExpiryDelta, htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths)
  require(requirement = (messageFlags & 1) != 0 == htlcMaximumMsat.isDefined, "htlcMaximumMsat is not consistent with messageFlags value")
  lazy val isHosted = Tools.fromShortId(shortChannelId) match { case (blockHeight, _, _) => blockHeight <= LNParams.maxHostedBlockHeight }
  lazy val feeEstimate = feeBaseMsat + feeProportionalMillionths * 10
}

case class Hop(nodeId: PublicKey, shortChannelId: Long,
               cltvExpiryDelta: Int, htlcMinimumMsat: Long,
               feeBaseMsat: Long, feeProportionalMillionths: Long) {

  lazy val (block, tx, output) = fromShortId(shortChannelId)
  lazy val feeBreakdown = f"${feeProportionalMillionths / 10000D}%2f%% of payment sum + baseline $feeBaseMsat msat"
  lazy val humanDetails = s"Node ID: $nodeId, Channel ID: ${block}x${tx}x$output, Expiry: $cltvExpiryDelta blocks, Routing fee: $feeBreakdown"
  def fee(amountMsat: Long) = feeBaseMsat + (feeProportionalMillionths * amountMsat) / 1000000L
}

case class QueryChannelRange(chainHash: ByteVector, firstBlockNum: Long, numberOfBlocks: Long) extends RoutingMessage
case class GossipTimestampFilter(chainHash: ByteVector, firstTimestamp: Long, timestampRange: Long) extends RoutingMessage

// NODE ADDRESS HANDLING

case class NodeAnnouncement(signature: ByteVector, features: ByteVector, timestamp: Long,
                            nodeId: PublicKey, rgbColor: RGB, alias: String, addresses: NodeAddressList,
                            unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage {

  val pretty = addresses collectFirst {
    case _: IPv4 | _: IPv6 => nodeId.toString take 15 grouped 3 mkString "\u0020"
    case _: Tor2 => s"<strong>Tor</strong>\u0020${nodeId.toString take 12 grouped 3 mkString "\u0020"}"
    case _: Tor3 => s"<strong>Tor</strong>\u0020${nodeId.toString take 12 grouped 3 mkString "\u0020"}"
  } getOrElse "No IP address"

  val asString = {
    val ellipsized = if (alias.length > 18) s"${alias take 16}..." else alias
    s"<strong>$ellipsized</strong><br><small>$pretty</small>"
  }

  val identifier = (alias + nodeId.toString).toLowerCase
  lazy val hostedChanId = Tools.hostedChanId(LNParams.keys.extendedNodeKey.publicKey.toBin, nodeId.toBin)
  def unsafeFirstAddress = addresses.collectFirst(NodeAddress.toInetSocketAddress)
}

sealed trait NodeAddress { def canBeUpdatedIfOffline: Boolean }
case object Padding extends NodeAddress { def canBeUpdatedIfOffline = false }

case class IPv4(ipv4: Inet4Address, port: Int) extends NodeAddress {
  override def toString: String = s"${ipv4.toString.tail}:$port"
  def canBeUpdatedIfOffline = true
}

case class IPv6(ipv6: Inet6Address, port: Int) extends NodeAddress {
  override def toString: String = s"${ipv6.toString.tail}:$port"
  def canBeUpdatedIfOffline = true
}

case class Tor2(tor2: String, port: Int) extends NodeAddress {
  override def toString: String = s"$tor2${NodeAddress.onionSuffix}:$port"
  def canBeUpdatedIfOffline = false
}

case class Tor3(tor3: String, port: Int) extends NodeAddress {
  override def toString: String = s"$tor3${NodeAddress.onionSuffix}:$port"
  def canBeUpdatedIfOffline = false
}

case class Domain(domain: String, port: Int) extends NodeAddress {
  override def toString: String = s"$domain:$port"
  def canBeUpdatedIfOffline = false
}

case object NodeAddress {
  val onionSuffix = ".onion"
  val V2Len = 16
  val V3Len = 56

  def toInetSocketAddress: PartialFunction[NodeAddress, InetSocketAddress] = {
    case Tor2(onionHost, port) => new InetSocketAddress(s"$onionHost$onionSuffix", port)
    case Tor3(onionHost, port) => new InetSocketAddress(s"$onionHost$onionSuffix", port)
    case IPv4(sockAddress, port) => new InetSocketAddress(sockAddress, port)
    case IPv6(sockAddress, port) => new InetSocketAddress(sockAddress, port)
    case Domain(site, port) => new InetSocketAddress(site, port)
  }

  def fromParts(host: String, port: Int, orElse: (String, Int) => NodeAddress = resolveIp) =
    if (host.endsWith(onionSuffix) && host.length == V2Len + onionSuffix.length) Tor2(host dropRight onionSuffix.length, port)
    else if (host.endsWith(onionSuffix) && host.length == V3Len + onionSuffix.length) Tor3(host dropRight onionSuffix.length, port)
    else orElse(host, port)

  def resolveIp(host: String, port: Int) = InetAddress getByName host match {
    case inetVersion4Address: Inet4Address => IPv4(ipv4 = inetVersion4Address, port)
    case inetVersion6Address: Inet6Address => IPv6(ipv6 = inetVersion6Address, port)
  }
}

// Hosted channel messages

trait HostedChannelMessage extends LightningMessage

case class InvokeHostedChannel(chainHash: ByteVector, refundScriptPubKey: ByteVector,
                               secret: ByteVector) extends HostedChannelMessage

case class InitHostedChannel(maxHtlcValueInFlightMsat: UInt64,
                             htlcMinimumMsat: Long, maxAcceptedHtlcs: Int, channelCapacityMsat: Long,
                             liabilityDeadlineBlockdays: Int, minimalOnchainRefundAmountSatoshis: Long,
                             initialClientBalanceMsat: Long, features: ByteVector) extends HostedChannelMessage

case class LastCrossSignedState(refundScriptPubKey: ByteVector,
                                initHostedChannel: InitHostedChannel, blockDay: Long, localBalanceMsat: Long, remoteBalanceMsat: Long,
                                localUpdates: Long, remoteUpdates: Long, incomingHtlcs: List[UpdateAddHtlc], outgoingHtlcs: List[UpdateAddHtlc],
                                remoteSigOfLocal: ByteVector, localSigOfRemote: ByteVector) extends HostedChannelMessage { me =>

  lazy val reverse: LastCrossSignedState =
    copy(localUpdates = remoteUpdates, remoteUpdates = localUpdates,
      localBalanceMsat = remoteBalanceMsat, remoteBalanceMsat = localBalanceMsat,
      remoteSigOfLocal = localSigOfRemote, localSigOfRemote = remoteSigOfLocal,
      incomingHtlcs = outgoingHtlcs, outgoingHtlcs = incomingHtlcs)

  lazy val hostedSigHash: ByteVector = Crypto sha256 {
    val inPayments = incomingHtlcs.map(updateAddHtlcCodec.encode(_).require.toByteVector).sortWith(LexicographicalOrdering.isLessThan)
    val outPayments = outgoingHtlcs.map(updateAddHtlcCodec.encode(_).require.toByteVector).sortWith(LexicographicalOrdering.isLessThan)

    refundScriptPubKey ++
      Protocol.writeUInt16(initHostedChannel.liabilityDeadlineBlockdays, LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.minimalOnchainRefundAmountSatoshis, LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.channelCapacityMsat, LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.initialClientBalanceMsat, LITTLE_ENDIAN) ++
      Protocol.writeUInt32(blockDay, LITTLE_ENDIAN) ++
      Protocol.writeUInt64(localBalanceMsat, LITTLE_ENDIAN) ++
      Protocol.writeUInt64(remoteBalanceMsat, LITTLE_ENDIAN) ++
      Protocol.writeUInt32(localUpdates, LITTLE_ENDIAN) ++
      Protocol.writeUInt32(remoteUpdates, LITTLE_ENDIAN) ++
      inPayments.foldLeft(ByteVector.empty) { case acc \ htlc => acc ++ htlc } ++
      outPayments.foldLeft(ByteVector.empty) { case acc \ htlc => acc ++ htlc }
  }

  def verifyRemoteSig(pubKey: PublicKey) = Crypto.verifySignature(hostedSigHash, remoteSigOfLocal, pubKey)
  def withLocalSigOfRemote(priv: PrivateKey) = me.modify(_.localSigOfRemote) setTo sign(reverse.hostedSigHash, priv)

  def isAhead(remoteLCSS: LastCrossSignedState) = remoteUpdates > remoteLCSS.localUpdates || localUpdates > remoteLCSS.remoteUpdates
  def isEven(remoteLCSS: LastCrossSignedState) = remoteUpdates == remoteLCSS.localUpdates && localUpdates == remoteLCSS.remoteUpdates
  def stateUpdate(isTerminal: Boolean) = StateUpdate(blockDay, localUpdates, remoteUpdates, localSigOfRemote, isTerminal)
}

case class StateUpdate(blockDay: Long, localUpdates: Long, remoteUpdates: Long,
                       localSigOfRemoteLCSS: ByteVector, isTerminal: Boolean) extends HostedChannelMessage

case class StateOverride(blockDay: Long, localBalanceMsat: Long, localUpdates: Long,
                         remoteUpdates: Long, localSigOfRemoteLCSS: ByteVector) extends HostedChannelMessage

// Not in a spec
case class OutRequest(sat: Long, badNodes: Set[String], badChans: Set[Long], from: Set[String], to: String)
case class LocalBackups(normal: Vector[HasNormalCommits], hosted: Vector[HostedCommits], earliestUtxoSeconds: Long, v: Int)
case class HostedState(channelId: ByteVector, nextLocalUpdates: LNMessageVector, nextRemoteUpdates: LNMessageVector, lastCrossSignedState: LastCrossSignedState)
case class CerberusPayload(payloads: Vector[AESZygote], halfTxIds: StringVec)
case class AESZygote(v: Int, iv: ByteVector, ciphertext: ByteVector)