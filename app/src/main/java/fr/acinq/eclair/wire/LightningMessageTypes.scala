/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.wire

import fr.acinq.eclair._
import immortan.{ChannelMaster, LNParams}
import scodec.bits.{BitVector, ByteVector}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}
import fr.acinq.eclair.{CltvExpiry, CltvExpiryDelta, Features, MilliSatoshi, ShortChannelId, UInt64}
import fr.acinq.bitcoin.{ByteVector32, ByteVector64, Crypto, LexicographicalOrdering, Protocol, Satoshi}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.channel.ChannelVersion
import fr.acinq.eclair.router.Announcements
import java.nio.charset.StandardCharsets
import com.google.common.base.Charsets
import immortan.crypto.Tools
import scodec.DecodeResult
import java.nio.ByteOrder

/**
 * Created by PM on 15/11/2016.
 */

sealed trait LightningMessage extends Serializable
sealed trait SetupMessage extends LightningMessage
sealed trait ChannelMessage extends LightningMessage
sealed trait HtlcMessage extends LightningMessage
sealed trait RoutingMessage extends LightningMessage
sealed trait AnnouncementMessage extends RoutingMessage // <- not in the spec
sealed trait HasTimestamp extends LightningMessage { def timestamp: Long }
sealed trait HasTemporaryChannelId extends LightningMessage { def temporaryChannelId: ByteVector32 } // <- not in the spec
sealed trait HasChannelId extends LightningMessage { def channelId: ByteVector32 } // <- not in the spec
sealed trait HasChainHash extends LightningMessage { def chainHash: ByteVector32 } // <- not in the spec
sealed trait UpdateMessage extends HtlcMessage // <- not in the spec

case class Init(features: Features, tlvs: TlvStream[InitTlv] = TlvStream.empty) extends SetupMessage {
  val networks: Seq[ByteVector32] = tlvs.get[InitTlv.Networks].map(_.chainHashes).getOrElse(Nil)
}

object Error {
  def apply(channelId: ByteVector32, msg: String): Error = {
    val bytes = msg.getBytes(Charsets.US_ASCII)
    Error(channelId, ByteVector view bytes)
  }
}

case class Error(channelId: ByteVector32, data: ByteVector) extends SetupMessage with HasChannelId {
  def toAscii: String = new String(data.toArray, StandardCharsets.US_ASCII)
}

case class Ping(pongLength: Int, data: ByteVector) extends SetupMessage

case class Pong(data: ByteVector) extends SetupMessage

case class ChannelReestablish(channelId: ByteVector32, nextLocalCommitmentNumber: Long,
                              nextRemoteRevocationNumber: Long, yourLastPerCommitmentSecret: PrivateKey,
                              myCurrentPerCommitmentPoint: PublicKey) extends ChannelMessage with HasChannelId

case class OpenChannel(chainHash: ByteVector32, temporaryChannelId: ByteVector32, fundingSatoshis: Satoshi, pushMsat: MilliSatoshi, dustLimitSatoshis: Satoshi, maxHtlcValueInFlightMsat: UInt64,
                       channelReserveSatoshis: Satoshi, htlcMinimumMsat: MilliSatoshi, feeratePerKw: FeeratePerKw, toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int, fundingPubkey: PublicKey,
                       revocationBasepoint: PublicKey, paymentBasepoint: PublicKey, delayedPaymentBasepoint: PublicKey, htlcBasepoint: PublicKey, firstPerCommitmentPoint: PublicKey,
                       channelFlags: Byte, tlvStream: TlvStream[OpenChannelTlv] = TlvStream.empty) extends ChannelMessage with HasTemporaryChannelId with HasChainHash

case class AcceptChannel(temporaryChannelId: ByteVector32, dustLimitSatoshis: Satoshi, maxHtlcValueInFlightMsat: UInt64, channelReserveSatoshis: Satoshi, htlcMinimumMsat: MilliSatoshi, minimumDepth: Long,
                         toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int, fundingPubkey: PublicKey, revocationBasepoint: PublicKey, paymentBasepoint: PublicKey, delayedPaymentBasepoint: PublicKey,
                         htlcBasepoint: PublicKey, firstPerCommitmentPoint: PublicKey, tlvStream: TlvStream[AcceptChannelTlv] = TlvStream.empty) extends ChannelMessage with HasTemporaryChannelId

case class FundingCreated(temporaryChannelId: ByteVector32, fundingTxid: ByteVector32, fundingOutputIndex: Int, signature: ByteVector64) extends ChannelMessage with HasTemporaryChannelId

case class FundingSigned(channelId: ByteVector32, signature: ByteVector64) extends ChannelMessage with HasChannelId

case class FundingLocked(channelId: ByteVector32, nextPerCommitmentPoint: PublicKey) extends ChannelMessage with HasChannelId

case class Shutdown(channelId: ByteVector32, scriptPubKey: ByteVector) extends ChannelMessage with HasChannelId

case class ClosingSigned(channelId: ByteVector32, feeSatoshis: Satoshi, signature: ByteVector64) extends ChannelMessage with HasChannelId

case class UpdateAddHtlc(channelId: ByteVector32, id: Long,
                         amountMsat: MilliSatoshi, paymentHash: ByteVector32, cltvExpiry: CltvExpiry, onionRoutingPacket: OnionRoutingPacket,
                         tlvStream: TlvStream.GenericTlvStream = TlvStream.empty) extends HtlcMessage with HasChannelId with UpdateMessage {

  // Important: LNParams.format must be defined
  private[this] lazy val fullTagOpt: Option[FullPaymentTag] = for {
    cipherBytes <- tlvStream.get[PaymentTagTlv.EncryptedPaymentSecret]
    plainBytes <- Tools.chaChaDecrypt(LNParams.format.keys.ourNodePrivateKey.value, cipherBytes.data).toOption
    DecodeResult(ShortPaymentTag(secret, tag), _) <- PaymentTagTlv.shortPaymentTagCodec.decode(plainBytes.toBitVector).toOption
  } yield FullPaymentTag(paymentHash, secret, tag)

  // This is relevant for outgoing payments, NO_SECRET means this is a locally initiated outgoing payment
  lazy val fullTag: FullPaymentTag = fullTagOpt getOrElse FullPaymentTag(ChannelMaster.NO_SECRET, paymentHash, PaymentTagTlv.LOCALLY_SENT)

  // This is relevant for outgoing payments where we can ensure onion key uniqueness
  final val partId: ByteVector = onionRoutingPacket.publicKey
}

case class UpdateFulfillHtlc(channelId: ByteVector32, id: Long, paymentPreimage: ByteVector32) extends HtlcMessage with HasChannelId with UpdateMessage {
  lazy val paymentHash: ByteVector32 = Crypto.sha256(paymentPreimage)
}

case class UpdateFailHtlc(channelId: ByteVector32, id: Long, reason: ByteVector) extends HtlcMessage with HasChannelId with UpdateMessage

case class UpdateFailMalformedHtlc(channelId: ByteVector32, id: Long, onionHash: ByteVector32, failureCode: Int) extends HtlcMessage with HasChannelId with UpdateMessage

case class CommitSig(channelId: ByteVector32, signature: ByteVector64, htlcSignatures: List[ByteVector64] = Nil) extends HtlcMessage with HasChannelId

case class RevokeAndAck(channelId: ByteVector32, perCommitmentSecret: PrivateKey, nextPerCommitmentPoint: PublicKey) extends HtlcMessage with HasChannelId

case class UpdateFee(channelId: ByteVector32, feeratePerKw: FeeratePerKw) extends ChannelMessage with HasChannelId with UpdateMessage

case class AnnouncementSignatures(channelId: ByteVector32, shortChannelId: ShortChannelId, nodeSignature: ByteVector64, bitcoinSignature: ByteVector64) extends RoutingMessage with HasChannelId

case class ChannelAnnouncement(nodeSignature1: ByteVector64, nodeSignature2: ByteVector64, bitcoinSignature1: ByteVector64, bitcoinSignature2: ByteVector64, features: Features,
                               chainHash: ByteVector32, shortChannelId: ShortChannelId, nodeId1: PublicKey, nodeId2: PublicKey, bitcoinKey1: PublicKey, bitcoinKey2: PublicKey,
                               unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage with AnnouncementMessage with HasChainHash {

  def getNodeIdSameSideAs(cu: ChannelUpdate): PublicKey = if (cu.position == ChannelUpdate.POSITION1NODE) nodeId1 else nodeId2

  def isPHC: Boolean = bitcoinKey1 == nodeId1 && bitcoinKey2 == nodeId2 && bitcoinSignature1 == nodeSignature1 && bitcoinSignature2 == nodeSignature2

  // Point useless fields to same object, db-restored should be the same

  def lite: ChannelAnnouncement =
    copy(nodeSignature1 = ByteVector64.Zeroes, nodeSignature2 = ByteVector64.Zeroes,
      bitcoinSignature1 = ByteVector64.Zeroes, bitcoinSignature2 = ByteVector64.Zeroes,
      features = Features.empty, chainHash = LNParams.chainHash,
      bitcoinKey1 = invalidPubKey, bitcoinKey2 = invalidPubKey)
}

case class Color(r: Byte, g: Byte, b: Byte)

sealed trait NodeAddress { def socketAddress: InetSocketAddress }

sealed trait OnionAddress extends NodeAddress

object NodeAddress {
  val onionSuffix = ".onion"
  val V2Len = 16
  val V3Len = 56

  def isTor(na: NodeAddress): Boolean = na match {
    case _: Tor2 | _: Tor3 => true
    case _ => false
  }

  def fromParts(host: String, port: Int, orElse: (String, Int) => NodeAddress = resolveIp): NodeAddress =
    if (host.endsWith(onionSuffix) && host.length == V2Len + onionSuffix.length) Tor2(host.dropRight(onionSuffix.length), port)
    else if (host.endsWith(onionSuffix) && host.length == V3Len + onionSuffix.length) Tor3(host.dropRight(onionSuffix.length), port)
    else orElse(host, port)

  def resolveIp(host: String, port: Int): NodeAddress =
    InetAddress getByName host match {
      case inetV4Address: Inet4Address => IPv4(inetV4Address, port)
      case inetV6Address: Inet6Address => IPv6(inetV6Address, port)
    }

  def unresolved(port: Int, host: Int*): NodeAddress =
    InetAddress getByAddress host.toArray.map(_.toByte) match {
      case inetV4Address: Inet4Address => IPv4(inetV4Address, port)
      case inetV6Address: Inet6Address => IPv6(inetV6Address, port)
    }
}

case class IPv4(ipv4: Inet4Address, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(ipv4, port)
  override def toString: String = s"${ipv4.toString.tail}:$port"
}

case class IPv6(ipv6: Inet6Address, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(ipv6, port)
  override def toString: String = s"${ipv6.toString.tail}:$port"
}

case class Tor2(tor2: String, port: Int) extends OnionAddress {
  override def socketAddress: InetSocketAddress = InetSocketAddress.createUnresolved(tor2 + NodeAddress.onionSuffix, port)
  override def toString: String = s"$tor2${NodeAddress.onionSuffix}:$port"
}

case class Tor3(tor3: String, port: Int) extends OnionAddress {
  override def socketAddress: InetSocketAddress = InetSocketAddress.createUnresolved(tor3 + NodeAddress.onionSuffix, port)
  override def toString: String = s"$tor3${NodeAddress.onionSuffix}:$port"
}

case class Domain(domain: String, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(domain, port)
  override def toString: String = s"$domain:$port"
}

case class NodeAnnouncement(signature: ByteVector64, features: Features,
                            timestamp: Long, nodeId: PublicKey, rgbColor: Color, alias: String, addresses: List[NodeAddress],
                            unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage with AnnouncementMessage with HasTimestamp

object ChannelUpdate {
  final val POSITION1NODE: java.lang.Integer = 1
  final val POSITION2NODE: java.lang.Integer = 2
  final val fullSet = Set(POSITION1NODE, POSITION2NODE)
}

case class UpdateCore(position: java.lang.Integer,
                      shortChannelId: ShortChannelId, feeBase: MilliSatoshi, feeProportionalMillionths: Long,
                      cltvExpiryDelta: CltvExpiryDelta, htlcMaximumMsat: Option[MilliSatoshi] = None)

case class ShortIdAndPosition(sid: ShortChannelId, position: java.lang.Integer)

case class ChannelUpdate(signature: ByteVector64, chainHash: ByteVector32, shortChannelId: ShortChannelId, timestamp: Long, messageFlags: Byte, channelFlags: Byte,
                         cltvExpiryDelta: CltvExpiryDelta, htlcMinimumMsat: MilliSatoshi, feeBaseMsat: MilliSatoshi, feeProportionalMillionths: Long, htlcMaximumMsat: Option[MilliSatoshi],
                         unknownFields: ByteVector = ByteVector.empty) extends RoutingMessage with AnnouncementMessage with HasTimestamp with HasChainHash {

  lazy val position: java.lang.Integer = if (Announcements isNode1 channelFlags) ChannelUpdate.POSITION1NODE else ChannelUpdate.POSITION2NODE

  lazy val core: UpdateCore = UpdateCore(position, shortChannelId, feeBaseMsat, feeProportionalMillionths, cltvExpiryDelta, htlcMaximumMsat)

  // Point useless fields to same object, db-restored should be the same, make sure it does not erase channelUpdateChecksumCodec fields
  def lite: ChannelUpdate = copy(signature = ByteVector64.Zeroes, LNParams.chainHash, unknownFields = ByteVector.empty)

  def toShortIdAndPosition: ShortIdAndPosition = ShortIdAndPosition(shortChannelId, position)
}

sealed trait EncodingType

object EncodingType {
  case object UNCOMPRESSED extends EncodingType
  case object COMPRESSED_ZLIB extends EncodingType
}

case class EncodedShortChannelIds(encoding: EncodingType, array: List[ShortChannelId] = Nil)

case class QueryShortChannelIds(chainHash: ByteVector32, shortChannelIds: EncodedShortChannelIds, tlvStream: TlvStream[QueryShortChannelIdsTlv] = TlvStream.empty) extends RoutingMessage with HasChainHash

case class ReplyShortChannelIdsEnd(chainHash: ByteVector32, complete: Byte) extends RoutingMessage with HasChainHash

case class QueryChannelRange(chainHash: ByteVector32, firstBlockNum: Long, numberOfBlocks: Long, tlvStream: TlvStream[QueryChannelRangeTlv] = TlvStream.empty) extends RoutingMessage

case class ReplyChannelRange(chainHash: ByteVector32, firstBlockNum: Long,
                             numberOfBlocks: Long, syncComplete: Byte, shortChannelIds: EncodedShortChannelIds,
                             tlvStream: TlvStream[ReplyChannelRangeTlv] = TlvStream.empty) extends RoutingMessage {

  val timestamps: ReplyChannelRangeTlv.EncodedTimestamps = tlvStream.get[ReplyChannelRangeTlv.EncodedTimestamps].get
  val checksums: ReplyChannelRangeTlv.EncodedChecksums = tlvStream.get[ReplyChannelRangeTlv.EncodedChecksums].get
}

case class GossipTimestampFilter(chainHash: ByteVector32, firstTimestamp: Long, timestampRange: Long) extends RoutingMessage with HasChainHash

case class UnknownMessage(tag: Int, data: BitVector) extends LightningMessage

// HOSTED CHANNELS

trait HostedChannelMessage extends ChannelMessage

case class InvokeHostedChannel(chainHash: ByteVector32, refundScriptPubKey: ByteVector, secret: ByteVector = ByteVector.empty) extends HostedChannelMessage

case class InitHostedChannel(maxHtlcValueInFlightMsat: UInt64, htlcMinimumMsat: MilliSatoshi, maxAcceptedHtlcs: Int, channelCapacityMsat: MilliSatoshi,
                             liabilityDeadlineBlockdays: Int, minimalOnchainRefundAmountSatoshis: Satoshi, initialClientBalanceMsat: MilliSatoshi,
                             version: ChannelVersion) extends HostedChannelMessage

case class HostedChannelBranding(rgbColor: Color, pngIcon: ByteVector, contactInfo: String) extends HostedChannelMessage

case class LastCrossSignedState(isHost: Boolean, refundScriptPubKey: ByteVector, initHostedChannel: InitHostedChannel, blockDay: Long, localBalanceMsat: MilliSatoshi,
                                remoteBalanceMsat: MilliSatoshi, localUpdates: Long, remoteUpdates: Long, incomingHtlcs: List[UpdateAddHtlc], outgoingHtlcs: List[UpdateAddHtlc],
                                remoteSigOfLocal: ByteVector64, localSigOfRemote: ByteVector64) extends HostedChannelMessage {

  lazy val reverse: LastCrossSignedState =
    copy(isHost = !isHost, localUpdates = remoteUpdates, remoteUpdates = localUpdates,
      localBalanceMsat = remoteBalanceMsat, remoteBalanceMsat = localBalanceMsat,
      remoteSigOfLocal = localSigOfRemote, localSigOfRemote = remoteSigOfLocal,
      incomingHtlcs = outgoingHtlcs, outgoingHtlcs = incomingHtlcs)

  lazy val hostedSigHash: ByteVector32 = {
    val inPayments = incomingHtlcs.map(add => LightningMessageCodecs.updateAddHtlcCodec.encode(add).require.toByteVector).sortWith(LexicographicalOrdering.isLessThan)
    val outPayments = outgoingHtlcs.map(add => LightningMessageCodecs.updateAddHtlcCodec.encode(add).require.toByteVector).sortWith(LexicographicalOrdering.isLessThan)
    val hostFlag = if (isHost) 1 else 0

    Crypto.sha256(refundScriptPubKey ++
      Protocol.writeUInt16(initHostedChannel.liabilityDeadlineBlockdays, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.minimalOnchainRefundAmountSatoshis.toLong, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.channelCapacityMsat.toLong, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt64(initHostedChannel.initialClientBalanceMsat.toLong, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt32(blockDay, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt64(localBalanceMsat.toLong, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt64(remoteBalanceMsat.toLong, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt32(localUpdates, ByteOrder.LITTLE_ENDIAN) ++
      Protocol.writeUInt32(remoteUpdates, ByteOrder.LITTLE_ENDIAN) ++
      inPayments.foldLeft(ByteVector.empty) { case (acc, htlc) => acc ++ htlc } ++
      outPayments.foldLeft(ByteVector.empty) { case (acc, htlc) => acc ++ htlc } :+
      hostFlag.toByte)
  }

  def stateUpdate: StateUpdate = StateUpdate(blockDay, localUpdates, remoteUpdates, localSigOfRemote)

  def verifyRemoteSig(pubKey: PublicKey): Boolean = Crypto.verifySignature(hostedSigHash, remoteSigOfLocal, pubKey)

  def withLocalSigOfRemote(priv: PrivateKey): LastCrossSignedState = {
    val localSignature = Crypto.sign(reverse.hostedSigHash, priv)
    copy(localSigOfRemote = localSignature)
  }
}

case class StateUpdate(blockDay: Long, localUpdates: Long, remoteUpdates: Long, localSigOfRemoteLCSS: ByteVector64) extends HostedChannelMessage

case class StateOverride(blockDay: Long, localBalanceMsat: MilliSatoshi, localUpdates: Long, remoteUpdates: Long, localSigOfRemoteLCSS: ByteVector64) extends HostedChannelMessage

case class RefundPending(startedAt: Long) extends HostedChannelMessage

case class AnnouncementSignature(nodeSignature: ByteVector64, wantsReply: Boolean) extends HostedChannelMessage

case class ResizeChannel(newCapacity: Satoshi, clientSig: ByteVector64 = ByteVector64.Zeroes) extends HostedChannelMessage {
  def isRemoteResized(remote: LastCrossSignedState): Boolean = newCapacity.toMilliSatoshi == remote.initHostedChannel.channelCapacityMsat
  def sign(priv: PrivateKey): ResizeChannel = ResizeChannel(clientSig = Crypto.sign(Crypto.sha256(sigMaterial), priv), newCapacity = newCapacity)
  def verifyClientSig(pubKey: PublicKey): Boolean = Crypto.verifySignature(Crypto.sha256(sigMaterial), clientSig, pubKey)
  lazy val sigMaterial: ByteVector = Protocol.writeUInt64(newCapacity.toLong, ByteOrder.LITTLE_ENDIAN)
  lazy val newCapacityMsatU64: UInt64 = UInt64(newCapacity.toMilliSatoshi.toLong)
}

// PHC

case class QueryPublicHostedChannels(chainHash: ByteVector32) extends RoutingMessage with HasChainHash

case class ReplyPublicHostedChannelsEnd(chainHash: ByteVector32) extends RoutingMessage with HasChainHash

// Swap In/Out

sealed trait ChainSwapMessage extends LightningMessage

sealed trait SwapIn // Chain -> LN

case object SwapInRequest extends SwapIn with ChainSwapMessage // (1) User notifies provider it wants to start swap-in

case class SwapInResponse(btcAddress: String, minChainDeposit: Satoshi) extends SwapIn with ChainSwapMessage // (2) Provider replies with chain address

case class SwapInPaymentRequest(paymentRequest: String, id: Long) extends SwapIn with ChainSwapMessage // (4) Once deposit is confirmed user can issue a withdraw invoice

object SwapInPaymentDenied {
  final val WITHDRAWAL_ALREADY_IN_FLIGHT = 1L
  final val INVOICE_TX_AMOUNT_MISMATCH = 2L
  final val NO_WITHDRAWABLE_TX = 3L
  final val INVALID_INVOICE = 4L
}

case class SwapInPaymentDenied(id: Long, reason: Long) extends SwapIn with ChainSwapMessage

case class ChainDeposit(id: Long, lnPaymentId: Option[String], lnStatus: Long, btcAddress: String, outIndex: Long, txid: String, amountSat: Long, depth: Long, stamp: Long) // (3) Periodic notifications

case class SwapInState(pending: List[ChainDeposit], ready: List[ChainDeposit], processing: List[ChainDeposit] = Nil) extends SwapIn with ChainSwapMessage // (3) Periodic notifications

sealed trait SwapOut // LN -> Chain

case object SwapOutRequest extends SwapOut with ChainSwapMessage

case class BlockTargetAndFee(blockTarget: Int, fee: Satoshi)

case class KeyedBlockTargetAndFee(feerates: List[BlockTargetAndFee], feerateKey: ByteVector32)

case class SwapOutFeerates(feerates: KeyedBlockTargetAndFee, providerCanHandle: Satoshi, minWithdrawable: Satoshi) extends SwapOut with ChainSwapMessage

case class SwapOutTransactionRequest(amount: Satoshi, btcAddress: String, blockTarget: Int, feerateKey: ByteVector32) extends SwapOut with ChainSwapMessage

case class SwapOutTransactionResponse(paymentRequest: String, amount: Satoshi, btcAddress: String, fee: Satoshi) extends SwapOut with ChainSwapMessage

object SwapOutTransactionDenied {
  final val INVALID_BITCOIN_ADDRESS = 1L
  final val UNKNOWN_CHAIN_FEERATES = 2L
  final val CAN_NOT_HANDLE_AMOUNT = 3L
  final val AMOUNT_TOO_SMALL = 4L
}

case class SwapOutTransactionDenied(btcAddress: String, reason: Long) extends SwapOut with ChainSwapMessage

// Trampoline status

sealed trait TrampolineStatus extends LightningMessage

case object TrampolineUndesired extends TrampolineStatus

object TrampolineOn {
  def default(minimumMsat: MilliSatoshi, cltvExpiryDelta: CltvExpiryDelta): TrampolineOn =
  // A relatively high (10 sat + 0.1%) and linear defaults in case if peer still wants to route while routing is undesired by us
    TrampolineOn(minimumMsat, maximumMsat = 1000000000L.msat, feeBaseMsat = 10000L.msat, feeProportionalMillionths = 1000L,
      exponent = 1.0, logExponent = 0.0, cltvExpiryDelta)
}

case class TrampolineOn(minimumMsat: MilliSatoshi, maximumMsat: MilliSatoshi, feeBaseMsat: MilliSatoshi,
                        feeProportionalMillionths: Long, exponent: Double, logExponent: Double,
                        cltvExpiryDelta: CltvExpiryDelta) extends TrampolineStatus
