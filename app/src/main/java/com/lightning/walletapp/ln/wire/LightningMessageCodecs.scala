package com.lightning.walletapp.ln.wire

import java.net._
import scodec.codecs._
import fr.acinq.eclair.UInt64.Conversions._
import scodec.bits.{BitVector, ByteVector}
import com.lightning.walletapp.ln.crypto.{Mac32, Sphinx}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}
import com.lightning.walletapp.ln.{LightningException, RevocationInfo}
import org.apache.commons.codec.binary.Base32
import fr.acinq.bitcoin.Crypto
import fr.acinq.eclair.UInt64
import scala.reflect.ClassTag
import java.math.BigInteger
import scala.util.Try


object LightningMessageCodecs { me =>
  type NodeAddressList = List[NodeAddress]
  type BitVectorAttempt = Attempt[BitVector]
  type LNMessageVector = Vector[LightningMessage]
  // True is sent localy, False is received from remote peer
  type LNDirectionalMessage = (LightningMessage, Boolean)
  type RedeemScriptAndSig = (ByteVector, ByteVector)
  type RGB = (Byte, Byte, Byte)

  def serialize(attempt: BitVectorAttempt): ByteVector = attempt match {
    case Attempt.Successful(binary) => ByteVector.view(binary.toByteArray)
    case Attempt.Failure(err) => throw new LightningException(err.message)
  }

  def deserialize(raw: ByteVector): LightningMessage =
    lightningMessageCodec decode BitVector(raw) match {
      case Attempt.Successful(decodedResult) => decodedResult.value
      case Attempt.Failure(err) => throw new LightningException(err.message)
    }

  def discriminatorWithDefault[A](discriminator: Codec[A], fallback: Codec[A]) = new Codec[A] {
    def encode(element: A) = discriminator encode element recoverWith { case _ => fallback encode element }
    def sizeBound = discriminator.sizeBound | fallback.sizeBound

    def decode(bv: BitVector) = discriminator decode bv recoverWith {
      case _: KnownDiscriminatorType[_]#UnknownDiscriminator => fallback decode bv
    }
  }

  // RGB <-> ByteVector
  private val bv2Rgb: PartialFunction[ByteVector, RGB] = {
    case ByteVector(red, green, blue, _*) => (red, green, blue)
  }

  private val rgb2Bv: PartialFunction[RGB, ByteVector] = {
    case (red, green, blue) => ByteVector(red, green, blue)
  }

  def der2wire(signature: ByteVector): ByteVector =
    Crypto decodeSignature signature match { case (r, s) =>
      val fixedR = Crypto fixSize ByteVector.view(r.toByteArray dropWhile 0.==)
      val fixedS = Crypto fixSize ByteVector.view(s.toByteArray dropWhile 0.==)
      fixedR ++ fixedS
    }

  def wire2der(sig: ByteVector): ByteVector = {
    val r = new BigInteger(1, sig.take(32).toArray)
    val s = new BigInteger(1, sig.takeRight(32).toArray)
    Crypto.encodeSignature(r, s) :+ 1.toByte
  }

  // Generic codecs

  val signature = Codec[ByteVector] (
    encoder = (der: ByteVector) => bytes(64).encode(me der2wire der),
    decoder = (wire: BitVector) => bytes(64).decode(wire).map(_ map wire2der)
  )

  val scalar = Codec[Scalar] (
    encoder = (scalar: Scalar) => bytes(32).encode(scalar.toBin),
    decoder = (wire: BitVector) => bytes(32).decode(wire).map(_ map Scalar.apply)
  )

  val point = Codec[Point] (
    encoder = (point: Point) => bytes(33).encode(point toBin true),
    decoder = (wire: BitVector) => bytes(33).decode(wire).map(_ map Point.apply)
  )

  val publicKey = Codec[PublicKey] (
    encoder = (publicKey: PublicKey) => bytes(33).encode(publicKey.value toBin true),
    decoder = (wire: BitVector) => bytes(33).decode(wire).map(_ map PublicKey.apply)
  )

  val ipv6address: Codec[Inet6Address] = bytes(16).exmap(
    bv => Attempt fromTry Try(Inet6Address.getByAddress(null, bv.toArray, null)),
    inet6Address => Attempt fromTry Try(ByteVector view inet6Address.getAddress)
  )

  private val ipv4address: Codec[Inet4Address] = bytes(4).xmap(
    bv => InetAddress.getByAddress(bv.toArray).asInstanceOf[Inet4Address],
    inet4Address => ByteVector.view(inet4Address.getAddress)
  )

  def base32(size: Int): Codec[String] = bytes(size).xmap(
    bv => (new Base32 encodeAsString bv.toArray).toLowerCase,
    address => ByteVector.view(new Base32 decode address.toUpperCase)
  )

  def minimalValue(codec: Codec[UInt64], min: UInt64): Codec[UInt64] = codec.exmap(f = {
    case value if value < min => Attempt failure Err("Value is not minimally encoded")
    case value => Attempt successful value
  }, Attempt.successful)

  val uint64Overflow: Codec[Long] = int64.narrow(f = {
    case value if value < 0 => Attempt failure Err(s"Overflow $value")
    case value => Attempt successful value
  }, identity)

  val uint64: Codec[UInt64] = bytes(8).xmap(UInt64.apply, _.toByteVector padLeft 8)

  val varint: Codec[UInt64] = {
    val large = minimalValue(uint64, 0x100000000L)
    val medium = minimalValue(uint32.xmap(UInt64.apply, _.toBigInt.toLong), 0x10000)
    val small = minimalValue(uint16.xmap(int => UInt64(int), _.toBigInt.toInt), 0xFD)
    val default: Codec[UInt64] = uint8L.xmap(int => UInt64(int), _.toBigInt.toInt)

    discriminatorWithDefault(discriminated[UInt64].by(uint8L)
      .\(0xFF) { case value if value >= UInt64(0x100000000L) => value } (large)
      .\(0xFE) { case value if value >= UInt64(0x10000) => value } (medium)
      .\(0xFD) { case value if value >= UInt64(0xFD) => value } (small),
      default)
  }

  val varintoverflow: Codec[Long] = varint.narrow(f = {
    case value if value > UInt64(Long.MaxValue) => Attempt failure Err(s"Overflow $value")
    case value => Attempt successful value.toBigInt.toLong
  }, UInt64.apply)

  val bytes32: Codec[ByteVector] = limitedSizeBytes(codec = bytesStrict(32).xmap(identity, identity), limit = 32)
  val zeropaddedstring: Codec[String] = fixedSizeBytes(32, utf8).xmap(_.takeWhile(_ != '\u0000'), identity)
  val varsizebinarydataLong: Codec[ByteVector] = variableSizeBytesLong(uint32, bytes)
  val varsizebinarydata: Codec[ByteVector] = variableSizeBytes(uint16, bytes)
  val rgb: Codec[RGB] = bytes(3).xmap(bv2Rgb, rgb2Bv)
  val txvec = vectorOfN(uint16, varsizebinarydata)

  def nodeaddress: Codec[NodeAddress] =
    discriminated[NodeAddress].by(uint8)
      .typecase(cr = (ipv4address :: uint16).as[IPv4], tag = 1)
      .typecase(cr = (ipv6address :: uint16).as[IPv6], tag = 2)
      .typecase(cr = (base32(10) :: uint16).as[Tor2], tag = 3)
      .typecase(cr = (base32(35) :: uint16).as[Tor3], tag = 4)
      .typecase(cr = (zeropaddedstring :: uint16).as[Domain], tag = 5)

  def prependmac[A](codec: Codec[A], mac32: Mac32) = Codec[A] (
    (a: A) => codec.encode(a).map(bits1 => mac32.mac(bits1.toByteVector).bits ++ bits1),
    (encodedMacBits: BitVector) => (bytes32 withContext "mac").decode(encodedMacBits) match {
      case Attempt.Successful(dr) if mac32.verify(dr.value, dr.remainder.toByteVector) => codec.decode(dr.remainder)
      case Attempt.Successful(invalidDr) => Attempt Failure scodec.Err(s"Invalid mac detected: $invalidDr")
      case Attempt.Failure(err) => Attempt Failure err
    }
  )

  // Tlv

  private val genericTlv = (varint withContext "tag") :: variableSizeBytesLong(varintoverflow, bytes)
  private val genericTlvCodec = genericTlv.as[GenericTlv].exmap(validateGenericTlv, validateGenericTlv)

  private def validateGenericTlv(generic: GenericTlv): Attempt[GenericTlv] =
    if (generic.tag.toBigInt % 2 == 0) Attempt Failure Err("Unknown even tlv type")
    else Attempt Successful generic

  private def variableSizeUInt64(size: Int, min: Long): Codec[UInt64] =
    minimalValue(bytes(size).xmap(f => UInt64(f), _.toByteVector takeRight size), min)

  val tu64: Codec[UInt64] = discriminated[UInt64].by(uint8)
    .\(0x00) { case value if value < 0x01 => value } (variableSizeUInt64(0, 0x00))
    .\(0x01) { case value if value < 0x0100 => value } (variableSizeUInt64(1, 0x01))
    .\(0x02) { case value if value < 0x010000 => value } (variableSizeUInt64(2, 0x0100))
    .\(0x03) { case value if value < 0x01000000 => value } (variableSizeUInt64(3, 0x010000))
    .\(0x04) { case value if value < 0x0100000000L => value } (variableSizeUInt64(4, 0x01000000))
    .\(0x05) { case value if value < 0x010000000000L => value } (variableSizeUInt64(5, 0x0100000000L))
    .\(0x06) { case value if value < 0x01000000000000L => value } (variableSizeUInt64(6, 0x010000000000L))
    .\(0x07) { case value if value < 0x0100000000000000L => value } (variableSizeUInt64(7, 0x01000000000000L))
    .\(0x08) { case value if value <= UInt64.MaxValue => value } (variableSizeUInt64(8, 0x0100000000000000L))

  val tu64Overflow: Codec[Long] = tu64.exmap(
    u => if (u <= Long.MaxValue) Attempt Successful u.toBigInt.toLong else Attempt Failure Err(s"overflow for value $u"),
    l => if (l >= 0) Attempt Successful UInt64(l) else Attempt Failure Err(s"uint64 must be positive, actual is $l")
  )

  val tu32: Codec[Long] = tu64.exmap(f = {
    case value if value > 0xFFFFFFFFL => Attempt Failure Err("tu32 overflow")
    case value => Attempt Successful value.toBigInt.toLong
  }, long => Attempt Successful long)

  val tu16: Codec[Int] = tu32.exmap(f = {
    case value if value > 0xFFFF => Attempt Failure Err("tu16 overflow")
    case value => Attempt Successful value.toBigInt.toInt
  }, long => Attempt Successful long)

  def tlvStream[T <: Tlv](codec: DiscriminatorCodec[T, UInt64]): Codec[TlvStream[T]] = {
    val withFallback = discriminatorFallback(genericTlvCodec, codec)

    list(withFallback).exmap(
      recordsEitherTlvList => {
        val knownTags = for (Right(known) <- recordsEitherTlvList) yield known
        val unknownTags = for (Left(unknown) <- recordsEitherTlvList) yield unknown

        val tags = for (record <- recordsEitherTlvList) yield record match {
          case Right(tlv) => codec.encode(tlv).flatMap(varint.decode).require.value
          case Left(unknownTlv) => unknownTlv.tag
        }

        if (tags.length != tags.distinct.length) Attempt Failure Err("Tlv streams must not contain duplicate records")
        else if (tags != tags.sorted) Attempt Failure Err("Tlv records must be ordered by monotonically-increasing types")
        else Attempt Successful TlvStream(knownTags, unknownTags)
      },

      stream => {
        val knownRecords = for (known <- stream.records) yield Right(known)
        val unknownRecords = for (unknown <- stream.unknown) yield Left(unknown)
        val records = (knownRecords ++ unknownRecords).toList

        val tags = for (record <- records) yield record match {
          case Right(tlv) => codec.encode(tlv).flatMap(varint.decode).require.value
          case Left(unknownTlv) => unknownTlv.tag
        }

        if (tags.length != tags.distinct.length) Attempt Failure Err("Tlv streams must not contain duplicate records")
        else Attempt Successful tags.zip(records).sortBy { case (tag, _) => tag }.map { case (_, record) => record }
      }
    )
  }

  // Onion

  def onionRoutingPacketCodec(payloadLength: Int) = {
    (uint8 withContext "version") ::
      (bytes(33) withContext "publicKey") ::
      (bytes(payloadLength) withContext "payload") ::
      (bytes32 withContext "hmac")
  }.as[OnionRoutingPacket]

  val paymentOnionPacketCodec: Codec[OnionRoutingPacket] = onionRoutingPacketCodec(Sphinx.PaymentPacket.PayloadLength)

  val payloadLengthDecoder = Decoder[Long] { bits: BitVector =>
    varintoverflow.decode(bits) map { decResult: DecodeResult[Long] =>
      val payload = decResult.value + (bits.length - decResult.remainder.length) / 8
      DecodeResult(payload, decResult.remainder)
    }
  }

  private val outgoingCltv: Codec[OnionTlv.OutgoingCltv] = (tu32 withContext "cltv").xmap(OnionTlv.OutgoingCltv, _.cltv)
  private val amountToForward: Codec[OnionTlv.AmountToForward] = (tu64Overflow withContext "amountMsat").xmap(OnionTlv.AmountToForward, _.amountMsat)
  private val outgoingChannelId: Codec[OnionTlv.OutgoingChannelId] = variableSizeBytesLong(varintoverflow, int64 withContext "shortChannelId").as[OnionTlv.OutgoingChannelId]

  private val paymentData: Codec[OnionTlv.PaymentData] = {
    val base = (bytes32 withContext "secret") :: (tu64Overflow withContext "totalAmountMsat")
    variableSizeBytesLong(varintoverflow, base).as[OnionTlv.PaymentData]
  }

  private val onionTlvCodec =
    discriminated[OnionTlv].by(varint)
      .typecase(UInt64(2), amountToForward)
      .typecase(UInt64(4), outgoingCltv)
      .typecase(UInt64(6), outgoingChannelId)
      .typecase(UInt64(8), paymentData)

  private val tlvPerHopPayloadCodec: Codec[OnionTlv.Stream] =
    variableSizeBytesLong(value = tlvStream(onionTlvCodec), size = varintoverflow)

  private val legacyRelayPerHopPayloadCodec: Codec[RelayLegacyPayload] = {
    (constant(ByteVector fromByte 0) withContext "realm") ::
      (uint64Overflow withContext "short_channel_id") ::
      (uint64Overflow withContext "amt_to_forward") ::
      (uint32 withContext "outgoing_cltv_value") ::
      (ignore(8 * 12) withContext "unused_with_v0_version_on_header")
  }.as[RelayLegacyPayload]

  private val legacyFinalPerHopPayloadCodec: Codec[FinalLegacyPayload] = {
    (constant(ByteVector fromByte 0) withContext "realm") ::
      (ignore(8 * 8) withContext "short_channel_id") ::
      (uint64Overflow withContext "amount") ::
      (uint32 withContext "expiry") ::
      (ignore(8 * 12) withContext "unused_with_v0_version_on_header")
  }.as[FinalLegacyPayload]

  case class MissingRequiredTlv(tag: UInt64) extends Err { me =>
    override def message = "Onion per-hop payload is invalid"
    override def pushContext(ctx: String): Err = me
    override def context: List[String] = Nil
  }

  val relayPerHopPayloadCodec: Codec[RelayPayload] =
    fallback(tlvPerHopPayloadCodec, legacyRelayPerHopPayloadCodec).narrow(
      f = {
        case Left(tlvs) if tlvs.get[OnionTlv.OutgoingChannelId].isEmpty => Attempt failure MissingRequiredTlv(6)
        case Left(tlvs) if tlvs.get[OnionTlv.AmountToForward].isEmpty => Attempt failure MissingRequiredTlv(2)
        case Left(tlvs) if tlvs.get[OnionTlv.OutgoingCltv].isEmpty => Attempt failure MissingRequiredTlv(4)
        case Left(tlvs) => Attempt successful RelayTlvPayload(tlvs)
        case Right(legacy) => Attempt successful legacy
      }, g = {
        case legacy: RelayLegacyPayload => Right(legacy)
        case RelayTlvPayload(tlvs) => Left(tlvs)
      }
    )

  val finalPerHopPayloadCodec: Codec[FinalPayload] =
    fallback(tlvPerHopPayloadCodec, legacyFinalPerHopPayloadCodec).narrow(
      f = {
        case Left(tlvs) if tlvs.get[OnionTlv.AmountToForward].isEmpty => Attempt failure MissingRequiredTlv(2)
        case Left(tlvs) if tlvs.get[OnionTlv.OutgoingCltv].isEmpty => Attempt failure MissingRequiredTlv(4)
        case Left(tlvs) => Attempt successful FinalTlvPayload(tlvs)
        case Right(legacy) => Attempt successful legacy
      }, g = {
        case legacy: FinalLegacyPayload => Right(legacy)
        case FinalTlvPayload(tlvs) => Left(tlvs)
      }
    )

  // LN messages

  private val init = (varsizebinarydata withContext "globalFeatures") :: (varsizebinarydata withContext "localFeatures")
  private val ping = (uint16 withContext "pongLength") :: (varsizebinarydata withContext "data")
  private val pong = varsizebinarydata withContext "data"

  val channelReestablish =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "nextLocalCommitmentNumber") ::
      (uint64Overflow withContext "nextRemoteRevocationNumber") ::
      (optional(bitsRemaining, scalar) withContext "yourLastPerCommitmentSecret") ::
      (optional(bitsRemaining, point) withContext "myCurrentPerCommitmentPoint")

  val channelFlagsCodec = (byte withContext "flags").as[ChannelFlags]

  val errorCodec = {
    (bytes32 withContext "channelId") ::
      (varsizebinarydata withContext "data")
  }.as[Error]

  private val openChannel =
    (bytes32 withContext "chainHash") ::
      (bytes32 withContext "temporaryChannelId") ::
      (uint64Overflow withContext "fundingSatoshis") ::
      (uint64Overflow withContext "pushMsat") ::
      (uint64Overflow withContext "dustLimitSatoshis") ::
      (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "channelReserveSatoshis") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "feeratePerKw") ::
      (uint16 withContext "toSelfDelay") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (publicKey withContext "fundingPubkey") ::
      (point withContext "revocationBasepoint") ::
      (point withContext "paymentBasepoint") ::
      (point withContext "delayedPaymentBasepoint") ::
      (point withContext "htlcBasepoint") ::
      (point withContext "firstPerCommitmentPoint") ::
      (channelFlagsCodec withContext "channelFlags")

  val acceptChannelCodec = {
    (bytes32 withContext "temporaryChannelId") ::
      (uint64Overflow withContext "dustLimitSatoshis") ::
      (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "channelReserveSatoshis") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "minimumDepth") ::
      (uint16 withContext "toSelfDelay") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (publicKey withContext "fundingPubkey") ::
      (point withContext "revocationBasepoint") ::
      (point withContext "paymentBasepoint") ::
      (point withContext "delayedPaymentBasepoint") ::
      (point withContext "htlcBasepoint") ::
      (point withContext "firstPerCommitmentPoint")
  }.as[AcceptChannel]

  private val fundingCreated =
    (bytes32 withContext "temporaryChannelId") ::
      (bytes32 withContext "txid") ::
      (uint16 withContext "fundingOutputIndex") ::
      (signature withContext "signature")

  private val fundingSigned =
    (bytes32 withContext "channelId") ::
      (signature withContext "signature")

  val fundingLockedCodec = {
    (bytes32 withContext "channelId") ::
      (point withContext "nextPerCommitmentPoint")
  }.as[FundingLocked]

  val shutdownCodec = {
    (bytes32 withContext "channelId") ::
      (varsizebinarydata withContext "scriptPubKey")
  }.as[Shutdown]

  val closingSignedCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "feeSatoshis") ::
      (signature withContext "signature")
  }.as[ClosingSigned]

  val updateAddHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (uint64Overflow withContext "amountMsat") ::
      (bytes32 withContext "paymentHash") ::
      (uint32 withContext "expiry") ::
      (paymentOnionPacketCodec withContext "onionRoutingPacket")
  }.as[UpdateAddHtlc]

  val updateFulfillHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "paymentPreimage")
  }.as[UpdateFulfillHtlc]

  val updateFailHtlcCodec = {
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (varsizebinarydata withContext "reason")
  }.as[UpdateFailHtlc]

  private val updateFailMalformedHtlc =
    (bytes32 withContext "channelId") ::
      (uint64Overflow withContext "id") ::
      (bytes32 withContext "onionHash") ::
      (uint16 withContext "failureCode")

  val commitSigCodec = {
    (bytes32 withContext "channelId") ::
      (signature withContext "signature") ::
      (listOfN(uint16, signature) withContext "htlcSignatures")
  }.as[CommitSig]

  private val revokeAndAck =
    (bytes32 withContext "channelId") ::
      (scalar withContext "perCommitmentSecret") ::
      (point withContext "nextPerCommitmentPoint")

  private val updateFee =
    (bytes32 withContext "channelId") ::
      (uint32 withContext "feeratePerKw")

  private val announcementSignatures =
    (bytes32 withContext "channelId") ::
      (int64 withContext "shortChannelId") ::
      (signature withContext "nodeSignature") ::
      (signature withContext "bitcoinSignature")

  val channelAnnouncementWitness =
    (varsizebinarydata withContext "features") ::
      (bytes32 withContext "chainHash") ::
      (int64 withContext "shortChannelId") ::
      (publicKey withContext "nodeId1") ::
      (publicKey withContext "nodeId2") ::
      (publicKey withContext "bitcoinKey1") ::
      (publicKey withContext "bitcoinKey2") ::
      (bytes withContext "unknownFields")

  private val channelAnnouncement =
    (signature withContext "nodeSignature1") ::
      (signature withContext "nodeSignature2") ::
      (signature withContext "bitcoinSignature1") ::
      (signature withContext "bitcoinSignature2") ::
      channelAnnouncementWitness

  val nodeAnnouncementWitness =
    (varsizebinarydata withContext "features") ::
      (uint32 withContext "timestamp") ::
      (publicKey withContext "nodeId") ::
      (rgb withContext "rgbColor") ::
      (zeropaddedstring withContext "alias") ::
      (variableSizeBytes(value = list(nodeaddress), size = uint16) withContext "addresses") ::
      (bytes withContext "unknownFields")

  val channelUpdateWitness =
    (bytes32 withContext "chainHash") ::
      (int64 withContext "shortChannelId") ::
      (uint32 withContext "timestamp") ::
      (byte withContext "messageFlags").flatPrepend { messageFlags =>
        (byte withContext "channelFlags" ) ::
          (uint16 withContext "cltvExpiryDelta") ::
          (uint64Overflow withContext "htlcMinimumMsat") ::
          (uint32 withContext "feeBaseMsat") ::
          (uint32 withContext "feeProportionalMillionths") ::
          (conditional(included = (messageFlags & 1) != 0, uint64Overflow) withContext "htlcMaximumMsat") ::
          (bytes withContext "unknownFields")
      }

  val nodeAnnouncementCodec = {
    (signature withContext "signature") ::
      nodeAnnouncementWitness
  }.as[NodeAnnouncement]

  val channelUpdateCodec = {
    (signature withContext "signature") ::
      channelUpdateWitness
  }.as[ChannelUpdate]

  val queryChannelRangeCodec = {
    (bytes32 withContext "chainHash") ::
      (uint32 withContext "firstBlockNum") ::
      (uint32 withContext "numberOfBlocks")
  }.as[QueryChannelRange]

  val gossipTimestampFilterCodec = {
    (bytes32 withContext "chainHash") ::
      (uint32 withContext "firstTimestamp") ::
      (uint32 withContext "timestampRange")
  }.as[GossipTimestampFilter]

  // Hosted messages codecs

  val invokeHostedChannelCodec = {
    (bytes32 withContext "chainHash") ::
      (varsizebinarydata withContext "refundScriptPubKey") ::
      (varsizebinarydata withContext "secret")
  }.as[InvokeHostedChannel]

  val initHostedChannelCodec = {
    (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (uint64Overflow withContext "channelCapacityMsat") ::
      (uint16 withContext "liabilityDeadlineBlockdays") ::
      (uint64Overflow withContext "minimalOnchainRefundAmountSatoshis") ::
      (uint64Overflow withContext "initialClientBalanceMsat") ::
      (varsizebinarydata withContext "features")
  }.as[InitHostedChannel]

  val lastCrossSignedStateCodec = {
    (varsizebinarydata withContext "refundScriptPubKey") ::
      (initHostedChannelCodec withContext "initHostedChannel") ::
      (uint32 withContext "blockDay") ::
      (uint64Overflow withContext "localBalanceMsat") ::
      (uint64Overflow withContext "remoteBalanceMsat") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (listOfN(uint16, updateAddHtlcCodec) withContext "incomingHtlcs") ::
      (listOfN(uint16, updateAddHtlcCodec) withContext "outgoingHtlcs") ::
      (signature withContext "remoteSigOfLocal") ::
      (signature withContext "localSigOfRemote")
  }.as[LastCrossSignedState]

  val stateUpdateCodec = {
    (uint32 withContext "blockDay") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (signature withContext "localSigOfRemoteLCSS") ::
      (bool withContext "isTerminal")
  }.as[StateUpdate]

  val stateOverrideCodec = {
    (uint32 withContext "blockDay") ::
      (uint64Overflow withContext "localBalanceMsat") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (signature withContext "localSigOfRemoteLCSS")
  }.as[StateOverride]

  val lightningMessageCodec =
    discriminated[LightningMessage].by(uint16)
      .typecase(cr = init.as[Init], tag = 16)
      .typecase(cr = errorCodec, tag = 17)
      .typecase(cr = ping.as[Ping], tag = 18)
      .typecase(cr = pong.as[Pong], tag = 19)
      .typecase(cr = openChannel.as[OpenChannel], tag = 32)
      .typecase(cr = acceptChannelCodec, tag = 33)
      .typecase(cr = fundingCreated.as[FundingCreated], tag = 34)
      .typecase(cr = fundingSigned.as[FundingSigned], tag = 35)
      .typecase(cr = fundingLockedCodec, tag = 36)
      .typecase(cr = shutdownCodec, tag = 38)
      .typecase(cr = closingSignedCodec, tag = 39)
      .typecase(cr = updateAddHtlcCodec, tag = 128)
      .typecase(cr = updateFulfillHtlcCodec, tag = 130)
      .typecase(cr = updateFailHtlcCodec, tag = 131)
      .typecase(cr = commitSigCodec, tag = 132)
      .typecase(cr = revokeAndAck.as[RevokeAndAck], tag = 133)
      .typecase(cr = updateFee.as[UpdateFee], tag = 134)
      .typecase(cr = updateFailMalformedHtlc.as[UpdateFailMalformedHtlc], tag = 135)
      .typecase(cr = channelReestablish.as[ChannelReestablish], tag = 136)
      .typecase(cr = channelAnnouncement.as[ChannelAnnouncement], tag = 256)
      .typecase(cr = nodeAnnouncementCodec, tag = 257)
      .typecase(cr = channelUpdateCodec, tag = 258)
      .typecase(cr = announcementSignatures.as[AnnouncementSignatures], tag = 259)
      .typecase(cr = queryChannelRangeCodec, tag = 263)
      .typecase(cr = gossipTimestampFilterCodec, tag = 265)
      .typecase(cr = invokeHostedChannelCodec, tag = 65535)
      .typecase(cr = initHostedChannelCodec, tag = 65533)
      .typecase(cr = lastCrossSignedStateCodec, tag = 65531)
      .typecase(cr = stateUpdateCodec, tag = 65529)
      .typecase(cr = stateOverrideCodec, tag = 65527)

  // Not in a spec

  val hopCodec = {
    (publicKey withContext "nodeId") ::
      (int64 withContext "shortChannelId") ::
      (uint16 withContext "cltvExpiryDelta") ::
      (uint64Overflow withContext "htlcMinimumMsat") ::
      (uint32 withContext "feeBaseMsat") ::
      (uint32 withContext "feeProportionalMillionths")
  }.as[Hop]

  val revocationInfoCodec = {
    (listOfN(uint16, varsizebinarydata ~ signature) withContext "redeemScriptsToSigs") ::
      (optional(bool, signature) withContext "claimMainTxSig") ::
      (optional(bool, signature) withContext "claimPenaltyTxSig") ::
      (uint64Overflow withContext "feeRate") ::
      (uint64Overflow withContext "dustLimit") ::
      (varsizebinarydata withContext "finalScriptPubKey") ::
      (uint16 withContext "toSelfDelay") ::
      (publicKey withContext "localPubKey") ::
      (publicKey withContext "remoteRevocationPubkey") ::
      (publicKey withContext "remoteDelayedPaymentKey")
  }.as[RevocationInfo]

  val hostedStateCodec = {
    (bytes32 withContext "channelId") ::
      (vectorOfN(uint16, lightningMessageCodec) withContext "nextLocalUpdates") ::
      (vectorOfN(uint16, lightningMessageCodec) withContext "nextRemoteUpdates") ::
      (lastCrossSignedStateCodec withContext "lastCrossSignedState")
  }.as[HostedState]

  val aesZygoteCodec = {
    (uint16 withContext "v") ::
      (varsizebinarydataLong withContext "iv") ::
      (varsizebinarydataLong withContext "ciphertext")
  }.as[AESZygote]

  val cerberusPayloadCodec = {
    (vectorOfN(uint16, aesZygoteCodec) withContext "payloads") ::
      (vectorOfN(uint16, zeropaddedstring) withContext "halfTxIds")
  }.as[CerberusPayload]
}

// TLV

trait Tlv
case class GenericTlv(tag: UInt64, value: ByteVector) extends Tlv
case class TlvStream[T <: Tlv](records: Traversable[T], unknown: Traversable[GenericTlv] = Nil) {
  def get[R <: T : ClassTag]: Option[R] = records.collectFirst { case record: R => record }
}

object TlvStream {
  type BaseStream = TlvStream[Tlv]
  val empty: BaseStream = TlvStream[Tlv](records = Nil)
  def apply[T <: Tlv](records: T*): TlvStream[T] = TlvStream(records)
}

// Onion

sealed trait OnionTlv extends Tlv
case class OnionRoutingPacket(version: Int, publicKey: ByteVector, payload: ByteVector, hmac: ByteVector)

object OnionTlv {
  type Stream = TlvStream[OnionTlv]
  case class OutgoingChannelId(shortChannelId: Long) extends OnionTlv // Id of the channel to use to forward a payment to the next node
  case class PaymentData(secret: ByteVector, totalAmountMsat: Long) extends OnionTlv // Bolt 11 payment details for the last node
  case class AmountToForward(amountMsat: Long) extends OnionTlv // Amount to forward to the next node
  case class OutgoingCltv(cltv: Long) extends OnionTlv
}

sealed trait PerHopPayloadFormat
sealed trait PerHopPayload { def encode: ByteVector }
sealed trait FinalPayload extends PerHopPayload with PerHopPayloadFormat { me =>
  def encode = LightningMessageCodecs.finalPerHopPayloadCodec.encode(me).require.toByteVector
  val paymentSecretOpt: Option[ByteVector]
  val totalAmountMsat: Long
  val amountMsat: Long
  val cltvExpiry: Long
}

sealed trait RelayPayload extends PerHopPayload with PerHopPayloadFormat { me =>
  def encode = LightningMessageCodecs.relayPerHopPayloadCodec.encode(me).require.toByteVector
  val amountToForwardMsat: Long
  val outgoingCltv: Long
}

sealed trait LegacyFormat extends PerHopPayloadFormat

case class RelayLegacyPayload(outgoingChannelId: Long, amountToForwardMsat: Long, outgoingCltv: Long) extends RelayPayload with LegacyFormat

case class RelayTlvPayload(records: OnionTlv.Stream) extends RelayPayload {
  override val amountToForwardMsat = records.get[OnionTlv.AmountToForward].get.amountMsat
  override val outgoingCltv = records.get[OnionTlv.OutgoingCltv].get.cltv
}

case class FinalLegacyPayload(amountMsat: Long, cltvExpiry: Long) extends FinalPayload with LegacyFormat {
  override val totalAmountMsat = amountMsat
  override val paymentSecretOpt = None
}

case class FinalTlvPayload(records: OnionTlv.Stream) extends FinalPayload {
  override val paymentSecretOpt = records.get[OnionTlv.PaymentData].map(_.secret)
  override val amountMsat = records.get[OnionTlv.AmountToForward].get.amountMsat
  override val cltvExpiry = records.get[OnionTlv.OutgoingCltv].get.cltv

  override val totalAmountMsat = records.get[OnionTlv.PaymentData] match {
    case Some(paymentData) if paymentData.totalAmountMsat == 0L => amountMsat
    case Some(paymentData) => paymentData.totalAmountMsat
    case None => amountMsat
  }
}