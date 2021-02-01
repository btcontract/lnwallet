package com.lightning.walletapp.ln.wire

import scodec.codecs._
import LightningMessageCodecs._
import com.lightning.walletapp.ln.wire.FailureMessageCodecs._
import com.lightning.walletapp.ln.crypto.Mac32
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import scodec.Attempt


sealed trait FailureMessage { me =>
  lazy val code: Int = failureMessageCodec.encode(me).flatMap(uint16.decode).require.value
}

sealed trait BadOnion extends FailureMessage { def onionHash: ByteVector }
sealed trait Update extends FailureMessage { def update: ChannelUpdate }
sealed trait Perm extends FailureMessage
sealed trait Node extends FailureMessage

sealed trait UnknownFailureMessage extends FailureMessage {
  override def equals(another: Any): Boolean = another match {
    case that: UnknownFailureMessage => that.code == code
    case _ => false
  }
}

case object InvalidRealm extends Perm
case object UnknownNextPeer extends Perm
case object PermanentChannelFailure extends Perm
case object RequiredChannelFeatureMissing extends Perm
case class IncorrectOrUnknownPaymentDetails(amountMsat: Long, height: Long) extends Perm

case object TemporaryNodeFailure extends Node
case object PermanentNodeFailure extends Perm with Node
case object RequiredNodeFeatureMissing extends Perm with Node

case object ExpiryTooFar extends FailureMessage
case object PaymentTimeout extends FailureMessage
case class FinalIncorrectCltvExpiry(expiry: Long) extends FailureMessage
case class FinalIncorrectHtlcAmount(amountMsat: Long) extends FailureMessage

case class InvalidOnionVersion(onionHash: ByteVector) extends BadOnion with Perm
case class InvalidOnionHmac(onionHash: ByteVector) extends BadOnion with Perm
case class InvalidOnionKey(onionHash: ByteVector) extends BadOnion with Perm
case class InvalidOnionPayload(tag: UInt64, offset: Int) extends Perm

case class AmountBelowMinimum(amountMsat: Long, update: ChannelUpdate) extends Update
case class ChannelDisabled(messageFlags: Byte, channelFlags: Byte, update: ChannelUpdate) extends Update
case class FeeInsufficient(amountMsat: Long, update: ChannelUpdate) extends Update
case class IncorrectCltvExpiry(expiry: Long, update: ChannelUpdate) extends Update
case class TemporaryChannelFailure(update: ChannelUpdate) extends Update
case class ExpiryTooSoon(update: ChannelUpdate) extends Update

object FailureMessageCodecs {
  private val invalidOnionPayload = (varint withContext "tag") :: (uint16 withContext "offset")
  private val channelUpdateCodecWithType = lightningMessageCodec.narrow[ChannelUpdate](Attempt successful _.asInstanceOf[ChannelUpdate], identity)
  private val channelUpdateWithLengthCodec = variableSizeBytes(value = choice(channelUpdateCodecWithType, channelUpdateCodec), size = uint16)
  private val disabled = (byte withContext "messageFlags") :: (byte withContext "channelFlags") :: channelUpdateWithLengthCodec
  private val wrongAmount = (uint64Overflow withContext "amountMsat") :: channelUpdateWithLengthCodec
  private val wrongExpiry = (uint32 withContext "expiry") :: channelUpdateWithLengthCodec

  private val incorrectOrUnknownPaymentDetails =
    (withDefaultValue(optional(bitsRemaining, uint64Overflow), 0L) withContext "amountMsat") ::
      (withDefaultValue(optional(bitsRemaining, uint32), 0L) withContext "height")

  val BADONION = 0x8000
  val UPDATE = 0x1000
  val PERM = 0x4000
  val NODE = 0x2000

  val failureMessageCodec =
    discriminatorWithDefault(
      discriminated[FailureMessage].by(uint16)
        .typecase(cr = provide(InvalidRealm), tag = PERM | 1)
        .typecase(cr = provide(TemporaryNodeFailure), tag = NODE | 2)
        .typecase(cr = provide(PermanentNodeFailure), tag = PERM | NODE | 2)
        .typecase(cr = provide(RequiredNodeFeatureMissing), tag = PERM | NODE | 3)
        .typecase(cr = bytes32.as[InvalidOnionVersion], tag = BADONION | PERM | 4)
        .typecase(cr = bytes32.as[InvalidOnionHmac], tag = BADONION | PERM | 5)
        .typecase(cr = bytes32.as[InvalidOnionKey], tag = BADONION | PERM | 6)
        .typecase(cr = channelUpdateWithLengthCodec.as[TemporaryChannelFailure], tag = UPDATE | 7)
        .typecase(cr = provide(PermanentChannelFailure), tag = PERM | 8)
        .typecase(cr = provide(RequiredChannelFeatureMissing), tag = PERM | 9)
        .typecase(cr = provide(UnknownNextPeer), tag = PERM | 10)
        .typecase(cr = wrongAmount.as[AmountBelowMinimum], tag = UPDATE | 11)
        .typecase(cr = wrongAmount.as[FeeInsufficient], tag = UPDATE | 12)
        .typecase(cr = wrongExpiry.as[IncorrectCltvExpiry], tag = UPDATE | 13)
        .typecase(cr = channelUpdateWithLengthCodec.as[ExpiryTooSoon], tag = UPDATE | 14)
        .typecase(cr = incorrectOrUnknownPaymentDetails.as[IncorrectOrUnknownPaymentDetails], tag = PERM | 15)
        .typecase(cr = (uint32 withContext "expiry").as[FinalIncorrectCltvExpiry], tag = 18)
        .typecase(cr = (uint64Overflow withContext "amountMsat").as[FinalIncorrectHtlcAmount], tag = 19)
        .typecase(cr = disabled.as[ChannelDisabled], tag = UPDATE | 20)
        .typecase(cr = provide(ExpiryTooFar), tag = 21)
        .typecase(cr = invalidOnionPayload.as[InvalidOnionPayload], tag = PERM | 22)
        .typecase(cr = provide(PaymentTimeout), tag = 23),
      uint16.xmap(unknownFailureFromCode(_).asInstanceOf[FailureMessage], (_: FailureMessage).code)
    )

  def unknownFailureFromCode(code: Int) = code match {
    case fc if (fc & PERM) != 0 && (fc & NODE) != 0 => new UnknownFailureMessage with Perm with Node { override lazy val code = fc }
    case fc if (fc & NODE) != 0 => new UnknownFailureMessage with Node { override lazy val code = fc }
    case fc if (fc & PERM) != 0 => new UnknownFailureMessage with Perm { override lazy val code = fc }
    case fc => new UnknownFailureMessage { override lazy val code = fc }
  }

  /**
    * An onion-encrypted failure from an intermediate node:
    * +----------------+----------------------------------+-----------------+----------------------+-----+
    * | HMAC(32 bytes) | failure message length (2 bytes) | failure message | pad length (2 bytes) | pad |
    * +----------------+----------------------------------+-----------------+----------------------+-----+
    * with failure message length + pad length = 256
    */

  def failureOnionCodec(mac: Mac32) = prependmac(paddedFixedSizeBytesDependent(260,
    variableSizeBytes(value = failureMessageCodec, size = uint16) withContext "failureMessage",
    nBits => variableSizeBytes(value = ignore(nBits - 2 * 8), size = uint16) withContext "padding"
  ).as[FailureMessage], mac)
}