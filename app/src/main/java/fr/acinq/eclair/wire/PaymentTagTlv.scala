package fr.acinq.eclair.wire

import scodec._
import scodec.codecs._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector


case class ShortPaymentTag(paymentSecret: ByteVector32, tag: Int)

case class FullPaymentTag(paymentHash: ByteVector32, paymentSecret: ByteVector32, tag: Int)

object PaymentTagTlv {
  final val LOCALLY_SENT = 0
  final val TRAMPLOINE_ROUTED = 1
  final val FINAL_INCOMING = 2

  case class EncryptedPaymentSecret(data: ByteVector) extends Tlv

  val encryptedPaymentSecretCodec = {
    varsizebinarydata withContext "data"
  }.as[EncryptedPaymentSecret]

  val shortPaymentTagCodec = {
    (bytes32 withContext "paymentSecret") ::
      (int32 withContext "tag")
  }.as[ShortPaymentTag]

  val codec: Codec[TlvStream.GenericTlvStream] = {
    val discriminatorCodec: DiscriminatorCodec[Tlv, UInt64] = discriminated.by(varint).typecase(TlvStream.paymentTag, encryptedPaymentSecretCodec)
    val prefixedTlvCodec: Codec[TlvStream.GenericTlvStream] = variableSizeBytesLong(value = TlvCodecs.tlvStream(discriminatorCodec), size = varintoverflow)

    fallback(provide(TlvStream.empty[Tlv]), prefixedTlvCodec).narrow(f = {
      case Left(emptyFallback) => Attempt.successful(emptyFallback)
      case Right(realStream) => Attempt.successful(realStream)
    }, g = Right.apply)
  }
}