package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import fr.acinq.bitcoin.Bech32._
import fr.acinq.bitcoin.Crypto._
import fr.acinq.bitcoin.Protocol._
import fr.acinq.eclair.crypto.BitStream._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.PaymentRequest._
import com.lightning.walletapp.ln.RoutingInfoTag._
import com.lightning.walletapp.ln.crypto.MultiStreamUtils._
import scodec.bits.{BitVector, ByteVector}
import com.lightning.walletapp.ln.wire.Hop
import fr.acinq.eclair.crypto.BitStream
import java.nio.ByteOrder.BIG_ENDIAN
import java.math.BigInteger
import scala.util.Try


sealed trait Tag {
  def toInt5s: Bytes
  def encode(ints: Bytes, v: Char): Bytes =
    Array(Bech32 map v, (ints.length / 32).toByte,
      (ints.length % 32).toByte) ++ ints
}

case class PaymentHashTag(hash: ByteVector) extends Tag {
  def toInt5s = encode(Bech32 eight2five hash.toArray, 'p')
}

case class PaymentSecretTag(secret: ByteVector) extends Tag {
  def toInt5s = encode(Bech32 eight2five secret.toArray, 's')
}

case class DescriptionTag(description: String) extends Tag {
  def toInt5s = encode(Bech32 eight2five description.getBytes, 'd')
}

case class DescriptionHashTag(hash: ByteVector) extends Tag {
  def toInt5s = encode(Bech32 eight2five hash.toArray, 'h')
}

case class FallbackAddressTag(version: Byte, hash: ByteVector) extends Tag {
  def toInt5s = encode(version +: Bech32.eight2five(hash.toArray), 'f')
}

object FallbackAddressTag { me =>
  def apply(address: String): FallbackAddressTag = {
    val base58Try = Try(me fromBase58Address address)
    val bech32Try = Try(me fromBech32Address address)
    bech32Try.orElse(base58Try).get
  }

  def fromBase58Address(address: String) = Base58Check decode address match {
    case Base58.Prefix.PubkeyAddressTestnet \ hash => FallbackAddressTag(17, hash)
    case Base58.Prefix.ScriptAddressTestnet \ hash => FallbackAddressTag(18, hash)
    case Base58.Prefix.PubkeyAddress \ hash => FallbackAddressTag(17, hash)
    case Base58.Prefix.ScriptAddress \ hash => FallbackAddressTag(18, hash)
  }

  def fromBech32Address(address: String): FallbackAddressTag = {
    val (_, version, hash) = Bech32 decodeWitnessAddressMainChain address
    FallbackAddressTag(version, hash)
  }
}

case class RoutingInfoTag(route: PaymentRoute) extends Tag {
  def toInt5s = encode(Bech32 eight2five route.flatMap(pack).toArray, 'r')
  def pack(hop: Hop) = aconcat(hop.nodeId.toBin.toArray, writeUInt64Array(hop.shortChannelId, BIG_ENDIAN),
    writeUInt32Array(hop.feeBaseMsat, BIG_ENDIAN), writeUInt32Array(hop.feeProportionalMillionths, BIG_ENDIAN),
    writeUInt16Array(hop.cltvExpiryDelta, BIG_ENDIAN))
}

object RoutingInfoTag {
  def parse(data: Bytes) = {
    val pubkey = ByteVector apply data.slice(0, 33)
    val shortChanId = uint64(data.slice(33, 33 + 8), BIG_ENDIAN)
    val feeBaseMsat = uint32(data.slice(33 + 8, 33 + 8 + 4), BIG_ENDIAN)
    val cltvExpiryDelta = uint16(data.slice(33 + 8 + 4 + 4, chunkLength), BIG_ENDIAN)
    val feeProportionalMillionths = uint32(data.slice(33 + 8 + 4, 33 + 8 + 4 + 4), BIG_ENDIAN)
    Hop(PublicKey(pubkey), shortChanId, cltvExpiryDelta, 0L, feeBaseMsat, feeProportionalMillionths)
  }

  type PaymentRoute = Vector[Hop]
  type PaymentRouteVec = Vector[PaymentRoute]

  val chunkLength = 33 + 8 + 4 + 4 + 2
  def parseAll(data: Bytes): PaymentRoute =
    data.grouped(chunkLength).map(parse).toVector
}

case class ExpiryTag(seconds: Long) extends Tag {
  def toInt5s = Bech32.map('x') +: (writeSize(ints.length) ++ ints)
  lazy val ints = writeUnsignedLong(seconds)
}

object FeaturesTag {
  def generate(features: Long*) = {
    val maskLong = features.foldLeft(0L) { case (current, feature) => (1L << feature) + current }
    FeaturesTag(bitMask = PaymentRequest long2Bits maskLong)
  }
}

case class FeaturesTag(bitMask: BitVector) extends Tag {
  def toInt5s = encode(Bech32 eight2five bitMask.dropRight(n = (8 - bitMask.size % 8) % 8).toByteArray, '9')
  val padBitMask = bitMask ++ BitVector.fill(n = (5 - bitMask.size % 5) % 5)(high = false)

  import com.lightning.walletapp.ln.Features._
  lazy val supported = areSupported(Set(PAYMENT_SECRET_MANDATORY, BASIC_MULTI_PART_PAYMENT_MANDATORY), padBitMask.reverse)
  lazy val allowMultiPart = hasFeature(padBitMask, BASIC_MULTI_PART_PAYMENT_MANDATORY) || hasFeature(padBitMask, BASIC_MULTI_PART_PAYMENT_OPTIONAL)
  lazy val allowPaymentSecret = hasFeature(padBitMask, PAYMENT_SECRET_MANDATORY) || hasFeature(padBitMask, PAYMENT_SECRET_OPTIONAL)
}

case class MinFinalCltvExpiryTag(expiryDelta: Long) extends Tag {
  def toInt5s = Bech32.map('c') +: (writeSize(ints.length) ++ ints)
  lazy val ints = writeUnsignedLong(expiryDelta)
}

case class UnknownTag(tag: Int5, int5s: Bytes) extends Tag {
  def toInt5s = tag +: (writeSize(int5s.length) ++ int5s)
}

case class PaymentRequest(prefix: String, amount: Option[MilliSatoshi], timestamp: Long, nodeId: PublicKey, tags: Vector[Tag], signature: ByteVector) {
  require(tags.collect { case _: DescriptionTag | _: DescriptionHashTag => true }.size == 1, "There can only be one description or description hash tag")
  for (MilliSatoshi(sum) <- amount) require(sum > 0L, "Amount is not valid")

  private val paymentHashes = tags.collect { case PaymentHashTag(hash) => hash }
  private val paymentSecrets = tags.collect { case PaymentSecretTag(secret) => secret }
  val features = tags.collectFirst { case fts: FeaturesTag => fts } getOrElse FeaturesTag(BitVector.empty)
  if (features.allowMultiPart) require(features.allowPaymentSecret, "There must be a payment secret when using multi-part payment request")
  if (features.allowPaymentSecret) require(paymentSecrets.size == 1, "There must be exactly one payment secret tag")
  require(paymentHashes.size == 1, "There must be exactly one payment hash tag")

  lazy val msatOrMin = amount getOrElse MilliSatoshi(LNParams.minPaymentMsat)
  lazy val routingInfo = tags.collect { case routingInfo: RoutingInfoTag => routingInfo }
  lazy val adjustedMinFinalCltvExpiry = tags.collectFirst { case MinFinalCltvExpiryTag(delta) => delta + leeway }.getOrElse(leeway)
  lazy val description = tags.collectFirst { case DescriptionHashTag(hash) => hash.toHex case DescriptionTag(info) => info } getOrElse new String

  lazy val fallbackAddress = tags.collectFirst {
    case FallbackAddressTag(17, hash) if prefix == "lnbc" => Base58Check.encode(Base58.Prefix.PubkeyAddress, hash)
    case FallbackAddressTag(18, hash) if prefix == "lnbc" => Base58Check.encode(Base58.Prefix.ScriptAddress, hash)
    case FallbackAddressTag(17, hash) if prefix == "lntb" || prefix == "lnbcrt" => Base58Check.encode(Base58.Prefix.PubkeyAddressTestnet, hash)
    case FallbackAddressTag(18, hash) if prefix == "lntb" || prefix == "lnbcrt" => Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, hash)
    case FallbackAddressTag(version, hash) if prefix == "lntb" || prefix == "lnbcrt" => Bech32.encodeWitnessAddress("tb", version, hash)
    case FallbackAddressTag(version, hash) if prefix == "lnbc" => Bech32.encodeWitnessAddress("bc", version, hash)
  }

  val paymentHash = paymentHashes.head
  val paymentSecretOpt = paymentSecrets.headOption

  def isFresh: Boolean = {
    val expiry = tags.collectFirst { case ex: ExpiryTag => ex.seconds }
    timestamp + expiry.getOrElse(3600L) > System.currentTimeMillis / 1000L
  }

  def stream: BitStream = {
    val int5s = Timestamp.encode(timestamp) ++ tags.flatMap(_.toInt5s)
    val stream1 = int5s.foldLeft(BitStream.empty)(PaymentRequest.write5)
    stream1
  }

  def hash: ByteVector = {
    val base = prefix + Amount.encode(amount)
    val ready = base.getBytes("UTF-8") ++ stream.bytes
    Crypto sha256 ByteVector.view(ready)
  }

  def sign(priv: PrivateKey) = {
    val (r, s) = Crypto.sign(hash, priv)
    val (pub1, _) = Crypto.recoverPublicKey(r -> s, hash)
    val recid = if (nodeId == pub1) 0.toByte else 1.toByte
    val signature1 = Signature.encode(r, s, recid)
    copy(signature = signature1)
  }
}

object PaymentRequest {
  type AmountOption = Option[MilliSatoshi]
  val expiryTag = ExpiryTag(seconds = 3600 * 24)
  val cltvExpiryTag = MinFinalCltvExpiryTag(LNParams.blocksPerDay * 2 - 3) // Minus 3 to account for trampoline senders
  val prefixes = Map(Block.RegtestGenesisBlock.hash -> "lnbcrt", Block.TestnetGenesisBlock.hash -> "lntb", Block.LivenetGenesisBlock.hash -> "lnbc")
  val leeway = 18L // When deciding whether to fulfill an incoming payment its CLTV expiry should have at least this much blocks left before chain height

  def apply(chain: ByteVector, amount: Option[MilliSatoshi], paymentHash: ByteVector, privKey: PrivateKey,
            description: String, fallbackAddress: Option[String], routes: PaymentRouteVec): PaymentRequest = {

    val timestampSecs = System.currentTimeMillis / 1000L
    val secret = ByteVector.view(Tools.random getBytes 32)
    val features = FeaturesTag.generate(Features.PAYMENT_SECRET_OPTIONAL, Features.BASIC_MULTI_PART_PAYMENT_OPTIONAL)
    val baseTags = Vector(DescriptionTag(description), cltvExpiryTag, PaymentHashTag(paymentHash), PaymentSecretTag(secret), expiryTag, features)
    val completeTags = routes.map(RoutingInfoTag.apply) ++ fallbackAddress.map(FallbackAddressTag.apply).toVector ++ baseTags
    PaymentRequest(prefixes(chain), amount, timestampSecs, privKey.publicKey, completeTags, ByteVector.empty) sign privKey
  }

  def long2Bits(value: Long) = {
    var highestPosition: Int = -1
    val bin = BitVector.fromLong(value)
    for (ord <- 0 until bin.size.toInt) if (bin(ord) && highestPosition == -1) highestPosition = ord
    val nonPadded: BitVector = if (highestPosition == -1) BitVector.empty else bin.drop(highestPosition)
    nonPadded.size % 5 match { case 0 => nonPadded case left => BitVector.fill(5 - left)(high = false) ++ nonPadded }
  }

  object Amount {
    // Shortest representation possible
    def unit(sum: MilliSatoshi): Char = sum match {
      case MilliSatoshi(pico) if pico * 10 % 1000 > 0 => 'p'
      case MilliSatoshi(pico) if pico * 10 % 1000000 > 0 => 'n'
      case MilliSatoshi(pico) if pico * 10 % 1000000000 > 0 => 'u'
      case _ => 'm'
    }

    def decode(input: String): AmountOption = input.lastOption match {
      case Some('p') => Some(MilliSatoshi apply input.dropRight(1).toLong / 10L)
      case Some('n') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100L)
      case Some('u') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100000L)
      case Some('m') => Some(MilliSatoshi apply input.dropRight(1).toLong * 100000000L)
      case _ if input.nonEmpty => Some(MilliSatoshi apply input.toLong * 100000000000L)
      case _ => None
    }

    def encode(amt: AmountOption): String = amt match {
      case Some(sum) if unit(sum) == 'p' => s"${sum.amount * 10}p"
      case Some(sum) if unit(sum) == 'n' => s"${sum.amount / 100}n"
      case Some(sum) if unit(sum) == 'u' => s"${sum.amount / 100000}u"
      case Some(sum) if unit(sum) == 'm' => s"${sum.amount / 100000000}m"
      case _ => ""
    }
  }

  object Timestamp {
    def decode(data: Bytes): Long = data.take(7).foldLeft(0L) { case (a, b) => a * 32 + b }
    def encode(timestamp: Long, acc: Bytes = Array.emptyByteArray): Bytes = if (acc.length == 7) acc
    else encode(timestamp / 32, (timestamp % 32).toByte +: acc)
  }

  object Signature {
    def decode(signature: ByteVector) = {
      require(signature.length == 65, "Invalid signature length")
      val s = new BigInteger(1, signature.slice(32, 64).toArray)
      val r = new BigInteger(1, signature.take(32).toArray)
      val recid = signature.last
      (r, s, recid)
    }

    def encode(r: BigInteger, s: BigInteger, recid: Byte) = {
      val rEncoded = Crypto fixSize ByteVector.view(r.toByteArray dropWhile 0.==)
      val sEncoded = Crypto fixSize ByteVector.view(s.toByteArray dropWhile 0.==)
      rEncoded ++ sEncoded :+ recid
    }
  }

  object Tag {
    def parse(input: Bytes): Tag = {
      val len = input(1) * 32 + input(2)

      input.head match {
        case tagP if tagP == 1 =>
          val hash = Bech32 five2eight input.slice(3, 52 + 3)
          PaymentHashTag(ByteVector view hash)

        case tagS if tagS == 16 =>
          val hash = Bech32 five2eight input.slice(3, 52 + 3)
          PaymentSecretTag(ByteVector view hash)

        case tagD if tagD == 13 =>
          val description = Bech32 five2eight input.slice(3, len + 3)
          DescriptionTag(Tools bin2readable description)

        case tagH if tagH == 23 =>
          val hash = Bech32 five2eight input.slice(3, len + 3)
          DescriptionHashTag(ByteVector view hash)

        case tagF if tagF == 9 =>
          val fallbackAddress = input.slice(4, len + 4 - 1)
          if (input(3) < 0 || input(3) > 18) UnknownTag(input.head, fallbackAddress) else {
            val fallbackAddressHash = ByteVector.view(Bech32 five2eight fallbackAddress)
            FallbackAddressTag(input(3), fallbackAddressHash)
          }

        case tagR if tagR == 3 =>
          val data = Bech32 five2eight input.slice(3, len + 3)
          val path = RoutingInfoTag parseAll data
          RoutingInfoTag(path)

        case tagX if tagX == 6 =>
          val ints: Bytes = input.slice(3, len + 3)
          val expiry = readUnsignedLong(len, ints)
          ExpiryTag(expiry)

        case tagC if tagC == 24 =>
          val ints: Bytes = input.slice(3, len + 3)
          val expiry = readUnsignedLong(len, ints)
          MinFinalCltvExpiryTag(expiry)

        case tag9 if tag9 == 5 =>
          val mask = Bech32 five2eight input.slice(3, len + 3)
          FeaturesTag(BitVector view mask)

        case _ =>
          val unknown = input.slice(3, len + 3)
          UnknownTag(input.head, unknown)
      }
    }
  }

  def toBits(value: Int5): Seq[Bit] =
    Seq(elems = (value & 16) != 0, (value & 8) != 0,
      (value & 4) != 0, (value & 2) != 0, (value & 1) != 0)

  def write5(stream: BitStream, value: Int5): BitStream =
    stream writeBits toBits(value)

  def read5(stream: BitStream) = {
    val (stream1, bits) = stream readBits 5

    val b0 = if (bits.head) 1 << 4 else 0
    val b1 = if (bits apply 1) 1 << 3 else 0
    val b2 = if (bits apply 2) 1 << 2 else 0
    val b3 = if (bits apply 3) 1 << 1 else 0
    val b4 = if (bits apply 4) 1 << 0 else 0
    val value = b0 + b1 + b2 + b3 + b4
    (stream1, (value & 0xff).toByte)
  }

  def toInt5s(stream: BitStream, acc: Bytes = Array.emptyByteArray): Bytes =
    if (stream.bitCount == 0) acc else {
      val stream1 \ value = read5(stream)
      toInt5s(stream1, acc :+ value)
    }

  def writeSize(size: Long): Bytes = {
    val outputData = writeUnsignedLong(size)
    require(outputData.length <= 2)

    outputData.length match {
      case 0 => Array(0.toByte, 0.toByte)
      case 1 => 0.toByte +: outputData
      case _ => outputData
    }
  }

  def writeUnsignedLong(value: Long, acc: Bytes = Array.emptyByteArray): Bytes =
    if (value == 0) acc else writeUnsignedLong(value / 32, (value % 32).toByte +: acc)

  def readUnsignedLong(length: Int, ints: Bytes): Long =
    ints.take(length).foldLeft(0L) { case acc \ i => acc * 32 + i }

  def read(input: String): PaymentRequest = {
    def loop(data: Bytes, tags: Seq[Bytes] = Nil): Seq[Bytes] =

      if (data.isEmpty) tags else {
        // 104 is the size of a signature
        val len = 1 + 2 + 32 * data(1) + data(2)
        val tags1 = tags :+ data.take(len)
        loop(data drop len, tags1)
      }

    val (hrp, data) = Bech32 decode input
    val stream = data.foldLeft(BitStream.empty)(write5)
    require(stream.bitCount >= 65 * 8, "Data is too short")

    val (stream1, sig) = stream.popBytes(65)
    val data0 = toInt5s(stream1)

    val rawtags = loop(data0 drop 7)
    val tags = rawtags map Tag.parse

    val signature = ByteVector(sig.reverse)
    val (r, s, recid) = Signature decode signature
    val messageHash = Crypto sha256 ByteVector.view(hrp.getBytes ++ stream1.bytes)
    val (pub1, pub2) = Crypto.recoverPublicKey(r -> s, messageHash)
    val pub = if (recid % 2 != 0) pub2 else pub1

    // Will throw on unknown prefix, this is fine
    val prefix = prefixes.values.find(hrp.startsWith).get
    val amountOpt = Amount.decode(hrp drop prefix.length)

    val pr = PaymentRequest(prefix, amountOpt, Timestamp decode data0, pub, tags.toVector, signature)
    require(Crypto.verifySignature(messageHash, r -> s, pub), "Invalid payment request signature")
    pr
  }

  def write(pr: PaymentRequest): String = {
    val hrp = pr.prefix + Amount.encode(pr.amount)
    val int5s = toInt5s(pr.stream writeBytes pr.signature.toSeq)
    Bech32.encode(hrp, int5s)
  }
}