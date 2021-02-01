package com.lightning.walletapp.test

import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import com.lightning.walletapp.ln.wire.{GenericTlv, Tlv, TlvStream}
import scodec.bits.ByteVector
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.UInt64
import fr.acinq.eclair.UInt64.Conversions._
import scodec.codecs._
import scodec.Codec


class TlvCodecsSpec {
  import TlvCodecsSpec._

  def allTests = {

    {
      println("encode/decode truncated uint16")

      val testCases = Seq(
        (ByteVector.fromValidHex("00"), 0),
        (ByteVector.fromValidHex("01 01"), 1),
        (ByteVector.fromValidHex("01 2a"), 42),
        (ByteVector.fromValidHex("01 ff"), 255),
        (ByteVector.fromValidHex("02 0100"), 256),
        (ByteVector.fromValidHex("02 0231"), 561),
        (ByteVector.fromValidHex("02 ffff"), 65535)
      )

      for ((bin, expected) <- testCases) {
        val decoded = tu16.decode(bin.bits).require.value
        assert(decoded == expected)

        val encoded = tu16.encode(expected).require.bytes
        assert(encoded == bin)
      }
    }

    {
      println("encode/decode truncated uint32")

      val testCases = Seq(
        (ByteVector.fromValidHex("00"), 0L),
        (ByteVector.fromValidHex("01 01"), 1L),
        (ByteVector.fromValidHex("01 2a"), 42L),
        (ByteVector.fromValidHex("01 ff"), 255L),
        (ByteVector.fromValidHex("02 0100"), 256L),
        (ByteVector.fromValidHex("02 0231"), 561L),
        (ByteVector.fromValidHex("02 ffff"), 65535L),
        (ByteVector.fromValidHex("03 010000"), 65536L),
        (ByteVector.fromValidHex("03 ffffff"), 16777215L),
        (ByteVector.fromValidHex("04 01000000"), 16777216L),
        (ByteVector.fromValidHex("04 01020304"), 16909060L),
        (ByteVector.fromValidHex("04 ffffffff"), 4294967295L)
      )

      for ((bin, expected) <- testCases) {
        val decoded = tu32.decode(bin.bits).require.value
        assert(decoded == expected)

        val encoded = tu32.encode(expected).require.bytes
        assert(encoded == bin)
      }
    }

    {
      println("encode/decode truncated uint64")

      val testCases = Seq(
        (ByteVector.fromValidHex("00"), UInt64(0)),
        (ByteVector.fromValidHex("01 01"), UInt64(1)),
        (ByteVector.fromValidHex("01 2a"), UInt64(42)),
        (ByteVector.fromValidHex("01 ff"), UInt64(255)),
        (ByteVector.fromValidHex("02 0100"), UInt64(256)),
        (ByteVector.fromValidHex("02 0231"), UInt64(561)),
        (ByteVector.fromValidHex("02 ffff"), UInt64(65535)),
        (ByteVector.fromValidHex("03 010000"), UInt64(65536)),
        (ByteVector.fromValidHex("03 ffffff"), UInt64(16777215)),
        (ByteVector.fromValidHex("04 01000000"), UInt64(16777216)),
        (ByteVector.fromValidHex("04 01020304"), UInt64(16909060)),
        (ByteVector.fromValidHex("04 ffffffff"), UInt64(4294967295L)),
        (ByteVector.fromValidHex("05 0100000000"), UInt64(4294967296L)),
        (ByteVector.fromValidHex("05 0102030405"), UInt64(4328719365L)),
        (ByteVector.fromValidHex("05 ffffffffff"), UInt64(1099511627775L)),
        (ByteVector.fromValidHex("06 010000000000"), UInt64(1099511627776L)),
        (ByteVector.fromValidHex("06 010203040506"), UInt64(1108152157446L)),
        (ByteVector.fromValidHex("06 ffffffffffff"), UInt64(281474976710655L)),
        (ByteVector.fromValidHex("07 01000000000000"), UInt64(281474976710656L)),
        (ByteVector.fromValidHex("07 01020304050607"), UInt64(283686952306183L)),
        (ByteVector.fromValidHex("07 ffffffffffffff"), UInt64(72057594037927935L)),
        (ByteVector.fromValidHex("08 0100000000000000"), UInt64(72057594037927936L)),
        (ByteVector.fromValidHex("08 0102030405060708"), UInt64(72623859790382856L)),
        (ByteVector.fromValidHex("08 ffffffffffffffff"), UInt64.MaxValue)
      )

      for ((bin, expected) <- testCases) {
        val decoded = tu64.decode(bin.bits).require.value
        assert(decoded == expected)

        val encoded = tu64.encode(expected).require.bytes
        assert(encoded == bin)
      }
    }

    {
      println("decode invalid truncated integers")

      val testCases = Seq(
        (tu16, ByteVector.fromValidHex("01 00")), // not minimal
        (tu16, ByteVector.fromValidHex("02 0001")), // not minimal
        (tu16, ByteVector.fromValidHex("03 ffffff")), // length too big
        (tu32, ByteVector.fromValidHex("01 00")), // not minimal
        (tu32, ByteVector.fromValidHex("02 0001")), // not minimal
        (tu32, ByteVector.fromValidHex("03 000100")), // not minimal
        (tu32, ByteVector.fromValidHex("04 00010000")), // not minimal
        (tu32, ByteVector.fromValidHex("05 ffffffffff")), // length too big
        (tu64, ByteVector.fromValidHex("01 00")), // not minimal
        (tu64, ByteVector.fromValidHex("02 0001")), // not minimal
        (tu64, ByteVector.fromValidHex("03 000100")), // not minimal
        (tu64, ByteVector.fromValidHex("04 00010000")), // not minimal
        (tu64, ByteVector.fromValidHex("05 0001000000")), // not minimal
        (tu64, ByteVector.fromValidHex("06 000100000000")), // not minimal
        (tu64, ByteVector.fromValidHex("07 00010000000000")), // not minimal
        (tu64, ByteVector.fromValidHex("08 0001000000000000")), // not minimal
        (tu64, ByteVector.fromValidHex("09 ffffffffffffffffff")) // length too big
      )

      for ((codec, bin) <- testCases) {
        assert(codec.decode(bin.bits).isFailure, bin)
      }
    }

    {
      println("encode/decode tlv stream")

      val testCases = Seq(
        (ByteVector.fromValidHex(""), TlvStream[Tlv](Nil, Nil)),
        (ByteVector.fromValidHex("21 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(33, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("fd0201 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(513, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("fd00fd 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(253, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("fd00ff 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(255, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("fe02000001 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(33554433, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("ff0200000000000001 00"), TlvStream[Tlv](Nil, Seq(GenericTlv(144115188075855873L, ByteVector.fromValidHex(""))))),
        (ByteVector.fromValidHex("01 00"), TlvStream[Tlv](TestType1(0) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 01 01"), TlvStream[Tlv](TestType1(1) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 01 2a"), TlvStream[Tlv](TestType1(42) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 02 0100"), TlvStream[Tlv](TestType1(256) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 03 010000"), TlvStream[Tlv](TestType1(65536) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 04 01000000"), TlvStream[Tlv](TestType1(16777216) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 05 0100000000"), TlvStream[Tlv](TestType1(4294967296L) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 06 010000000000"), TlvStream[Tlv](TestType1(1099511627776L) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 07 01000000000000"), TlvStream[Tlv](TestType1(281474976710656L) :: Nil, Nil)),
        (ByteVector.fromValidHex("01 08 0100000000000000"), TlvStream[Tlv](TestType1(72057594037927936L) :: Nil, Nil)),
        (ByteVector.fromValidHex("02 08 0000000000000226"), TlvStream[Tlv](TestType2(550L) :: Nil, Nil)),
        (ByteVector.fromValidHex("03 31 023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb 0000000000000231 0000000000000451"), TlvStream[Tlv](TestType3(PublicKey.fromValidHex("023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"), 561, 1105) :: Nil, Nil)),
        (ByteVector.fromValidHex("fd00fe 02 0226"), TlvStream[Tlv](TestType254(550) :: Nil, Nil)),
        (ByteVector.fromValidHex("01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"), TlvStream[Tlv](TestType1(561) :: TestType2(1105L) :: TestType3(PublicKey.fromValidHex("02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"), 561, 1105) :: Nil, Nil)),
        (ByteVector.fromValidHex("01020231 0b020451 fd00fe02002a"), TlvStream[Tlv](Seq(TestType1(561), TestType254(42)), Seq(GenericTlv(11, ByteVector.fromValidHex("0451")))))
      )

      for ((bin, expected) <- testCases) {
        val decoded = testTlvStreamCodec.decode(bin.bits).require.value
        assert(decoded == expected)
        val encoded = testTlvStreamCodec.encode(expected).require.bytes
        assert(encoded == bin)
      }
    }

    {
      println("decode invalid tlv stream")

      val testCases = Seq(
        // Type truncated.
        ByteVector.fromValidHex("fd"),
        ByteVector.fromValidHex("fd01"),
        // Not minimally encoded type.
        ByteVector.fromValidHex("fd0001 00"),
        // Missing length.
        ByteVector.fromValidHex("fd0101"),
        // Length truncated.
        ByteVector.fromValidHex("0f fd"),
        ByteVector.fromValidHex("0f fd02"),
        // Not minimally encoded length.
        ByteVector.fromValidHex("0f fd0001 00"),
        ByteVector.fromValidHex("0f fe00000001 00"),
        // Missing value.
        ByteVector.fromValidHex("0f fd2602"),
        // Value truncated.
        ByteVector.fromValidHex("0f fd0201 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        // Unknown even type.
        ByteVector.fromValidHex("12 00"),
        ByteVector.fromValidHex("0a 00"),
        ByteVector.fromValidHex("fd0102 00"),
        ByteVector.fromValidHex("fe01000002 00"),
        ByteVector.fromValidHex("01020101 0a0101"),
        ByteVector.fromValidHex("ff0100000000000002 00"),
        // Invalid TestTlv1.
        ByteVector.fromValidHex("01 01 00"), // not minimally-encoded
        ByteVector.fromValidHex("01 02 0001"), // not minimally-encoded
        ByteVector.fromValidHex("01 03 000100"), // not minimally-encoded
        ByteVector.fromValidHex("01 04 00010000"), // not minimally-encoded
        ByteVector.fromValidHex("01 05 0001000000"), // not minimally-encoded
        ByteVector.fromValidHex("01 06 000100000000"), // not minimally-encoded
        ByteVector.fromValidHex("01 07 00010000000000"), // not minimally-encoded
        ByteVector.fromValidHex("01 08 0001000000000000"), // not minimally-encoded
        // Invalid TestTlv2.
        ByteVector.fromValidHex("02 07 01010101010101"), // invalid length
        ByteVector.fromValidHex("02 09 010101010101010101"), // invalid length
        // Invalid TestTlv3.
        ByteVector.fromValidHex("03 21 023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"), // invalid length
        ByteVector.fromValidHex("03 29 023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb0000000000000001"), // invalid length
        ByteVector.fromValidHex("03 30 023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb000000000000000100000000000001"), // invalid length
        ByteVector.fromValidHex("03 32 023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb0000000000000001000000000000000001"), // invalid length
        // Invalid TestTlv254.
        ByteVector.fromValidHex("fd00fe 00"), // invalid length
        ByteVector.fromValidHex("fd00fe 01 01"), // invalid length
        ByteVector.fromValidHex("fd00fe 03 010101"), // invalid length
        // Invalid multi-record streams.
        ByteVector.fromValidHex("01012a 02"), // valid tlv record followed by invalid tlv record (length missing)
        ByteVector.fromValidHex("01012a 0208"), // valid tlv record followed by invalid tlv record (value missing)
        ByteVector.fromValidHex("01012a 020801010101"), // valid tlv record followed by invalid tlv record (value truncated)
        ByteVector.fromValidHex("02080000000000000226 01012a"), // valid tlv records but invalid ordering
        ByteVector.fromValidHex("1f00 0f012a"), // valid tlv records but invalid ordering
        ByteVector.fromValidHex("02080000000000000231 02080000000000000451"), // duplicate tlv type
        ByteVector.fromValidHex("01012a 0b020231 0b020451"), // duplicate tlv type
        ByteVector.fromValidHex("1f00 1f012a"), // duplicate tlv type
        ByteVector.fromValidHex("01012a 0a020231 0b020451") // valid tlv records but from different namespace
      )

      for (testCase <- testCases) {
        assert(testTlvStreamCodec.decode(testCase.bits).isFailure, testCase)
      }
    }

    {
      println("encode/decode length-prefixed tlv stream")

      val testCases = Seq(
        ByteVector.fromValidHex("41 01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        ByteVector.fromValidHex("fd014d 01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451 ff6543210987654321 fd0100 10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010010101010101")
      )

      for (testCase <- testCases) {
        assert(lengthPrefixedTestTlvStreamCodec.encode(lengthPrefixedTestTlvStreamCodec.decode(testCase.bits).require.value).require.bytes == testCase)
      }
    }

    {
      println("decode invalid length-prefixed tlv stream")

      val testCases = Seq(
        // Length too big.
        ByteVector.fromValidHex("42 01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        // Length too short.
        ByteVector.fromValidHex("40 01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        // Missing length.
        ByteVector.fromValidHex("01020231 02080000000000000451 033102eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f28368661900000000000002310000000000000451"),
        // Valid length but duplicate types.
        ByteVector.fromValidHex("14 02080000000000000231 02080000000000000451"),
        // Valid length but invalid ordering.
        ByteVector.fromValidHex("0e 02080000000000000451 01020231"),
        // Valid length but unknown even type.
        ByteVector.fromValidHex("02 0a 00")
      )

      for (testCase <- testCases) {
        assert(lengthPrefixedTestTlvStreamCodec.decode(testCase.bits).isFailure)
      }
    }

    {
      println("encode unordered tlv stream (codec should sort appropriately)")

      val stream = TlvStream[Tlv](Seq(TestType254(42), TestType1(42)), Seq(GenericTlv(13, ByteVector.fromValidHex("2a")), GenericTlv(11, ByteVector.fromValidHex("2b"))))
      assert(testTlvStreamCodec.encode(stream).require.toByteVector == ByteVector.fromValidHex("01012a 0b012b 0d012a fd00fe02002a"))
      assert(lengthPrefixedTestTlvStreamCodec.encode(stream).require.toByteVector == ByteVector.fromValidHex("0f 01012a 0b012b 0d012a fd00fe02002a"))
    }

    {
      println("encode invalid tlv stream")

      val testCases = Seq(
        // Unknown even type.
        TlvStream[Tlv](Nil, Seq(GenericTlv(42, ByteVector.fromValidHex("2a")))),
        TlvStream[Tlv](Seq(TestType1(561), TestType2(1105L)), Seq(GenericTlv(42, ByteVector.fromValidHex("2a")))),
        // Duplicate type.
        TlvStream[Tlv](TestType1(561) :: TestType1(1105) :: Nil, Nil),
        TlvStream[Tlv](Seq(TestType1(561)), Seq(GenericTlv(1, ByteVector.fromValidHex("0451"))))
      )

      for (stream <- testCases) {
        assert(testTlvStreamCodec.encode(stream).isFailure, stream)
        assert(lengthPrefixedTestTlvStreamCodec.encode(stream).isFailure, stream)
      }
    }
  }
}

object TlvCodecsSpec {
  case class TestType1(uintValue: UInt64) extends Tlv
  case class TestType2(shortChannelId: Long) extends Tlv
  case class TestType3(nodeId: PublicKey, value1: UInt64, value2: UInt64) extends Tlv
  case class TestType254(intValue: Int) extends Tlv

  private val testCodec1: Codec[TestType1] = ("value" | tu64).as[TestType1]
  val testCodec2: Codec[TestType2] = (("length" | constant(ByteVector.fromValidHex("08"))) :: ("short_channel_id" | int64)).as[TestType2]
  val testCodec3: Codec[TestType3] = (("length" | constant(ByteVector.fromValidHex("31"))) :: ("node_id" | publicKey) :: ("value_1" | uint64) :: ("value_2" | uint64)).as[TestType3]
  val testCodec254: Codec[TestType254] = (("length" | constant(ByteVector.fromValidHex("02"))) :: ("value" | uint16)).as[TestType254]
  private val testTlvCodec = discriminated[Tlv].by(varint).typecase(1, testCodec1).typecase(2, testCodec2).typecase(3, testCodec3).typecase(254, testCodec254)

  val testTlvStreamCodec = tlvStream(testTlvCodec)
  val lengthPrefixedTestTlvStreamCodec = variableSizeBytesLong(varintoverflow, tlvStream(testTlvCodec))

  case class OtherType1(uintValue: UInt64) extends Tlv
  case class OtherType2(smallValue: Long) extends Tlv

  val otherCodec1: Codec[OtherType1] = ("value" | tu64).as[OtherType1]
  val otherCodec2: Codec[OtherType2] = ("value" | tu32).as[OtherType2]
  val otherTlvStreamCodec = tlvStream(discriminated[Tlv].by(varint).typecase(10, otherCodec1).typecase(11, otherCodec2))
}
