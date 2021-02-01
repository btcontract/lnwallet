package com.lightning.walletapp.test

import java.net.{Inet4Address, Inet6Address, InetAddress}

import com.google.common.net.InetAddresses
import com.lightning.walletapp.ln.Announcements
import com.lightning.walletapp.ln.crypto.{Hmac256, Sphinx}
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import scodec.bits.{BitVector, ByteVector}
import fr.acinq.bitcoin.{Block, Crypto, Protocol}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, Scalar}
import fr.acinq.eclair.UInt64
import scala.util.Random


class WireSpec {
  def randomKey: PrivateKey = PrivateKey({
    val bin = Array.fill[Byte](32)(0)
    Random.nextBytes(bin)
    ByteVector.view(bin)
  }, compressed = true)

  def randomBytes(size: Int) = {
    val bin = new Array[Byte](size)
    Random.nextBytes(bin)
    ByteVector.view(bin)
  }

  def randomSignature: ByteVector = {
    val priv = randomBytes(32)
    val data = randomBytes(32)
    val (r, s) = Crypto.sign(data, PrivateKey(priv, compressed = true))
    Crypto.encodeSignature(r, s) :+ fr.acinq.bitcoin.SIGHASH_ALL.toByte
  }

  def allTests = {
    def bin(size: Int, fill: Byte) = ByteVector.view(Array.fill[Byte](size)(fill))

    def scalar(fill: Byte) = Scalar(bin(32, fill))

    def point(fill: Byte) = Scalar(bin(32, fill)).toPoint

    def publicKey(fill: Byte) = PrivateKey(bin(32, fill), compressed = true).publicKey

    {
      println("encode/decode all kind of IPv6 addresses with ipv6address codec")

      {
        // IPv4 mapped
        val bin = BitVector.fromValidHex("00000000000000000000ffffae8a0b08")
        val ipv6 = Inet6Address.getByAddress(null, bin.toByteArray, null)
        val bin2 = ipv6address.encode(ipv6).require
        assert(bin == bin2)
      }

      {
        // regular IPv6 address
        val ipv6 = InetAddresses.forString("1080:0:0:0:8:800:200C:417A").asInstanceOf[Inet6Address]
        val bin = ipv6address.encode(ipv6).require
        val ipv62 = ipv6address.decode(bin).require.value
        assert(ipv6 == ipv62)
      }
    }

    {
      println("encode/decode with rgb codec")

      val color = (47.toByte, 255.toByte, 142.toByte)
      val bin = rgb.encode(color).toOption.get
      assert(bin == BitVector.fromValidHex("2f ff 8e"))
      val color2 = rgb.decode(bin).toOption.get.value
      assert(color == color2)
    }

    {
      println("encode/decode all kind of IPv6 addresses with ipv6address codec")

      {
        // IPv4 mapped
        val bin = BitVector.fromValidHex("00000000000000000000ffffae8a0b08")
        val ipv6 = Inet6Address.getByAddress(null, bin.toByteArray, null)
        val bin2 = ipv6address.encode(ipv6).require
        assert(bin == bin2)
      }

      {
        // regular IPv6 address
        val ipv6 = InetAddresses.forString("1080:0:0:0:8:800:200C:417A").asInstanceOf[Inet6Address]
        val bin = ipv6address.encode(ipv6).require
        val ipv62 = ipv6address.decode(bin).require.value
        assert(ipv6 == ipv62)
      }
    }

    {
      println("encode/decode with nodeaddress codec")

      {
        val ipv4addr = InetAddress.getByAddress(Array[Byte](192.toByte, 168.toByte, 1.toByte, 42.toByte)).asInstanceOf[Inet4Address]
        val nodeaddr = IPv4(ipv4addr, 4231)
        val bin = nodeaddress.encode(nodeaddr).require
        assert(bin == BitVector.fromValidHex("01 C0 A8 01 2A 10 87"))
        val nodeaddr2 = nodeaddress.decode(bin).require.value
        assert(nodeaddr == nodeaddr2)
      }
      {
        val ipv6addr = InetAddress.getByAddress(ByteVector.fromValidHex("2001 0db8 0000 85a3 0000 0000 ac1f 8001").toArray).asInstanceOf[Inet6Address]
        val nodeaddr = IPv6(ipv6addr, 4231)
        val bin = nodeaddress.encode(nodeaddr).require
        assert(bin == BitVector.fromValidHex("02 2001 0db8 0000 85a3 0000 0000 ac1f 8001 1087"))
        val nodeaddr2 = nodeaddress.decode(bin).require.value
        assert(nodeaddr == nodeaddr2)
      }
    }

    {
      println("encode/decode with signature codec")

      val sig = randomSignature
      val wire = LightningMessageCodecs.signature.encode(sig).toOption.get
      val sig1 = LightningMessageCodecs.signature.decode(wire).toOption.get.value
      assert(sig1 == sig)
    }

    {
      println("encode/decode with scalar codec")

      val value = Scalar(randomBytes(32))
      val wire = LightningMessageCodecs.scalar.encode(value).toOption.get
      assert(wire.length == 256)
      val value1 = LightningMessageCodecs.scalar.decode(wire).toOption.get.value
      assert(value1 == value)
    }

    {
      println("encode/decode with point codec")

      val value = Scalar(randomBytes(32)).toPoint
      val wire = LightningMessageCodecs.point.encode(value).toOption.get
      assert(wire.length == 33 * 8)
      val value1 = LightningMessageCodecs.point.decode(wire).toOption.get.value
      assert(value1 == value)
    }

    {
      println("encode/decode with public key codec")

      val value = PrivateKey(randomBytes(32), compressed = true).publicKey
      val wire = LightningMessageCodecs.publicKey.encode(value).toOption.get
      assert(wire.length == 33 * 8)
      val value1 = LightningMessageCodecs.publicKey.decode(wire).toOption.get.value
      assert(value1 == value)
    }

    {
      println("encode/decode with zeropaddedstring codec")

      val c = zeropaddedstring

      {
        val alias = "IRATEMONK"
        val bin = c.encode(alias).toOption.get
        assert(bin == BitVector(alias.getBytes("UTF-8") ++ Array.fill[Byte](32 - alias.length)(0)))
        val alias2 = c.decode(bin).toOption.get.value
        assert(alias == alias2)
      }

      {
        val alias = "this-alias-is-exactly-32-B-long."
        val bin = c.encode(alias).toOption.get
        assert(bin == BitVector(alias.getBytes("UTF-8") ++ Array.fill[Byte](32 - alias.length)(0)))
        val alias2 = c.decode(bin).toOption.get.value
        assert(alias == alias2)
      }

      {
        val alias = "this-alias-is-far-too-long-because-we-are-limited-to-32-bytes"
        assert(c.encode(alias).isFailure)
      }
    }

    {
      println("encode/decode with uint64 codec")

      val expected = Map(
        UInt64(0) -> ByteVector.fromValidHex("0000000000000000"),
        UInt64(42) -> ByteVector.fromValidHex("000000000000002a"),
        UInt64(6211610197754262546L) -> ByteVector.fromValidHex("5634129078563412"),
        UInt64(ByteVector.fromValidHex("ffffffffffffffff")) -> ByteVector.fromValidHex("ffffffffffffffff")
      ).mapValues(_.toBitVector)

      for ((uint, ref) <- expected) {
        val encoded = uint64.encode(uint).require
        assert(ref == encoded)
        val decoded = uint64.decode(encoded).require.value
        assert(uint == decoded)
      }
    }

    {
      println("encode/decode with varint codec")
      val expected = Map(
        UInt64(0L) -> ByteVector.fromValidHex("00"),
        UInt64(42L) -> ByteVector.fromValidHex("2a"),
        UInt64(253L) -> ByteVector.fromValidHex("fd 00 fd"),
        UInt64(254L) -> ByteVector.fromValidHex("fd 00 fe"),
        UInt64(255L) -> ByteVector.fromValidHex("fd 00 ff"),
        UInt64(550L) -> ByteVector.fromValidHex("fd 02 26"),
        UInt64(998000L) -> ByteVector.fromValidHex("fe 00 0f 3a 70"),
        UInt64(1311768467284833366L) -> ByteVector.fromValidHex("ff 12 34 56 78 90 12 34 56"),
        UInt64.MaxValue -> ByteVector.fromValidHex("ff ff ff ff ff ff ff ff ff")
      ).mapValues(_.toBitVector)

      for ((uint, ref) <- expected) {
        val encoded = varint.encode(uint).require
        assert(ref == encoded, ref)
        val decoded = varint.decode(encoded).require.value
        assert(uint == decoded, uint)
      }
    }

    {
      println("decode invalid varint")
      val testCases = Seq(
        ByteVector.fromValidHex("fd"), // truncated
        ByteVector.fromValidHex("fe 01"), // truncated
        ByteVector.fromValidHex("fe"), // truncated
        ByteVector.fromValidHex("fe 12 34"), // truncated
        ByteVector.fromValidHex("ff"), // truncated
        ByteVector.fromValidHex("ff 12 34 56 78"), // truncated
        ByteVector.fromValidHex("fd 00 00"), // not minimally-encoded
        ByteVector.fromValidHex("fd 00 fc"), // not minimally-encoded
        ByteVector.fromValidHex("fe 00 00 00 00"), // not minimally-encoded
        ByteVector.fromValidHex("fe 00 00 ff ff"), // not minimally-encoded
        ByteVector.fromValidHex("ff 00 00 00 00 00 00 00 00"), // not minimally-encoded
        ByteVector.fromValidHex("ff 00 00 00 00 01 ff ff ff"), // not minimally-encoded
        ByteVector.fromValidHex("ff 00 00 00 00 ff ff ff ff") // not minimally-encoded
      ).map(_.toBitVector)

      for (testCase <- testCases) {
        assert(varint.decode(testCase).isFailure, testCase.toByteVector)
      }
    }

    {
      println("encode/decode with varintoverflow codec")
      val expected = Map(
        0L -> ByteVector.fromValidHex("00"),
        42L -> ByteVector.fromValidHex("2a"),
        253L -> ByteVector.fromValidHex("fd 00 fd"),
        254L -> ByteVector.fromValidHex("fd 00 fe"),
        255L -> ByteVector.fromValidHex("fd 00 ff"),
        550L -> ByteVector.fromValidHex("fd 02 26"),
        998000L -> ByteVector.fromValidHex("fe 00 0f 3a 70"),
        1311768467284833366L -> ByteVector.fromValidHex("ff 12 34 56 78 90 12 34 56"),
        Long.MaxValue -> ByteVector.fromValidHex("ff 7f ff ff ff ff ff ff ff")
      ).mapValues(_.toBitVector)

      for ((long, ref) <- expected) {
        val encoded = varintoverflow.encode(long).require
        assert(ref == encoded, ref)
        val decoded = varintoverflow.decode(encoded).require.value
        assert(long == decoded, long)
      }
    }

    {
      println("decode invalid varintoverflow")
      val testCases = Seq(
        ByteVector.fromValidHex("ff 80 00 00 00 00 00 00 00"),
        ByteVector.fromValidHex("ff ff ff ff ff ff ff ff ff")
      ).map(_.toBitVector)

      for (testCase <- testCases) {
        assert(varintoverflow.decode(testCase).isFailure, testCase.toByteVector)
      }
    }

    {
      println("encode/decode UInt64")
      val refs = Seq(
        UInt64(ByteVector.fromValidHex("ffffffffffffffff")),
        UInt64(ByteVector.fromValidHex("fffffffffffffffe")),
        UInt64(ByteVector.fromValidHex("efffffffffffffff")),
        UInt64(ByteVector.fromValidHex("effffffffffffffe"))
      )
      assert(refs.forall(value => uint64.decode(uint64.encode(value).require).require.value == value))
    }

    {
      println("encode/decode with prependmac codec")
      val mac = Hmac256(Protocol.Zeroes)
      val testCases = Seq(
        (uint64, UInt64(561), ByteVector.fromValidHex("d5b500b8843e19a34d8ab54740db76a7ea597e4ff2ada3827420f87c7e60b7c6 0000000000000231")),
        (varint, UInt64(65535), ByteVector.fromValidHex("71e17e5b97deb6916f7ad97a53650769d4e4f0b1e580ff35ca332200d61e765c fdffff"))
      )

      for ((codec, expected, bin) <- testCases) {
        val macCodec = prependmac(codec, mac)
        val decoded = macCodec.decode(bin.toBitVector).require.value
        assert(decoded == expected)

        val encoded = macCodec.encode(expected).require.toByteVector
        assert(encoded == bin)
      }
    }

    {
      println("encode/decode all channel messages")
      val open = OpenChannel(randomBytes(32), randomBytes(32), 3, 4, 5, UInt64(6), 7, 8, 9, 10, 11, publicKey(1), point(2), point(3), point(4), point(5), point(6), ChannelFlags(0.toByte))
      val accept = AcceptChannel(randomBytes(32), 3, UInt64(4), 5, 6, 7, 8, 9, publicKey(1), point(2), point(3), point(4), point(5), point(6))
      val funding_created = FundingCreated(randomBytes(32), bin(32, 0), 3, randomSignature)
      val funding_signed = FundingSigned(randomBytes(32), randomSignature)
      val funding_locked = FundingLocked(randomBytes(32), point(2))
      val update_fee = UpdateFee(randomBytes(32), 2)
      val shutdown = Shutdown(randomBytes(32), bin(47, 0))
      val closing_signed = ClosingSigned(randomBytes(32), 2, randomSignature)
      val update_add_htlc = UpdateAddHtlc(randomBytes(32), 2, 3, bin(32, 0), 4)
      val update_fulfill_htlc = UpdateFulfillHtlc(randomBytes(32), 2, bin(32, 0))
      val update_fail_htlc = UpdateFailHtlc(randomBytes(32), 2, bin(154, 0))
      val update_fail_malformed_htlc = UpdateFailMalformedHtlc(randomBytes(32), 2, randomBytes(32), 1111)
      val commit_sig = CommitSig(randomBytes(32), randomSignature, randomSignature :: randomSignature :: randomSignature :: Nil)
      val revoke_and_ack = RevokeAndAck(randomBytes(32), scalar(0), point(1))
      val channel_announcement = ChannelAnnouncement(randomSignature, randomSignature, randomSignature, randomSignature, bin(7, 9), Block.RegtestGenesisBlock.hash, 1, randomKey.publicKey, randomKey.publicKey, randomKey.publicKey, randomKey.publicKey)
      val node_announcement = NodeAnnouncement(randomSignature, bin(1, 2), 1, randomKey.publicKey, (100.toByte, 200.toByte, 300.toByte), "node-alias", IPv4(InetAddress.getByAddress(Array[Byte](192.toByte, 168.toByte, 1.toByte, 42.toByte)).asInstanceOf[Inet4Address], 42000) :: Nil)
      val channel_update = ChannelUpdate(randomSignature, Block.RegtestGenesisBlock.hash, 1, 2, 42, 0, 3, 4, 5, 6, None)
      val announcement_signatures = AnnouncementSignatures(randomBytes(32), 42, randomSignature, randomSignature)
      val ping = Ping(100, ByteVector.fromValidHex("01" * 10))
      val pong = Pong(ByteVector.fromValidHex("01" * 10))
      val channel_reestablish = ChannelReestablish(randomBytes(32), 242842L, 42L, None, None)

      val invoke_hosted_channel = InvokeHostedChannel(randomBytes(32), bin(47, 0), bin(112, 0))
      val init_hosted_channel = InitHostedChannel(UInt64(6), 10, 20, 500000000L, 5000, 1000000, 1000000, ByteVector.empty)
      val state_override = StateOverride(50000L, 500000, 70000, 700000, randomSignature)

      val state_update = StateUpdate(50000L, 10, 20, randomSignature, isTerminal = false)
      val lcss1 = LastCrossSignedState(bin(47, 0), init_hosted_channel, 10000, 10000, 20000, 10, 20, List(update_add_htlc, update_add_htlc, update_add_htlc), List(update_add_htlc, update_add_htlc, update_add_htlc), randomSignature, randomSignature)
      val lcss2 = LastCrossSignedState(bin(47, 0), init_hosted_channel, 10000, 10000, 20000, 10, 20, Nil, List(update_add_htlc, update_add_htlc), randomSignature, randomSignature)
      val lcss3 = LastCrossSignedState(bin(47, 0), init_hosted_channel, 10000, 10000, 20000, 10, 20, List(update_add_htlc, update_add_htlc), Nil, randomSignature, randomSignature)
      val lcss4 = LastCrossSignedState(bin(47, 0), init_hosted_channel, 10000, 10000, 20000, 10, 20, Nil, Nil, randomSignature, randomSignature)

      val msgs: List[LightningMessage] =
        open :: accept :: funding_created :: funding_signed :: funding_locked :: update_fee :: shutdown :: closing_signed ::
          update_add_htlc :: update_fulfill_htlc :: update_fail_htlc :: update_fail_malformed_htlc :: commit_sig :: revoke_and_ack ::
          channel_announcement :: node_announcement :: channel_update :: announcement_signatures :: ping :: pong :: channel_reestablish ::
          invoke_hosted_channel :: init_hosted_channel :: state_override :: state_update :: lcss1 :: lcss2 :: lcss3 :: lcss4 :: Nil

      msgs foreach { msg =>
        val encoded = lightningMessageCodec.encode(msg).require
        val decoded = lightningMessageCodec.decode(encoded).require
        assert(msg == decoded.value)
      }
    }

    {
      println("decode channel_update with htlc_maximum_msat")
      val bin = ByteVector.fromValidHex("010258fff7d0e987e2cdd560e3bb5a046b4efe7b26c969c2f51da1dceec7bcb8ae1b634790503d5290c1a6c51d681cf8f4211d27ed33a257dcc1102862571bf1792306226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f0005a100000200005bc75919010100060000000000000001000000010000000a000000003a699d00")
      val update = LightningMessageCodecs.lightningMessageCodec.decode(BitVector(bin.toArray)).require.value.asInstanceOf[ChannelUpdate]
      assert(update == ChannelUpdate(ByteVector.fromValidHex("3044022058fff7d0e987e2cdd560e3bb5a046b4efe7b26c969c2f51da1dceec7bcb8ae1b0220634790503d5290c1a6c51d681cf8f4211d27ed33a257dcc1102862571bf1792301"), ByteVector.fromValidHex("06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f"), 0x5a10000020000L, 1539791129, 1, 1, 6, 1, 1, 10, Some(980000000L)))
      val nodeId = PublicKey(ByteVector.fromValidHex("03370c9bac836e557eb4f017fe8f9cc047f44db39c1c4e410ff0f7be142b817ae4"))
      assert(Announcements.checkSig(update, nodeId))
      val bin2 = ByteVector.view(LightningMessageCodecs.lightningMessageCodec.encode(update).require.toByteArray)
      assert(bin == bin2)
    }
  }
}
