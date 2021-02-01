package com.lightning.walletapp.test

import com.lightning.walletapp.ln.Tools
import com.lightning.walletapp.ln.crypto.Hmac256
import com.lightning.walletapp.ln.wire._
import fr.acinq.bitcoin.{Block, Protocol}
import fr.acinq.eclair.UInt64
import scodec.bits.{BitVector, ByteVector}

import scala.util.Random


class FailureMessageLightningMessageCodecsSpec {

  val channelUpdate = ChannelUpdate(
    signature = ByteVector.fromValidHex("3045022100c451cd65c88f55b1767941a247e849e12f5f4d4a93a07316659e22f5267d2088022009042a595c6bc8942cd9d729317b82b306edc259fb6b3a3cecb3dd1bd446e90601"),
    chainHash = Block.RegtestGenesisBlock.hash,
    shortChannelId = 12345L,
    timestamp = 1234567L,
    cltvExpiryDelta = 100,
    messageFlags = 0,
    channelFlags = 1,
    htlcMinimumMsat = 1000,
    feeBaseMsat = 12,
    feeProportionalMillionths = 76,
    htlcMaximumMsat = None)

  def randomBytes(size: Int) = {
    val bin = new Array[Byte](size)
    Random.nextBytes(bin)
    ByteVector.view(bin)
  }

  def allTests = {
    val msgs = Seq[(Boolean, Boolean, FailureMessage)](
      (false, true, InvalidRealm),
      (true, false, TemporaryNodeFailure),
      (true, true, PermanentNodeFailure),
      (true, true, RequiredNodeFeatureMissing),
      (false, true, InvalidOnionVersion(randomBytes(32))),
      (false, true, InvalidOnionHmac(randomBytes(32))),
      (false, true, InvalidOnionKey(randomBytes(32))),
      (false, true, InvalidOnionPayload(UInt64(0), 0)),
      (false, false, TemporaryChannelFailure(channelUpdate)),
      (false, true, PermanentChannelFailure),
      (false, true, RequiredChannelFeatureMissing),
      (false, true, UnknownNextPeer),
      (false, false, AmountBelowMinimum(123456, channelUpdate)),
      (false, false, FeeInsufficient(546463, channelUpdate)),
      (false, false, IncorrectCltvExpiry(1211, channelUpdate)),
      (false, false, ExpiryTooSoon(channelUpdate)),
      (false, true, IncorrectOrUnknownPaymentDetails(123456, 1105)),
      (false, false, FinalIncorrectCltvExpiry(1234)),
      (false, false, ChannelDisabled(0, 1, channelUpdate)),
      (false, false, ExpiryTooFar)
    )

    for ((node, perm, msg) <- msgs) {
      val encoded = FailureMessageCodecs.failureMessageCodec.encode(msg).require
      val decoded = FailureMessageCodecs.failureMessageCodec.decode(encoded).require.value
      assert(msg == decoded)
      assert(decoded.isInstanceOf[Node] == node)
      assert(decoded.isInstanceOf[Perm] == perm)
    }

    println("support encoding of channel_update with/without type in failure messages")
    val tmp_channel_failure_notype = ByteVector.fromValidHex("10070080cc3e80149073ed487c76e48e9622bf980f78267b8a34a3f61921f2d8fce6063b08e74f34a073a13f2097337e4915bb4c001f3b5c4d81e9524ed575e1f45782196fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000000008260500041300005b91b52f0003000e00000000000003e80000000100000001")
    val tmp_channel_failure_withtype = ByteVector.fromValidHex("100700820102cc3e80149073ed487c76e48e9622bf980f78267b8a34a3f61921f2d8fce6063b08e74f34a073a13f2097337e4915bb4c001f3b5c4d81e9524ed575e1f45782196fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000000008260500041300005b91b52f0003000e00000000000003e80000000100000001")
    val ref = TemporaryChannelFailure(ChannelUpdate(ByteVector.fromValidHex("3045022100cc3e80149073ed487c76e48e9622bf980f78267b8a34a3f61921f2d8fce6063b022008e74f34a073a13f2097337e4915bb4c001f3b5c4d81e9524ed575e1f457821901"), Block.LivenetGenesisBlock.hash, 0x826050004130000L, 1536275759, 0, 3, 14, 1000, 1, 1, None))
    val u = FailureMessageCodecs.failureMessageCodec.decode(tmp_channel_failure_notype.toBitVector).require.value
    assert(u == ref)
    val bin = ByteVector.view(FailureMessageCodecs.failureMessageCodec.encode(u).require.toByteArray)
    assert(bin == tmp_channel_failure_withtype)
    val u2 = FailureMessageCodecs.failureMessageCodec.decode(BitVector(bin)).require.value
    assert(u2 == ref)

    println("decode unknown failure messages")
    val testCases = Seq(
      // Deprecated incorrect_payment_amount.
      (false, true, ByteVector.fromValidHex("4010")),
      // Deprecated final_expiry_too_soon.
      (false, true, ByteVector.fromValidHex("4011")),
      // Unknown failure messages.
      (false, false, ByteVector.fromValidHex("00ff 42")),
      (true, false, ByteVector.fromValidHex("20ff 42")),
      (true, true, ByteVector.fromValidHex("60ff 42"))
    )

    for ((node, perm, bin) <- testCases) {
      val decoded = FailureMessageCodecs.failureMessageCodec.decode(bin.bits).require.value
      assert(decoded.isInstanceOf[UnknownFailureMessage])
      assert(decoded.isInstanceOf[Node] == node)
      assert(decoded.isInstanceOf[Perm] == perm)
    }

    {
      println("bad onion failure code")
      val msgs = Map(
        (FailureMessageCodecs.BADONION | FailureMessageCodecs.PERM | 4) -> InvalidOnionVersion(randomBytes(32)),
        (FailureMessageCodecs.BADONION | FailureMessageCodecs.PERM | 5) -> InvalidOnionHmac(randomBytes(32)),
        (FailureMessageCodecs.BADONION | FailureMessageCodecs.PERM | 6) -> InvalidOnionKey(randomBytes(32)),
        (FailureMessageCodecs.PERM | 22) -> InvalidOnionPayload(UInt64(0), 0)
      )

      for ((code, message) <- msgs) {
        assert(message.code == code)
      }
    }

    {
      println("encode/decode failure onion")
      val codec = FailureMessageCodecs.failureOnionCodec(Hmac256(Protocol.Zeroes))
      val testCases = Map(
        InvalidOnionKey(ByteVector.fromValidHex("2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a")) -> ByteVector.fromValidHex("41a824e2d630111669fa3e52b600a518f369691909b4e89205dc624ee17ed2c1 0022 c006 2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a 00de 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        IncorrectOrUnknownPaymentDetails(42, 1105) -> ByteVector.fromValidHex("5eb766da1b2f45b4182e064dacd8da9eca2c9a33f0dce363ff308e9bdb3ee4e3 000e 400f 000000000000002a 00000451 00f2 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
      )

      for ((expected, bin) <- testCases) {
        val decoded = codec.decode(bin.toBitVector).require.value
        assert(decoded == expected)

        val encoded = codec.encode(expected).require.toByteVector
        assert(encoded == bin)
      }
    }

    {
      println("decode invalid failure onion packet")
      val codec = FailureMessageCodecs.failureOnionCodec(Hmac256(Protocol.Zeroes))
      val testCases = Seq(
        // Invalid failure message.
        ByteVector.fromValidHex("fd2f3eb163dacfa7fe2ec1a7dc73c33438e7ca97c561475cf0dc96dc15a75039 0020 c005 2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a 00e0 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        // Invalid mac.
        ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000000 0022 c006 2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a 00de 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        // Padding too small.
        ByteVector.fromValidHex("7bfb2aa46218240684f623322ae48af431d06986c82e210bb0cee83c7ddb2ba8 0002 4001 0002 0000"),
        // Padding length doesn't match actual padding.
        ByteVector.fromValidHex("8c92256e45bbe765130d952e6c043cf594ab25224701f5477fce0e50ee88fa21 0002 4001 0002 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        // Padding too big.
        ByteVector.fromValidHex("6f9e2c0e44b3692dac37523c6ff054cc9b26ecab1a78ed6906a46848bffc2bd5 0002 4001 00ff 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        // Padding length doesn't match actual padding.
        ByteVector.fromValidHex("3898307b7c01781628ff6f854a4a78524541e4afde9b44046bdb84093f082d9d 0002 4001 00ff 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
      )

      for (testCase <- testCases) {
        assert(codec.decode(testCase.toBitVector).isFailure)
      }
    }

    {
      println("encode/decode all failure messages")
      import com.lightning.walletapp.ln.wire.FailureMessageCodecs._
      val msgs: List[FailureMessage] =
        InvalidRealm :: TemporaryNodeFailure :: PermanentNodeFailure :: RequiredNodeFeatureMissing ::
          InvalidOnionVersion(ByteVector(Tools.random.getBytes(32))) :: InvalidOnionHmac(ByteVector(Tools.random.getBytes(32))) :: InvalidOnionKey(ByteVector(Tools.random.getBytes(32))) ::
          TemporaryChannelFailure(channelUpdate) :: PermanentChannelFailure :: RequiredChannelFeatureMissing :: UnknownNextPeer ::
          AmountBelowMinimum(123456L, channelUpdate) :: FeeInsufficient(546463L, channelUpdate) :: IncorrectCltvExpiry(1211, channelUpdate) :: ExpiryTooSoon(channelUpdate) ::
          IncorrectOrUnknownPaymentDetails(123456L, 1105) :: FinalIncorrectCltvExpiry(1234) :: ChannelDisabled(0, 1, channelUpdate) :: ExpiryTooFar :: InvalidOnionPayload(UInt64(561), 1105) :: PaymentTimeout :: Nil

      msgs.foreach {
        msg => {
          val encoded = failureMessageCodec.encode(msg).require
          val decoded = failureMessageCodec.decode(encoded).require
          assert(msg == decoded.value)
        }
      }
    }
  }
}