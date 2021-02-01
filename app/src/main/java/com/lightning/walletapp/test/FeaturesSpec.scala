package com.lightning.walletapp.test

import java.nio.ByteOrder
import fr.acinq.bitcoin.Protocol
import scodec.bits.ByteVector


class FeaturesSpec {
  import com.lightning.walletapp.ln.Features._

  def allTests = {
    {
      println("features compatibility")
      assert(isNodeSupported(Protocol.writeUInt64(1L << OPTION_DATA_LOSS_PROTECT_OPTIONAL, ByteOrder.BIG_ENDIAN)))
      assert(!isNodeSupported(ByteVector.fromValidHex("14")))
      assert(!isNodeSupported(ByteVector.fromValidHex("0141")))
      assert(isNodeSupported(Protocol.writeUInt64(1l << VARIABLE_LENGTH_ONION_OPTIONAL, ByteOrder.BIG_ENDIAN)))
      assert(!isNodeSupported(ByteVector.fromValidHex("14")))
      assert(!isNodeSupported(ByteVector.fromValidHex("0141")))
    }

    {
      assert(isNodeSupported(ByteVector.fromLong(1L << OPTION_DATA_LOSS_PROTECT_MANDATORY)))
      assert(isNodeSupported(ByteVector.fromLong(1L << OPTION_DATA_LOSS_PROTECT_OPTIONAL)))
      assert(isNodeSupported(ByteVector.fromLong(1L << VARIABLE_LENGTH_ONION_OPTIONAL)))
      assert(isNodeSupported(ByteVector.fromLong(1L << VARIABLE_LENGTH_ONION_MANDATORY)))
      assert(isNodeSupported(ByteVector.fromLong(1L << PAYMENT_SECRET_MANDATORY)))
      assert(isNodeSupported(ByteVector.fromLong(1L << PAYMENT_SECRET_OPTIONAL)))
      assert(isNodeSupported(ByteVector.fromLong(1L << BASIC_MULTI_PART_PAYMENT_MANDATORY)))
      assert(isNodeSupported(ByteVector.fromLong(1L << BASIC_MULTI_PART_PAYMENT_OPTIONAL)))
    }
  }
}
