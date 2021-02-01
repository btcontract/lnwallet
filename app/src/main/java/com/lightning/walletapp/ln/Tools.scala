package com.lightning.walletapp.ln

import scodec.bits.{BitVector, ByteVector}
import fr.acinq.bitcoin.Protocol.{One, Zeroes}
import fr.acinq.bitcoin.{Crypto, LexicographicalOrdering}
import com.lightning.walletapp.ln.Tools.runAnd
import com.lightning.walletapp.ln.wire.UpdateAddHtlc
import fr.acinq.bitcoin.Crypto.PrivateKey

import language.implicitConversions
import crypto.RandomGenerator

import scala.util.Try


object \ {
  // Matching Tuple2 via arrows with much less noise
  def unapply[A, B](t2: (A, B) /* Got a tuple */) = Some(t2)
}

object Tools {
  type Bytes = Array[Byte]
  val random = new RandomGenerator
  val nextDummyHtlc = UpdateAddHtlc(Zeroes, id = -1, LNParams.minCapacityMsat, One, expiry = 144 * 3)
  def randomPrivKey = PrivateKey(ByteVector.view(random getBytes 32), compressed = true)
  def log(consoleMessage: String): Unit = android.util.Log.d("LN", consoleMessage)
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def bin2readable(bin: Bytes) = new String(bin, "UTF-8")
  def none: PartialFunction[Any, Unit] = { case _ => }
  def runAnd[T](result: T)(action: Any): T = result

  def toDefMap[T, K, V](source: Seq[T], keyFun: T => K, valFun: T => V, default: V): Map[K, V] = {
    val sequenceOfTuples = for (mapElement <- source) yield keyFun(mapElement) -> valFun(mapElement)
    sequenceOfTuples.toMap withDefaultValue default
  }

  def sign(data: ByteVector, pk: PrivateKey) = Try {
    Crypto encodeSignature Crypto.sign(data, pk)
  } getOrElse ByteVector.empty

  def fromShortId(id: Long) = {
    val blockHeight = id.>>(40).&(0xFFFFFF).toInt
    val txIndex = id.>>(16).&(0xFFFFFF).toInt
    val outputIndex = id.&(0xFFFF).toInt
    (blockHeight, txIndex, outputIndex)
  }

  def toShortIdOpt(blockHeight: Long, txIndex: Long, outputIndex: Long): Option[Long] = {
    val result = blockHeight.&(0xFFFFFFL).<<(40) | txIndex.&(0xFFFFFFL).<<(16) | outputIndex.&(0xFFFFL)
    if (txIndex < 0) None else Some(result)
  }

  def toLongId(txid: ByteVector, fundingOutputIndex: Int) = {
    require(fundingOutputIndex < 65536, "Index is larger than 65535")
    val part2 = txid(30).^(fundingOutputIndex >> 8).toByte
    val part3 = txid(31).^(fundingOutputIndex).toByte
    txid.take(30) :+ part2 :+ part3
  }

  def hostedChanId(pubkey1: ByteVector, pubkey2: ByteVector) = {
    val pubkey1First: Boolean = LexicographicalOrdering.isLessThan(pubkey1, pubkey2)
    if (pubkey1First) Crypto.sha256(pubkey1 ++ pubkey2) else Crypto.sha256(pubkey2 ++ pubkey1)
  }
}

object Features {
  val OPTION_DATA_LOSS_PROTECT_MANDATORY = 0L
  val OPTION_DATA_LOSS_PROTECT_OPTIONAL = 1L

  val INITIAL_ROUTING_SYNC_BIT_OPTIONAL = 3L

  val CHANNEL_RANGE_QUERIES_BIT_MANDATORY = 6L
  val CHANNEL_RANGE_QUERIES_BIT_OPTIONAL = 7L

  val VARIABLE_LENGTH_ONION_MANDATORY = 8L
  val VARIABLE_LENGTH_ONION_OPTIONAL = 9L

  val PAYMENT_SECRET_MANDATORY = 14L
  val PAYMENT_SECRET_OPTIONAL = 15L

  val BASIC_MULTI_PART_PAYMENT_MANDATORY = 16L
  val BASIC_MULTI_PART_PAYMENT_OPTIONAL = 17L

  def hasFeature(features: ByteVector, bit: Long): Boolean = hasFeature(features.bits, bit)
  def hasFeature(features: BitVector, bit: Long): Boolean = if (features sizeLessThanOrEqual bit) false else features.reverse.get(bit)
  def isBitSet(requiredPosition: Int, bitField: Byte): Boolean = bitField.&(1 << requiredPosition) == (1 << requiredPosition)

  def isNodeSupported(features: ByteVector) =
    areSupported(Set(OPTION_DATA_LOSS_PROTECT_MANDATORY, CHANNEL_RANGE_QUERIES_BIT_MANDATORY,
      VARIABLE_LENGTH_ONION_MANDATORY, PAYMENT_SECRET_MANDATORY, BASIC_MULTI_PART_PAYMENT_MANDATORY),
      features.bits.reverse)

  def areSupported(mandatoryFeatures: Set[Long], reversedFeatures: BitVector): Boolean = {
    def mandatoryUnsupported(n: Long) = reversedFeatures.get(n) && !mandatoryFeatures.contains(n)
    !(0L until reversedFeatures.length by 2 exists mandatoryUnsupported)
  }
}

class LightningException(reason: String = "Lightning related failure") extends RuntimeException(reason)
case class CMDAddImpossible(rd: RoutingData, code: Int, hint: Long = 0L) extends LightningException

// STATE MACHINE

abstract class StateMachine[T] {
  def become(freshData: T, freshState: String) =
    runAnd { data = freshData } { state = freshState }

  def doProcess(change: Any)
  var state: String = _
  var data: T = _
}