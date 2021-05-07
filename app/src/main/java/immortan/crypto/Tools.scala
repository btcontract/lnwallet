package immortan.crypto

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import scala.concurrent.duration._
import immortan.crypto.StateMachine._
import immortan.crypto.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.eclair.{CltvExpiryDelta, MilliSatoshi, ShortChannelId}
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import fr.acinq.eclair.router.Graph.GraphStructure.GraphEdge
import fr.acinq.eclair.payment.PaymentRequest.ExtraHop
import fr.acinq.eclair.router.Router.ChannelDesc
import fr.acinq.eclair.router.RouteCalculation
import fr.acinq.eclair.crypto.ChaCha20Poly1305
import immortan.crypto.Noise.KeyPair
import java.util.concurrent.TimeUnit
import java.io.ByteArrayInputStream
import language.implicitConversions
import immortan.utils.ThrottledWork
import scala.collection.mutable
import rx.lang.scala.Observable
import scodec.bits.ByteVector
import java.nio.ByteOrder
import scala.util.Try


object Tools {
  type Bytes = Array[Byte]
  type Fiat2Btc = Map[String, Double]
  final val SEPARATOR = " "

  def none: PartialFunction[Any, Unit] = { case _ => }
  def runAnd[T](result: T)(action: Any): T = result

  implicit class Any2Some[T](underlying: T) {
    def toSome: Option[T] = Some(underlying)
  }

  def mapKeys[K, V, K1](items: mutable.Map[K, V], mapper: K => K1, defVal: V): mutable.Map[K1, V] =
    items.map { case (key, value) => mapper(key) -> value } withDefaultValue defVal

  def memoize[In <: Object, Out <: Object](fun: In => Out): LoadingCache[In, Out] = {
    val loader = new CacheLoader[In, Out] { override def load(key: In): Out = fun apply key }
    CacheBuilder.newBuilder.expireAfterAccess(7, TimeUnit.DAYS).maximumSize(2000).build[In, Out](loader)
  }

  def randomBest[T, B](bestItem: T, mapper: T => B, items: Iterable[T] = Nil): T = {
    // Given a list and a best item from it, get random best if there are many equally good items
    val bestItems = items.filter(item => mapper(item) == mapper(bestItem) || item == bestItem)
    scala.util.Random.shuffle(bestItems).head
  }

  def hostedNodesCombined(pubkey1: ByteVector, pubkey2: ByteVector): ByteVector = {
    val pubkey1First: Boolean = LexicographicalOrdering.isLessThan(pubkey1, pubkey2)
    if (pubkey1First) pubkey1 ++ pubkey2 else pubkey2 ++ pubkey1
  }

  def hostedChanId(pubkey1: ByteVector, pubkey2: ByteVector): ByteVector32 = {
    val nodesCombined = hostedNodesCombined(pubkey1, pubkey2)
    Crypto.sha256(nodesCombined)
  }

  def hostedShortChanId(pubkey1: ByteVector, pubkey2: ByteVector): ShortChannelId = {
    val stream = new ByteArrayInputStream(hostedNodesCombined(pubkey1, pubkey2).toArray)
    def getChunk: Long = Protocol.uint64(stream, ByteOrder.BIG_ENDIAN)
    val id = List.fill(8)(getChunk).foldLeft(Long.MaxValue)(_ % _)
    ShortChannelId(id)
  }

  def mkFakeLocalEdge(from: PublicKey, toPeer: PublicKey): GraphEdge = {
    // Augments a graph with local edge corresponding to our local channel
    // Parameters do not matter except that it must point to real peer

    val zeroCltvDelta = CltvExpiryDelta(0)
    val randomShortChannelId = ShortChannelId(secureRandom.nextLong)
    val fakeDesc = ChannelDesc(randomShortChannelId, from, to = toPeer)
    val fakeHop = ExtraHop(from, randomShortChannelId, MilliSatoshi(0L), 0L, zeroCltvDelta)
    GraphEdge(updExt = RouteCalculation.toFakeUpdate(fakeHop), desc = fakeDesc)
  }

  def randomKeyPair: KeyPair = {
    val pk: PrivateKey = randomKey
    KeyPair(pk.publicKey.value, pk.value)
  }

  def chaChaEncrypt(key: ByteVector32, nonce: ByteVector, data: ByteVector): ByteVector = {
    val (ciphertext, mac) = ChaCha20Poly1305.encrypt(key, nonce, data, ByteVector.empty)
    mac ++ nonce ++ ciphertext // 16b + 12b + variable size
  }

  def chaChaDecrypt(key: ByteVector32, data: ByteVector): Try[ByteVector] = Try {
    ChaCha20Poly1305.decrypt(key, nonce = data drop 16 take 12, ciphertext = data drop 28, ByteVector.empty, mac = data take 16)
  }
}

trait CanBeShutDown {
  def becomeShutDown: Unit
}

trait CanBeRepliedTo {
  def process(reply: Any): Unit
}

object StateMachine {
  var INTERVAL: Int = 90
}

abstract class StateMachine[T] { me =>
  def become(freshData: T, freshState: String): StateMachine[T] = {
    // Update state, data and return itself for easy chaining operations
    state = freshState
    data = freshData
    this
  }

  def doProcess(change: Any): Unit
  var state: String = _
  var data: T = _

  var secondsLeft: Long = INTERVAL

  lazy val delayedCMDWorker: ThrottledWork[String, Long] = new ThrottledWork[String, Long] {
    def work(cmd: String): Observable[Long] = Observable.interval(1.second).doOnSubscribe { secondsLeft = INTERVAL }
    def error(canNotHappen: Throwable): Unit = none

    def process(cmd: String, tickInterval: Long): Unit = {
      secondsLeft = INTERVAL - math.min(INTERVAL, tickInterval + 1)
      if (secondsLeft <= 0L) runAnd(unsubscribeCurrentWork)(me doProcess cmd)
    }
  }
}