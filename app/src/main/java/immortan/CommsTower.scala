package immortan

import fr.acinq.eclair._
import scala.concurrent._
import fr.acinq.eclair.wire._
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import immortan.crypto.Tools.{Bytes, none}
import rx.lang.scala.{Subscription, Observable}
import java.util.concurrent.{ConcurrentHashMap, Executors}
import fr.acinq.eclair.wire.LightningMessageCodecs.lightningMessageCodecWithFallback
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Noise.KeyPair
import fr.acinq.eclair.Features
import scala.collection.mutable
import scodec.bits.ByteVector
import java.net.Socket


case class KeyPairAndPubKey(keyPair: KeyPair, them: PublicKey)

object CommsTower {
  type Listeners = Set[ConnectionListener]
  val workers: mutable.Map[KeyPairAndPubKey, Worker] = new ConcurrentHashMap[KeyPairAndPubKey, Worker].asScala
  val listeners: mutable.Map[KeyPairAndPubKey, Listeners] = new ConcurrentHashMap[KeyPairAndPubKey, Listeners].asScala withDefaultValue Set.empty

  def listen(listeners1: Set[ConnectionListener], pair: KeyPairAndPubKey, info: RemoteNodeInfo): Unit = synchronized {
    // Update and either insert a new worker or fire onOperational on NEW listeners if worker currently exists and online
    // First add listeners, then try to add worker because we may already have a connected worker, but no listeners
    listeners(pair) ++= listeners1

    workers.get(pair) match {
      case Some(worker) => worker.theirInit.foreach(worker handleTheirRemoteInitMessage listeners1)
      case None => workers(pair) = new Worker(pair, info, new Bytes(1024), new Socket)
    }
  }

  def sendMany(messages: Traversable[LightningMessage], pair: KeyPairAndPubKey): Unit = CommsTower.workers.get(pair).foreach(messages foreach _.handler.process)
  // Add or remove listeners to a connection where our nodeId is stable, not a randomly generated one (one which makes us seen as a constant peer by remote)
  def listenNative(listeners1: Set[ConnectionListener], remoteInfo: RemoteNodeInfo): Unit = listen(listeners1, remoteInfo.nodeSpecificPair, remoteInfo)
  def rmListenerNative(info: RemoteNodeInfo, listener: ConnectionListener): Unit = listeners(info.nodeSpecificPair) -= listener
  def disconnectNative(info: RemoteNodeInfo): Unit = workers.get(info.nodeSpecificPair).foreach(_.disconnect)

  def forget(pair: KeyPairAndPubKey): Unit = {
    // First remove all listeners, then disconnect
    // this ensures listeners won't try to reconnect

    listeners.remove(pair)
    workers.get(pair).foreach(_.disconnect)
  }

  class Worker(val pair: KeyPairAndPubKey, val info: RemoteNodeInfo, buffer: Bytes, sock: Socket) { me =>
    implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor

    var lastMessage: Long = System.currentTimeMillis
    var theirInit: Option[Init] = Option.empty
    var pinging: Subscription = _

    val handler: TransportHandler =
      new TransportHandler(pair.keyPair, info.nodeId.value) {
        def handleEncryptedOutgoingData(data: ByteVector): Unit =
          try sock.getOutputStream.write(data.toArray) catch {
            case _: Throwable => disconnect
          }

        def handleDecryptedIncomingData(data: ByteVector): Unit = {
          // Prevent pinger from disconnecting or sending pings
          val ourListeners: Listeners = listeners(pair)
          lastMessage = System.currentTimeMillis

          // UnknownMessage is typically a wrapper for non-standard message
          // BUT it may wrap standard messages, too, so call `onMessage` on those
          lightningMessageCodecWithFallback.decode(data.bits).require.value match {
            case message: Init => handleTheirRemoteInitMessage(ourListeners)(remoteInit = message)
            case message: Ping => handler process Pong(ByteVector fromValidHex "00" * message.pongLength)

            case message: UnknownMessage =>
              LightningMessageCodecs.decode(message) match {
                case msg: HostedChannelMessage => for (lst <- ourListeners) lst.onHostedMessage(me, msg)
                case msg: SwapOut => for (lst <- ourListeners) lst.onSwapOutMessage(me, msg)
                case msg: SwapIn => for (lst <- ourListeners) lst.onSwapInMessage(me, msg)
                case msg => for (lst <- ourListeners) lst.onMessage(me, msg)
              }

            case message => for (lst <- ourListeners) lst.onMessage(me, message)
          }
        }

        def handleEnterOperationalState: Unit = {
          pinging = Observable.interval(10.seconds) subscribe { _ =>
            if (lastMessage < System.currentTimeMillis - 50 * 1000L) disconnect
            else if (lastMessage < System.currentTimeMillis - 20 * 1000L) sendPing
          }

          // Send our node parameters
          handler process LNParams.ourInit
        }
      }

    private[this] val thread = Future {
      sock.connect(info.address.socketAddress, 7500)
      handler.init

      while (true) {
        val length = sock.getInputStream.read(buffer, 0, buffer.length)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process ByteVector.view(buffer take length)
      }
    }

    thread onComplete { _ =>
      // Will also run after forget
      try pinging.unsubscribe catch none
      listeners(pair).foreach(_ onDisconnect me)
      // Once disconnected, worker gets removed
      workers -= pair
    }

    def disconnect: Unit = try sock.close catch none
    def handleTheirRemoteInitMessage(listeners1: Set[ConnectionListener] = Set.empty)(remoteInit: Init): Unit = {
      // Use a separate variable for listeners here because a set of listeners provided to this method may be different
      // Account for a case where they disconnect while we are deciding on their features (do nothing in this case)
      // Important: always store their init once obtained, it may be used upstream (see sync listener)
      theirInit = Some(remoteInit)

      if (!thread.isCompleted) {
        val areNetworksOK = remoteInit.networks.intersect(LNParams.ourInit.networks).nonEmpty
        val areFeaturesOK = Features.areCompatible(LNParams.ourInit.features, remoteInit.features)
        if (areNetworksOK && areFeaturesOK) for (lst <- listeners1) lst.onOperational(me, remoteInit)
        else disconnect
      }
    }

    def sendPing: Unit = {
      val payloadLength = secureRandom.nextInt(5) + 1
      val data = randomBytes(length = payloadLength)
      handler process Ping(payloadLength, data)
    }
  }
}

class ConnectionListener {
  def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = none
  def onMessage(worker: CommsTower.Worker, msg: LightningMessage): Unit = none
  def onHostedMessage(worker: CommsTower.Worker, msg: HostedChannelMessage): Unit = none
  def onSwapOutMessage(worker: CommsTower.Worker, msg: SwapOut): Unit = none
  def onSwapInMessage(worker: CommsTower.Worker, msg: SwapIn): Unit = none
  def onDisconnect(worker: CommsTower.Worker): Unit = none
}
