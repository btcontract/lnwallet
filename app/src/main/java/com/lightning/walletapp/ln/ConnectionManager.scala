package com.lightning.walletapp.ln

import scala.concurrent._
import scala.concurrent.duration._
import com.lightning.walletapp.ln.wire._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Features._
import rx.lang.scala.{Subscription, Observable => Obs}
import java.util.concurrent.{ConcurrentHashMap, Executors}
import com.lightning.walletapp.ln.crypto.Noise.KeyPair
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import java.net.Socket


object ConnectionManager {
  var listeners = Set.empty[ConnectionListener]
  val workers = new ConcurrentHashMap[PublicKey, Worker].asScala
  val keyPair = KeyPair(LNParams.keys.extendedNodeKey.publicKey.toBin,
    LNParams.keys.extendedNodeKey.privateKey.toBin)

  protected[this] val events = new ConnectionListener {
    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = for (lst <- listeners) lst.onMessage(nodeId, msg)
    override def onHostedMessage(ann: NodeAnnouncement, msg: HostedChannelMessage) = for (lst <- listeners) lst.onHostedMessage(ann, msg)
    override def onOperational(nodeId: PublicKey, isCompat: Boolean) = for (lst <- listeners) lst.onOperational(nodeId, isCompat)
    override def onDisconnect(nodeId: PublicKey) = for (lst <- listeners) lst.onDisconnect(nodeId)
  }

  def connectTo(ann: NodeAnnouncement, notify: Boolean) = synchronized {
    if (workers.get(ann.nodeId).isEmpty) workers(ann.nodeId) = new Worker(ann)
    else if (notify) events.onOperational(ann.nodeId, isCompat = true)
  }

  class Worker(val ann: NodeAnnouncement, buffer: Bytes = new Bytes(1024), val sock: Socket = new Socket) {
    implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
    private var ourLastPing = Option.empty[Ping]
    private var pinging: Subscription = _

    def disconnect: Unit = try sock.close catch none
    val handler: TransportHandler = new TransportHandler(keyPair, ann.nodeId) {
      def handleEncryptedOutgoingData(data: ByteVector) = try sock.getOutputStream write data.toArray catch handleError
      def handleDecryptedIncomingData(data: ByteVector) = Tuple2(LightningMessageCodecs deserialize data, ourLastPing) match {
        case (init: Init, _) => events.onOperational(isCompat = Features.isNodeSupported(init.localFeatures), nodeId = ann.nodeId)
        case Ping(replyLength, _) \ _ if replyLength > 0 && replyLength <= 65532 => handler process Pong(ByteVector fromValidHex "00" * replyLength)
        case Pong(randomData) \ Some(ourPing) if randomData.size == ourPing.pongLength => ourLastPing = None
        case (message: HostedChannelMessage, _) => events.onHostedMessage(ann, message)
        case (message, _) => events.onMessage(ann.nodeId, message)
      }

      def handleEnterOperationalState = {
        handler process Init(LNParams.globalFeatures, LNParams.localFeatures)
        pinging = Obs.interval(15.seconds).map(_ => random.nextInt(10) + 1) subscribe { length =>
          val ourNextPing = Ping(data = ByteVector.view(random getBytes length), pongLength = length)
          if (ourLastPing.isEmpty) handler process ourNextPing else disconnect
          ourLastPing = Some(ourNextPing)
        }
      }

      // Just disconnect immediately in all cases
      def handleError = { case _ => disconnect }
    }

    val thread = Future {
      // Always use the first available address, it's safe it throw here
      val workingAddress = NodeAddress toInetSocketAddress ann.addresses.head
      sock.connect(workingAddress, 7500)
      handler.init

      while (true) {
        val length = sock.getInputStream.read(buffer, 0, buffer.length)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process ByteVector.view(buffer take length)
      }
    }

    thread onComplete { _ =>
      workers.remove(ann.nodeId)
      events.onDisconnect(ann.nodeId)
      try pinging.unsubscribe catch none
    }
  }
}

class ConnectionListener {
  def onOpenOffer(nodeId: PublicKey, msg: OpenChannel): Unit = none
  def onMessage(nodeId: PublicKey, msg: LightningMessage): Unit = none
  def onHostedMessage(ann: NodeAnnouncement, msg: HostedChannelMessage): Unit = none
  def onOperational(nodeId: PublicKey, isCompat: Boolean): Unit = none
  def onDisconnect(nodeId: PublicKey): Unit = none
}