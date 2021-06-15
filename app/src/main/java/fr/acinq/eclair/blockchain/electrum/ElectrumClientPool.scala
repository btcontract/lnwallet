/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.blockchain.electrum

import akka.actor._
import fr.acinq.bitcoin._
import org.json4s.JsonAST._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool._
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.SSL
import fr.acinq.eclair.blockchain.CurrentBlockCount
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.ExecutionContext
import org.json4s.native.JsonMethods
import java.net.InetSocketAddress
import java.io.InputStream
import scala.util.Random
import immortan.LNParams


class ElectrumClientPool(blockCount: AtomicLong, chainHash: ByteVector32)(implicit val ec: ExecutionContext) extends Actor with FSM[ElectrumClientPool.State, ElectrumClientPool.Data] {

  val serverAddresses: Set[ElectrumServerAddress] = ElectrumClientPool.loadFromChainHash(chainHash)

  val addresses = collection.mutable.Map.empty[ActorRef, InetSocketAddress]

  val statusListeners = collection.mutable.HashSet.empty[ActorRef]

  (0 until LNParams.maxChainConnectionsCount).foreach(_ => self ! Connect)

  context.system.scheduler.schedule(6.hours, 6.hours) {
    // Once in every 6 hours we rotate chain providers
    addresses.keys.foreach(_ ! PoisonPill)
  }

  override def supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy(loggingEnabled = false) {
      case _ => SupervisorStrategy.stop
    }

  startWith(Disconnected, DisconnectedData)

  when(Disconnected) {
    case Event(msg: ElectrumClient.ElectrumReady, _) if addresses.contains(sender) =>
      sender ! ElectrumClient.HeaderSubscription(self)
      handleHeader(sender, msg.height, msg.tip, None)

    case Event(ElectrumClient.AddStatusListener(listener), _) =>
      statusListeners += listener
      stay

    case Event(Terminated(actor), _) =>
      context.system.scheduler.scheduleOnce(5.seconds, self, Connect)
      addresses -= actor
      stay
  }

  when(Connected) {
    case Event(ElectrumClient.ElectrumReady(height, tip, _), data: ConnectedData) if addresses.contains(sender) =>
      sender ! ElectrumClient.HeaderSubscription(self)
      handleHeader(sender, height, tip, data.asSome)

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, tip), data: ConnectedData) if addresses.contains(sender) =>
      handleHeader(sender, height, tip, data.asSome)

    case Event(request: ElectrumClient.Request, data: ConnectedData) =>
      data.master forward request
      stay

    case Event(ElectrumClient.AddStatusListener(listener), data: ConnectedData) if addresses.contains(data.master) =>
      listener ! ElectrumClient.ElectrumReady(data.tips(data.master), addresses(data.master))
      statusListeners += listener
      stay

    case Event(Terminated(actor), data: ConnectedData) =>
      context.system.scheduler.scheduleOnce(5.seconds, self, Connect)
      val tips1 = data.tips - actor
      addresses -= actor

      if (tips1.isEmpty) {
        goto(Disconnected) using DisconnectedData
      } else if (data.master != actor) {
        stay using data.copy(tips = tips1)
      } else {
        val (bestClient, height1 ~ header) = tips1.toSeq.maxBy { case (_, height ~ _) => height }
        handleHeader(bestClient, height1, header, data.copy(tips = tips1).asSome)
      }
  }

  whenUnhandled {
    case Event(Connect, _) =>
      connectToRandomProvider
      stay

    case _ =>
      stay
  }

  onTransition {
    case Connected -> Disconnected =>
      statusListeners.foreach(_ ! ElectrumClient.ElectrumDisconnected)
      context.system.eventStream publish ElectrumClient.ElectrumDisconnected
  }

  initialize

  private def handleHeader(connection: ActorRef, height: Int, tip: BlockHeader, data: Option[ConnectedData] = None) = {
    val remoteAddress = addresses(connection)
    // we update our block count even if it doesn't come from our current master
    updateBlockCount(height)
    data match {
      case None =>
        // as soon as we have a connection to an electrum server, we select it as master
        log.info("selecting master {} at {}", remoteAddress, tip)
        statusListeners.foreach(_ ! ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        context.system.eventStream.publish(ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        goto(Connected) using ConnectedData(connection, Map(connection -> (height, tip)))
      case Some(d) if connection != d.master && height >= d.blockHeight + 2L =>
        // we only switch to a new master if there is a significant difference with our current master, because
        // we don't want to switch to a new master every time a new block arrives (some servers will be notified before others)
        // we check that the current connection is not our master because on regtest when you generate several blocks at once
        // (and maybe on testnet in some pathological cases where there's a block every second) it may seen like our master
        // skipped a block and is suddenly at height + 2
        log.info("switching to master {} at {}", remoteAddress, tip)
        // we've switched to a new master, treat this as a disconnection/reconnection
        // so users (wallet, watcher, ...) will reset their subscriptions
        statusListeners.foreach(_ ! ElectrumClient.ElectrumDisconnected)
        context.system.eventStream.publish(ElectrumClient.ElectrumDisconnected)
        statusListeners.foreach(_ ! ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        context.system.eventStream.publish(ElectrumClient.ElectrumReady(height, tip, remoteAddress))
        goto(Connected) using d.copy(master = connection, tips = d.tips + (connection -> (height, tip)))
      case Some(d) =>
        log.debug("received tip {} from {} at {}", tip, remoteAddress, height)
        stay using d.copy(tips = d.tips + (connection -> (height, tip)))
    }
  }

  private def updateBlockCount(newTip: Long): Unit = if (blockCount.get < newTip) {
    context.system.eventStream publish CurrentBlockCount(newTip)
    blockCount.set(newTip)
  }

  private def connectToRandomProvider: Unit = {
    pickAddress(serverAddresses, addresses.values.toSet) foreach { info =>
      val resolvedAddress = new InetSocketAddress(info.address.getHostName, info.address.getPort)
      val client = context actorOf Props(classOf[ElectrumClient], resolvedAddress, info.ssl, ec)
      client ! ElectrumClient.AddStatusListener(self)
      addresses += (client -> info.address)
      context watch client
    }
  }
}

object ElectrumClientPool {
  case class ElectrumServerAddress(address: InetSocketAddress, ssl: SSL)

  var loadFromChainHash: ByteVector32 => Set[ElectrumServerAddress] = {
    case Block.LivenetGenesisBlock.hash => readServerAddresses(classOf[ElectrumServerAddress].getResourceAsStream("/electrum/servers_mainnet.json"), sslEnabled = false)
    case Block.TestnetGenesisBlock.hash => readServerAddresses(classOf[ElectrumServerAddress].getResourceAsStream("/electrum/servers_testnet.json"), sslEnabled = false)
    case _ => throw new RuntimeException
  }

  def readServerAddresses(stream: InputStream, sslEnabled: Boolean): Set[ElectrumServerAddress] = try {
    val JObject(values) = JsonMethods.parse(stream)
    val addresses = values
      .toMap
      .flatMap {
        case (name, fields)  =>
          if (sslEnabled) {
            // We don't authenticate seed servers (SSL.LOOSE), because:
            // - we don't know them so authentication doesn't really bring anything
            // - most of them have self-signed SSL certificates so it would always fail
            fields \ "s" match {
              case JString(port) => Some(ElectrumServerAddress(InetSocketAddress.createUnresolved(name, port.toInt), SSL.LOOSE))
              case _ => None
            }
          } else {
            fields \ "t" match {
              case JString(port) => Some(ElectrumServerAddress(InetSocketAddress.createUnresolved(name, port.toInt), SSL.OFF))
              case _ => None
            }
          }
      }
    addresses.toSet
  } finally {
    stream.close
  }

  def pickAddress(serverAddresses: Set[ElectrumServerAddress], usedAddresses: Set[InetSocketAddress] = Set.empty): Option[ElectrumServerAddress] =
    Random.shuffle(serverAddresses.filterNot(usedAddresses contains _.address).toSeq).headOption

  sealed trait State
  case object Disconnected extends State
  case object Connected extends State

  sealed trait Data
  case object DisconnectedData extends Data

  type HeightAndHeader = (Int, BlockHeader)
  case class ConnectedData(master: ActorRef, tips: Map[ActorRef, HeightAndHeader] = Map.empty) extends Data {
    def blockHeight: Int = tips.get(master).map { case (height, _) => height }.getOrElse(0)
  }

  case object Connect
}
