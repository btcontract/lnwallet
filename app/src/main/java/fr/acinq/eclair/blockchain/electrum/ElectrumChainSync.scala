package fr.acinq.eclair.blockchain.electrum

import scala.util.{Failure, Success, Try}
import akka.actor.{ActorRef, FSM, PoisonPill}
import fr.acinq.bitcoin.{Block, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{DISCONNECTED, RUNNING, SYNCING, WAITING_FOR_TIP}
import fr.acinq.eclair.blockchain.electrum.Blockchain.RETARGETING_PERIOD
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.GetHeaders
import fr.acinq.eclair.blockchain.electrum.db.HeaderDb


class ElectrumChainSync(client: ActorRef, headerDb: HeaderDb, chainHash: ByteVector32) extends FSM[ElectrumWallet.State, Blockchain] {

  def loadChain: Blockchain = if (chainHash != Block.RegtestGenesisBlock.hash) {
    val blockchain = Blockchain.fromCheckpoints(checkpoints = CheckPoint.load(chainHash, headerDb), chainhash = chainHash)
    val headers = headerDb.getHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, maxCount = Int.MaxValue)
    Blockchain.addHeadersChunk(blockchain, height = blockchain.checkpoints.size * RETARGETING_PERIOD, headers)
  } else Blockchain.fromGenesisBlock(Block.RegtestGenesisBlock.hash, Block.RegtestGenesisBlock.header)

  client ! ElectrumClient.AddStatusListener(self)

  startWith(DISCONNECTED, loadChain)

  when(DISCONNECTED) {
    case Event(_: ElectrumClient.ElectrumReady, blockchain) =>
      client ! ElectrumClient.HeaderSubscription(self)
      goto(WAITING_FOR_TIP) using blockchain
  }

  when(WAITING_FOR_TIP) {
    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if response.height < blockchain.height =>
      log.info("Electrum peer is behind us, killing and switching to new one")
      goto(DISCONNECTED) replying PoisonPill

    case Event(_: ElectrumClient.HeaderSubscriptionResponse, blockchain) if blockchain.bestchain.isEmpty =>
      client ! ElectrumClient.GetHeaders(blockchain.checkpoints.size * RETARGETING_PERIOD, RETARGETING_PERIOD)
      log.info("Performing full chain sync")
      goto(SYNCING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) if response.header == blockchain.tip.header =>
      context.system.eventStream.publish(blockchain)
      goto(RUNNING)

    case Event(response: ElectrumClient.HeaderSubscriptionResponse, blockchain) =>
      client ! ElectrumClient.GetHeaders(blockchain.tip.height + 1, RETARGETING_PERIOD)
      log.info(s"Syncing headers ${blockchain.height} to ${response.height}")
      goto(SYNCING)
  }

  when(SYNCING) {
    case Event(respone: ElectrumClient.GetHeadersResponse, blockchain) if respone.headers.isEmpty =>
      context.system.eventStream.publish(blockchain)
      goto(RUNNING)

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) =>
      val blockchain1Try = Try apply Blockchain.addHeaders(blockchain, start, headers)

      blockchain1Try match {
        case Success(blockchain1) =>
          val (blockchain2, chunks) = Blockchain.optimize(blockchain1)
          for (chunk <- chunks grouped RETARGETING_PERIOD) headerDb.addHeaders(chunk.map(_.header), chunk.head.height)
          log.info(s"Got new headers chunk at ${blockchain2.tip.height}, requesting next chunk")
          client ! ElectrumClient.GetHeaders(blockchain2.tip.height + 1, RETARGETING_PERIOD)
          goto(SYNCING) using blockchain2

        case Failure(error) =>
          log.error("Electrum peer sent bad headers", error)
          goto(DISCONNECTED) replying PoisonPill
      }

    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), _) =>
      log.debug(s"Ignoring header $header at $height while syncing")
      stay
  }

  when(RUNNING) {
    case Event(ElectrumClient.HeaderSubscriptionResponse(height, header), blockchain) if blockchain.tip.header != header =>
      val difficultyOk = Blockchain.getDifficulty(blockchain, height, headerDb).forall(header.bits.==)
      val blockchain1Try = Try apply Blockchain.addHeader(blockchain, height, header)

      blockchain1Try match {
        case Success(blockchain1) if difficultyOk =>
          val (blockchain2, chunks) = Blockchain.optimize(blockchain1)
          for (chunk <- chunks grouped RETARGETING_PERIOD) headerDb.addHeaders(chunk.map(_.header), chunk.head.height)
          log.info(s"Got new chain tip ${header.blockId} at $height, obtained header is $header, publishing chain")
          context.system.eventStream.publish(blockchain2)
          stay using blockchain2

        case _ =>
          log.error("Electrum peer sent bad headers")
          stay replying PoisonPill
      }

    case Event(ElectrumClient.GetHeadersResponse(start, headers, _), blockchain) =>
      val blockchain1Try = Try apply Blockchain.addHeaders(blockchain, start, headers)

      blockchain1Try match {
        case Success(blockchain1) =>
          headerDb.addHeaders(headers, start)
          context.system.eventStream.publish(blockchain1)
          stay using blockchain1

        case _ =>
          log.error("Electrum peer sent bad headers")
          stay replying PoisonPill
      }
  }

  whenUnhandled {
    case Event(getHeaders: GetHeaders, _) =>
      client ! getHeaders
      stay

    case Event(ElectrumClient.ElectrumDisconnected, _) =>
      goto(DISCONNECTED)
  }

  initialize
}
