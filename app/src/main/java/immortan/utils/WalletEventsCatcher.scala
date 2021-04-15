package immortan.utils

import fr.acinq.eclair.blockchain.electrum.ElectrumClient._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet._
import fr.acinq.eclair.blockchain.CurrentBlockCount
import immortan.crypto.Tools.none
import akka.actor.Actor


class WalletEventsCatcher extends Actor {
  var listeners: Set[WalletEventsListener] = Set.empty
  context.system.eventStream.subscribe(channel = classOf[WalletEvent], subscriber = self)
  context.system.eventStream.subscribe(channel = classOf[ElectrumEvent], subscriber = self)

  override def receive: Receive = {
    case listener: WalletEventsListener => listeners += listener

    case event: WalletReady => for (lst <- listeners) lst.onChainSynchronized(event)

    case event: TransactionReceived => for (lst <- listeners) lst.onTransactionReceived(event)

    case ElectrumDisconnected => for (lst <- listeners) lst.onChainDisconnected
  }
}

class WalletEventsListener {
  def onChainSynchronized(event: WalletReady): Unit = none
  def onTransactionReceived(event: TransactionReceived): Unit = none
  def onChainDisconnected: Unit = none
}
