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

  context.system.eventStream.subscribe(channel = classOf[CurrentBlockCount], subscriber = self)

  override def receive: Receive = {
    case listener: WalletEventsListener => listeners += listener

    case event: WalletReady => for (lst <- listeners) lst.onWalletReady(event)

    case event: CurrentBlockCount => for (lst <- listeners) lst.onCurrentBlockCount(event)

    case event: TransactionReceived => for (lst <- listeners) lst.onTransactionReceived(event)

    case event: TransactionConfidenceChanged => for (lst <- listeners) lst.onTransactionConfidenceChanged(event)

    case event: NewWalletReceiveAddress => for (lst <- listeners) lst.onNewWalletReceiveAddress(event)

    case ElectrumDisconnected => for (lst <- listeners) lst.onElectrumDisconnected
  }
}

class WalletEventsListener {
  def onWalletReady(event: WalletReady): Unit = none
  def onCurrentBlockCount(event: CurrentBlockCount): Unit = none
  def onTransactionReceived(event: TransactionReceived): Unit = none
  def onTransactionConfidenceChanged(event: TransactionConfidenceChanged): Unit = none
  def onNewWalletReceiveAddress(event: NewWalletReceiveAddress): Unit = none
  def onElectrumDisconnected: Unit = none
}
