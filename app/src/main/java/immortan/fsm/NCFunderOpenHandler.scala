package immortan.fsm

import fr.acinq.eclair._
import immortan.{ChannelListener, ChannelMaster, ChannelNormal, CommsTower, ConnectionListener, LNParams, RemoteNodeInfo, WalletExt}
import fr.acinq.eclair.channel.{ChannelVersion, DATA_WAIT_FOR_FUNDING_CONFIRMED, INPUT_INIT_FUNDER, PersistentChannelData}
import fr.acinq.eclair.wire.{HasChannelId, HasTemporaryChannelId, Init, LightningMessage}
import immortan.Channel.{WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE}
import immortan.ChannelListener.{Malfunction, Transition}
import fr.acinq.bitcoin.{ByteVector32, Satoshi}


// Important: this must be initiated when chain tip is actually known
abstract class NCFunderOpenHandler(info: RemoteNodeInfo, tempChannelId: ByteVector32, fundingAmount: Satoshi, wallet: WalletExt, cm: ChannelMaster) {
  def onPeerDisconnect(worker: CommsTower.Worker): Unit
  def onEstablished(channel: ChannelNormal): Unit
  def onFailure(err: Throwable): Unit

  val freshChannel: ChannelNormal = new ChannelNormal(cm.chanBag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, info.nodeSpecificPair)
    def STORE(normalData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(normalData)
    var chainWallet: WalletExt = wallet
  }

  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasTemporaryChannelId if msg.temporaryChannelId == tempChannelId => freshChannel process message
      case msg: HasChannelId if msg.channelId == tempChannelId => freshChannel process message
      case _ => // Do nothing to avoid conflicts
    }

    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = {
      val cmd = INPUT_INIT_FUNDER(info, tempChannelId, fundingAmount, pushAmount = 0L.msat,
        initialFeeratePerKw = LNParams.feeRatesInfo.onChainFeeConf.getCommitmentFeerate(ChannelVersion.STATIC_REMOTEKEY, None),
        fundingTxFeeratePerKw = LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget),
        localParams = LNParams.makeChannelParams(info, freshChannel.chainWallet, isFunder = true, fundingAmount), theirInit, channelFlags = 0.toByte, ChannelVersion.STATIC_REMOTEKEY)

      freshChannel process cmd
    }

    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      // Peer has disconnected during HC opening process
      CommsTower.rmListenerNative(info, me)
      onPeerDisconnect(worker)
    }

    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _, data: DATA_WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE) =>
        // It is up to NC to store itself and communicate successful opening
        cm.implantChannel(data.commitments, freshChannel)
        CommsTower.rmListenerNative(info, me)
        onEstablished(freshChannel)
    }

    override def onException: PartialFunction[Malfunction, Unit] = {
      // Something went wrong while trying to establish a channel

      case (_, error: Throwable) =>
        CommsTower.rmListenerNative(info, me)
        onFailure(error)
    }
  }

  freshChannel.listeners = Set(makeChanListener)
  CommsTower.listenNative(Set(makeChanListener), info)
}
