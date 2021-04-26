package immortan.fsm

import immortan._
import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import fr.acinq.eclair.channel._
import scala.util.{Failure, Success}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Script}
import immortan.ChannelListener.{Malfunction, Transition}
import immortan.Channel.{WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE}
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Scripts
import fr.acinq.bitcoin.Crypto.PublicKey
import scala.concurrent.Future


object NCFunderOpenHandler {
  val defFeerate: FeeratePerKw = LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget)
  def makeFunding(chainWallet: WalletExt, local: PublicKey, remote: PublicKey, fundingAmount: Satoshi, fundingTxFeeratePerKw: FeeratePerKw = defFeerate): Future[MakeFundingTxResponse] =
    chainWallet.wallet.makeFundingTx(Script.write(Script pay2wsh Scripts.multiSig2of2(local, remote).toList), fundingAmount, fundingTxFeeratePerKw)
}

// Important: this must be initiated when chain tip is actually known
abstract class NCFunderOpenHandler(info: RemoteNodeInfo, tempChannelId: ByteVector32,
                                   fakeFunding: MakeFundingTxResponse, cWallet: WalletExt,
                                   cm: ChannelMaster) {

  def onEstablished(channel: ChannelNormal): Unit
  def onFailure(err: Throwable): Unit

  val freshChannel: ChannelNormal = new ChannelNormal(cm.chanBag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, info.nodeSpecificPair)
    def STORE(normalData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(normalData)
    var chainWallet: WalletExt = cWallet
  }

  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasTemporaryChannelId if msg.temporaryChannelId == tempChannelId => freshChannel process message
      case msg: HasChannelId if msg.channelId == tempChannelId => freshChannel process message
      case _ => // Do nothing to avoid conflicts
    }

    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit =
      freshChannel process INPUT_INIT_FUNDER(info, tempChannelId, fakeFunding, pushAmount = 0L.msat,
        initialFeeratePerKw = LNParams.feeRatesInfo.onChainFeeConf.getCommitmentFeerate(ChannelVersion.STATIC_REMOTEKEY, None),
        localParams = LNParams.makeChannelParams(info, freshChannel.chainWallet, isFunder = true, fakeFunding.fundingAmount),
        theirInit, channelFlags = 0.toByte, ChannelVersion.STATIC_REMOTEKEY)

    override def onDisconnect(worker: CommsTower.Worker): Unit =
      onException(freshChannel, freshChannel.data, PeerDisconnected)

    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _: DATA_WAIT_FOR_ACCEPT_CHANNEL, data: DATA_WAIT_FOR_FUNDING_INTERNAL, WAIT_FOR_ACCEPT, WAIT_FOR_ACCEPT) =>
        val future = NCFunderOpenHandler.makeFunding(cWallet, data.lastSent.fundingPubkey, data.remoteParams.fundingPubKey, data.initFunder.fakeFunding.fundingAmount)
        future onComplete { case Failure(reason) => onException(freshChannel, data, reason) case Success(realFunding) => freshChannel process realFunding }

      case (_, _, data: DATA_WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE) =>
        // It is up to NC to store itself and communicate successful opening
        cm.implantChannel(data.commitments, freshChannel)
        CommsTower.rmListenerNative(info, me)
        onEstablished(freshChannel)
    }

    override def onException: PartialFunction[Malfunction, Unit] = {
      // Something went wrong while trying to establish a new channel

      case (_, _, error: Throwable) =>
        CommsTower.rmListenerNative(info, me)
        onFailure(error)
    }
  }

  freshChannel.listeners = Set(makeChanListener)
  CommsTower.listenNative(Set(makeChanListener), info)
}
