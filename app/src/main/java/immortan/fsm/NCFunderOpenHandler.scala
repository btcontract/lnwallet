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
  val dummyLocal: PublicKey = randomKey.publicKey
  val dummyRemote: PublicKey = randomKey.publicKey

  val defFeerate: FeeratePerKw = {
    val target = LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget
    LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(target)
  }

  def makeFunding(chainWallet: WalletExt, fundingAmount: Satoshi, local: PublicKey = dummyLocal, remote: PublicKey = dummyRemote, feeratePerKw: FeeratePerKw = defFeerate): Future[MakeFundingTxResponse] =
    chainWallet.wallet.makeFundingTx(Script.write(Script pay2wsh Scripts.multiSig2of2(local, remote).toList), fundingAmount, feeratePerKw)
}

abstract class NCFunderOpenHandler(info: RemoteNodeInfo, fakeFunding: MakeFundingTxResponse, cWallet: WalletExt, cm: ChannelMaster) {
  // Important: this must be initiated when chain tip is actually known
  def onEstablished(channel: ChannelNormal): Unit
  def onFailure(err: Throwable): Unit

  private val tempChannelId: ByteVector32 = randomBytes32
  private val freshChannel = new ChannelNormal(cm.chanBag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, info.nodeSpecificPair)
    def STORE(normalData: PersistentChannelData): PersistentChannelData = cm.chanBag.put(normalData)
    var chainWallet: WalletExt = cWallet
  }

  private var assignedChanId = Option.empty[ByteVector32]
  private val makeChanListener = new ConnectionListener with ChannelListener { me =>
    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case msg: HasTemporaryChannelId if msg.temporaryChannelId == tempChannelId => freshChannel process msg
      case msg: HasChannelId if assignedChanId.contains(msg.channelId) => freshChannel process msg
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
        val future = NCFunderOpenHandler.makeFunding(cWallet, data.initFunder.fakeFunding.fundingAmount, data.lastSent.fundingPubkey, data.remoteParams.fundingPubKey)
        future onComplete { case Failure(reason) => onException(freshChannel, data, reason) case Success(realFunding) => freshChannel process realFunding }

      case (_, _: DATA_WAIT_FOR_FUNDING_INTERNAL, data: DATA_WAIT_FOR_FUNDING_SIGNED, WAIT_FOR_ACCEPT, WAIT_FOR_ACCEPT) =>
        // Once funding tx becomes known peer will start sending messages using a real channel ID, not a temp one
        assignedChanId = Some(data.channelId)

      case (_, _, data: DATA_WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_ACCEPT, WAIT_FUNDING_DONE) =>
        // On disconnect we remove this listener from CommsTower, but retain it as channel listener
        // this ensures successful implanting if disconnect happens while funding is being published
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
