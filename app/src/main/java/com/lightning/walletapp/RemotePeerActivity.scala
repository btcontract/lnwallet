package com.lightning.walletapp

import fr.acinq.eclair._
import fr.acinq.eclair.Features._
import com.lightning.walletapp.R.string._
import android.widget.{LinearLayout, ProgressBar, TextView}
import immortan.{CommsTower, ConnectionListener, LNParams, RemoteNodeInfo}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import com.ornach.nobobutton.NoboButton
import immortan.utils.{InputParser, ThrottledWork}
import immortan.crypto.Tools._
import fr.acinq.eclair.wire.Init
import android.os.Bundle
import android.view.View
import androidx.appcompat.app.AlertDialog
import fr.acinq.bitcoin.Satoshi

import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import immortan.fsm.NCFunderOpenHandler
import rx.lang.scala.Observable

import scala.util.Success


class RemotePeerActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val peerNodeKey = findViewById(R.id.peerNodeKey).asInstanceOf[TextView]
  private[this] lazy val peerIpAddress = findViewById(R.id.peerIpAddress).asInstanceOf[TextView]

  private[this] lazy val progressBar = findViewById(R.id.progressBar).asInstanceOf[ProgressBar]

  private[this] lazy val peerDetails = findViewById(R.id.peerDetails).asInstanceOf[LinearLayout]
  private[this] lazy val viewNoFeatureSupport = findViewById(R.id.viewNoFeatureSupport).asInstanceOf[TextView]
  private[this] lazy val viewYesFeatureSupport = findViewById(R.id.viewYesFeatureSupport).asInstanceOf[LinearLayout]
  private[this] lazy val optionHostedChannel = findViewById(R.id.optionHostedChannel).asInstanceOf[NoboButton]
  private[this] lazy val remotePeerMain = findViewById(R.id.remotePeerMain).asInstanceOf[LinearLayout]

  private[this] lazy val criticalFeatures = Set(OptionDataLossProtect, BasicMultiPartPayment, StaticRemoteKey)

  private[this] lazy val featureTextViewMap = Map(
    ChannelRangeQueriesExtended -> findViewById(R.id.ChannelRangeQueriesExtended).asInstanceOf[TextView],
    OptionDataLossProtect -> findViewById(R.id.OptionDataLossProtect).asInstanceOf[TextView],
    BasicMultiPartPayment -> findViewById(R.id.BasicMultiPartPayment).asInstanceOf[TextView],
    TrampolineRouting -> findViewById(R.id.TrampolineRouting).asInstanceOf[TextView],
    StaticRemoteKey -> findViewById(R.id.StaticRemoteKey).asInstanceOf[TextView],
    HostedChannels -> findViewById(R.id.HostedChannels).asInstanceOf[TextView],
    ChainSwap -> findViewById(R.id.ChainSwap).asInstanceOf[TextView],
    Wumbo -> findViewById(R.id.Wumbo).asInstanceOf[TextView]
  )

  val baseListener: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = UITask {
      val check = Features.canUseFeature(LNParams.ourInit.features, theirInit.features, _: Feature)
      criticalSupportAvailable = criticalFeatures.forall(check)

      featureTextViewMap.foreach {
        case (feature, view) => view setText feature.rfcName
      }

      featureTextViewMap foreach {
        case (feature, view) if check(feature) => view.setBackgroundResource(R.drawable.border_green)
        case (feature, view) if criticalFeatures.contains(feature) => view.setBackgroundResource(R.drawable.border_red)
        case (_, view) => view.setBackgroundResource(R.drawable.border_gray)
      }

      TransitionManager.beginDelayedTransition(remotePeerMain)
      viewNoFeatureSupport setVisibility BaseActivity.viewMap(!criticalSupportAvailable)
      viewYesFeatureSupport setVisibility BaseActivity.viewMap(criticalSupportAvailable)
      optionHostedChannel setVisibility BaseActivity.viewMap(check apply HostedChannels)
      peerDetails setVisibility View.VISIBLE
      progressBar setVisibility View.GONE
    }.run

    override def onDisconnect(worker: CommsTower.Worker): Unit = UITask {
      CommsTower.rmListenerNative(remoteNodeInfo, baseListener)
      WalletApp.app.quickToast(R.string.rpa_disconnect)
      finish
    }.run
  }

  private var whenBackPressed: Runnable = UITask(finish)
  private var criticalSupportAvailable: Boolean = false
  private var remoteNodeInfo: RemoteNodeInfo = _

  override def checkExternalData(whenNone: Runnable): Unit =
    InputParser.checkAndMaybeErase {
      case remoteInfo: RemoteNodeInfo =>
        peerIpAddress.setText(remoteInfo.address.toString)
        peerNodeKey.setText(remoteInfo.nodeId.toString.humanFour)
        CommsTower.listenNative(listeners1 = Set(baseListener), remoteInfo)
        whenBackPressed = UITask(CommsTower disconnectNative remoteInfo)
        remoteNodeInfo = remoteInfo

      case _ =>
        // Not interested in anything else
        // so we retreat back right away
        finish
    }

  override def onBackPressed: Unit =
    whenBackPressed.run

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_remote_peer)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // BUTTON ACTIONS

  def openNewChannel(view: View): Unit = {
    val body = getLayoutInflater.inflate(R.layout.frag_input_fund_channel, null)
    val rateManager = new RateManager(body, None, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode, asMsat = false)
    val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.toMilliSatoshi, Colors.cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.toMilliSatoshi)
    val builder = titleBodyAsViewBuilder(getString(rpa_open_nc), rateManager.content)
    val feeView = new FeeView(body)

    val dummyLocal = randomKey.publicKey
    val dummyRemote = randomKey.publicKey

    val worker: ThrottledWork[Satoshi, MakeFundingTxResponse] = new ThrottledWork[Satoshi, MakeFundingTxResponse] {
      def work(amount: Satoshi): Observable[MakeFundingTxResponse] = Observable from NCFunderOpenHandler.makeFunding(LNParams.chainWallet, dummyLocal, dummyRemote, amount)
      def process(amount: Satoshi, response: MakeFundingTxResponse): Unit = feeView.update(NCFunderOpenHandler.defFeerate, response.fee.toMilliSatoshi.toSome, showIssue = false).run
      def error(exc: Throwable): Unit =
        println(s"-- $exc")
        feeView.update(NCFunderOpenHandler.defFeerate, None, showIssue = true).run
    }

    def notifyTooSmallFunding = UITask {
      val min = LNParams.minFundingSatoshis.toMilliSatoshi
      val minHuman = LNParams.denomination.parsedWithSign(min, Colors.cardZero)
      WalletApp.app quickToast getString(rpa_funding_too_small).format(minHuman)
    }

    def attempt(alert: AlertDialog): Unit =
      NCFunderOpenHandler.makeFunding(LNParams.chainWallet, dummyLocal, dummyRemote, rateManager.result.truncateToSatoshi) onComplete {
        case Success(reply) if reply.fundingAmount < LNParams.minFundingSatoshis => notifyTooSmallFunding.run
        case Success(reply) =>
        case _ =>
      }

    feeView.update(NCFunderOpenHandler.defFeerate, None, showIssue = false).run
    rateManager.hintDenom.setText(getString(dialog_can_send).format(canSend).html)
    rateManager.hintFiatDenom.setText(getString(dialog_can_send).format(canSendFiat).html)
    rateManager.inputAmount addTextChangedListener onTextChange { _ => worker addWork rateManager.result.truncateToSatoshi }
    mkCheckFormNeutral(attempt, none, _ => rateManager.inputAmount.setText(WalletApp.lastChainBalance.toLong.toString), builder, dialog_ok, dialog_cancel, dialog_max)
  }

  def sharePeerSpecificNodeId(view: View): Unit =
    share(remoteNodeInfo.nodeSpecificPubKey.toString)

  def requestHostedChannel(view: View): Unit = {

  }
}
