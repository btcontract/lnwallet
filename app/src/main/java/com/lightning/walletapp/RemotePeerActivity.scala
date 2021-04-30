package com.lightning.walletapp

import immortan._
import fr.acinq.eclair._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.Features._
import com.lightning.walletapp.R.string._
import fr.acinq.bitcoin.{ByteVector32, Satoshi}
import immortan.utils.{InputParser, Rx, ThrottledWork}
import android.widget.{LinearLayout, ProgressBar, TextView}
import immortan.fsm.{HCOpenHandler, NCFundeeOpenHandler, NCFunderOpenHandler}
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import com.lightning.walletapp.BaseActivity.StringOps
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import androidx.appcompat.app.AlertDialog
import com.ornach.nobobutton.NoboButton
import rx.lang.scala.Observable
import android.os.Bundle
import android.view.View


class RemotePeerActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val peerNodeKey = findViewById(R.id.peerNodeKey).asInstanceOf[TextView]
  private[this] lazy val peerIpAddress = findViewById(R.id.peerIpAddress).asInstanceOf[TextView]

  private[this] lazy val progressBar = findViewById(R.id.progressBar).asInstanceOf[ProgressBar]
  private[this] lazy val peerDetails = findViewById(R.id.peerDetails).asInstanceOf[LinearLayout]
  private[this] lazy val viewNoFeatureSupport = findViewById(R.id.viewNoFeatureSupport).asInstanceOf[TextView]
  private[this] lazy val viewYesFeatureSupport = findViewById(R.id.viewYesFeatureSupport).asInstanceOf[LinearLayout]
  private[this] lazy val optionHostedChannel = findViewById(R.id.optionHostedChannel).asInstanceOf[NoboButton]

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

  class DisconnectListener extends ConnectionListener {
    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      UITask(WalletApp.app quickToast R.string.rpa_disconnected).run
      disconnectListenersAndFinish
    }
  }

  private lazy val incomingAcceptingListener = new DisconnectListener {
    override def onMessage(worker: CommsTower.Worker, message: LightningMessage): Unit = message match {
      case theirOpen: OpenChannel if (theirOpen.channelFlags & 0x01) == 0 => acceptIncomingChannel(theirOpen)
      case _: OpenChannel => UITask(WalletApp.app quickToast error_rejected_incoming_public).run
      case _ => // Do nothing
    }
  }

  private lazy val incomingIgnoringListener = new DisconnectListener

  private lazy val viewUpdatingListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = UITask {
      val checkFeature = Features.canUseFeature(LNParams.ourInit.features, theirInit.features, _: Feature)
      val criticalSupportAvailable = criticalFeatures.forall(checkFeature)

      featureTextViewMap foreach {
        case (feature, view) if checkFeature(feature) =>
          view.setBackgroundResource(R.drawable.border_green)
          view.setText(feature.rfcName)

        case (feature, view) if criticalFeatures.contains(feature) =>
          view.setBackgroundResource(R.drawable.border_red)
          view.setText(feature.rfcName)

        case (feature, view) =>
          view.setBackgroundResource(R.drawable.border_gray)
          view.setText(feature.rfcName)
      }

      switchView(showProgress = false)
      viewNoFeatureSupport setVisibility BaseActivity.viewMap(!criticalSupportAvailable)
      viewYesFeatureSupport setVisibility BaseActivity.viewMap(criticalSupportAvailable)
      optionHostedChannel setVisibility BaseActivity.viewMap(checkFeature apply HostedChannels)
    }.run
  }

  private var whenBackPressed: Runnable = UITask(finish)
  private var remoteNodeInfo: RemoteNodeInfo = _

  override def checkExternalData(whenNone: Runnable): Unit =
    InputParser.checkAndMaybeErase {
      case remoteInfo: RemoteNodeInfo =>
        peerIpAddress.setText(remoteInfo.address.toString)
        peerNodeKey.setText(remoteInfo.nodeId.toString.humanFour)
        CommsTower.listenNative(Set(viewUpdatingListener, incomingAcceptingListener), remoteInfo)
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

  def acceptIncomingChannel(theirOpen: OpenChannel): Unit = {
    new NCFundeeOpenHandler(remoteNodeInfo, theirOpen, LNParams.cm) {
      override def onEstablished(chan: ChannelNormal): Unit = disconnectListenersAndFinish
      override def onFailure(reason: Throwable): Unit = revertAndInform(reason)
    }

    switchView(showProgress = true)
    stopAcceptingIncomingOffers
  }

  def fundNewChannel(view: View): Unit = {
    val body = getLayoutInflater.inflate(R.layout.frag_input_fund_channel, null)
    val manager = new RateManager(body, extraHint = None, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
    val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, Colors.cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.totalBalance)

    def useMax(alert: AlertDialog): Unit = {
      val balanceSatLong = WalletApp.lastChainBalance.totalSatLong
      manager.inputAmount.setText(balanceSatLong.toString)
    }

    def attempt(alert: AlertDialog): Unit = {
      NCFunderOpenHandler.makeFunding(LNParams.chainWallet, manager.resultSat) foreach { fakeFunding =>
        new NCFunderOpenHandler(remoteNodeInfo, fakeFunding, LNParams.chainWallet, LNParams.cm) {
          override def onEstablished(chan: ChannelNormal): Unit = disconnectListenersAndFinish
          override def onFailure(reason: Throwable): Unit = revertAndInform(reason)
        }
      }

      switchView(showProgress = true)
      stopAcceptingIncomingOffers
      alert.dismiss
    }

    val builder = titleBodyAsViewBuilder(title = getString(rpa_open_nc), body = manager.content)
    val alert = mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, dialog_max)

    val feeView = new FeeView(body) {
      override def update(rate: FeeratePerKw, feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = {
        // We update fee view and button availability at once so user can't proceed if there are tx issues
        updateButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(rate, feeOpt, showIssue)
      }
    }

    val worker = new ThrottledWork[Satoshi, MakeFundingTxResponse] {
      def work(amount: Satoshi): Observable[MakeFundingTxResponse] = Rx fromFutureOnIo NCFunderOpenHandler.makeFunding(LNParams.chainWallet, amount)
      def process(amount: Satoshi, res: MakeFundingTxResponse): Unit = feeView.update(NCFunderOpenHandler.defFeerate, Some(res.fee.toMilliSatoshi), showIssue = false)
      def error(exc: Throwable): Unit = feeView.update(NCFunderOpenHandler.defFeerate, None, showIssue = true)
    }

    manager.inputAmount addTextChangedListener onTextChange { _ =>
      if (manager.resultSat >= LNParams.minFundingSatoshis) worker.addWork(manager.resultSat)
      else feeView.update(NCFunderOpenHandler.defFeerate, feeOpt = None, showIssue = false)
    }

    manager.hintDenom.setText(getString(dialog_can_send).format(canSend).html)
    manager.hintFiatDenom.setText(getString(dialog_can_send).format(canSendFiat).html)
    feeView.update(NCFunderOpenHandler.defFeerate, feeOpt = None, showIssue = false).run
  }

  def sharePeerSpecificNodeId(view: View): Unit =
    share(remoteNodeInfo.nodeSpecificPubKey.toString)

  def requestHostedChannel(view: View): Unit =
    mkCheckForm(alertDialog => removeAndProceedWithTimeout(alertDialog)(doRequestHostedChannel), none,
      new AlertDialog.Builder(me).setTitle(rpa_request_hc).setMessage(getString(rpa_hc_warn).html), dialog_ok, dialog_cancel)

  private def doRequestHostedChannel: Unit = {
    // Switch view first since HC may throw immediately
    switchView(showProgress = true)
    stopAcceptingIncomingOffers

    // We only need local params to extract defaultFinalScriptPubKey
    val localParams = LNParams.makeChannelParams(remoteNodeInfo, LNParams.chainWallet, isFunder = false, LNParams.minFundingSatoshis)
    new HCOpenHandler(remoteNodeInfo, peerSpecificSecret = ByteVector32.Zeroes, localParams.defaultFinalScriptPubKey, LNParams.cm) {
      def onEstablished(channel: ChannelHosted): Unit = disconnectListenersAndFinish
      def onFailure(reason: Throwable): Unit = revertAndInform(reason)
    }
  }

  def switchView(showProgress: Boolean): Unit = UITask {
    progressBar setVisibility BaseActivity.viewMap(showProgress)
    peerDetails setVisibility BaseActivity.viewMap(!showProgress)
  }.run

  def revertAndInform(reason: Throwable): Unit = {
    // Whatever the reason for this to happen we may accept new incoming offers
    CommsTower.listenNative(Set(incomingAcceptingListener), remoteNodeInfo)
    switchView(showProgress = false)
    onFail(reason)
  }

  def stopAcceptingIncomingOffers: Unit = {
    CommsTower.listenNative(Set(incomingIgnoringListener), remoteNodeInfo)
    CommsTower.rmListenerNative(remoteNodeInfo, incomingAcceptingListener)
  }

  def disconnectListenersAndFinish: Unit = {
    CommsTower.rmListenerNative(remoteNodeInfo, incomingAcceptingListener)
    CommsTower.rmListenerNative(remoteNodeInfo, incomingIgnoringListener)
    CommsTower.rmListenerNative(remoteNodeInfo, viewUpdatingListener)
    finish
  }
}
