package com.lightning.walletapp

import immortan._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import fr.acinq.eclair.Features._
import com.lightning.walletapp.R.string._
import immortan.utils.{InputParser, Rx, ThrottledWork}
import android.widget.{LinearLayout, ProgressBar, TextView}
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import com.lightning.walletapp.BaseActivity.StringOps
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import androidx.appcompat.app.AlertDialog
import com.ornach.nobobutton.NoboButton
import immortan.fsm.NCFunderOpenHandler
import fr.acinq.eclair.wire.Init
import fr.acinq.bitcoin.Satoshi
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

  val baseListener: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, theirInit: Init): Unit = UITask {
      val checkFeature = Features.canUseFeature(LNParams.ourInit.features, theirInit.features, _: Feature)
      featureTextViewMap.foreach { case (feature, textView) => textView setText feature.rfcName }
      criticalSupportAvailable = criticalFeatures.forall(checkFeature)

      featureTextViewMap foreach {
        case (feature, view) if checkFeature(feature) => view.setBackgroundResource(R.drawable.border_green)
        case (feature, view) if criticalFeatures.contains(feature) => view.setBackgroundResource(R.drawable.border_red)
        case (_, view) => view.setBackgroundResource(R.drawable.border_gray)
      }

      switchView(showProgress = false)
      viewNoFeatureSupport setVisibility BaseActivity.viewMap(!criticalSupportAvailable)
      viewYesFeatureSupport setVisibility BaseActivity.viewMap(criticalSupportAvailable)
      optionHostedChannel setVisibility BaseActivity.viewMap(checkFeature apply HostedChannels)
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
    val manager = new RateManager(body, extraHint = None, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
    val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.toMilliSatoshi, Colors.cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.toMilliSatoshi)

    def useMax(alert: AlertDialog): Unit = {
      val balanceAsLong = WalletApp.lastChainBalance.toLong
      manager.inputAmount.setText(balanceAsLong.toString)
    }

    def attempt(alert: AlertDialog): Unit = {
      NCFunderOpenHandler.makeFunding(LNParams.chainWallet, manager.resultSat) foreach { fakeFunding =>
        new NCFunderOpenHandler(remoteNodeInfo, fakeFunding, LNParams.chainWallet, LNParams.cm) {
          override def onEstablished(freshChannel: ChannelNormal): Unit = onBackPressed.run

          override def onFailure(reason: Throwable): Unit = {
            // Put peer details back and show error details
            switchView(showProgress = false)
            onFail(reason)
          }
        }
      }

      // Show loader while opening
      switchView(showProgress = true)
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
      def process(amount: Satoshi, res: MakeFundingTxResponse): Unit = feeView.update(NCFunderOpenHandler.defFeerate, res.fee.toMilliSatoshi.toSome, showIssue = false)
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

  def requestHostedChannel(view: View): Unit = {

  }

  def switchView(showProgress: Boolean): Unit = UITask {
    progressBar setVisibility BaseActivity.viewMap(showProgress)
    peerDetails setVisibility BaseActivity.viewMap(!showProgress)
  }.run
}
