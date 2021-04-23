package com.lightning.walletapp

import fr.acinq.eclair.Features._
import fr.acinq.eclair.{Feature, Features}
import android.widget.{LinearLayout, ProgressBar, TextView}
import immortan.{CommsTower, ConnectionListener, LNParams, RemoteNodeInfo}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import com.ornach.nobobutton.NoboButton
import immortan.utils.InputParser
import fr.acinq.eclair.wire.Init
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

  }

  def sharePeerSpecificNodeId(view: View): Unit =
    share(remoteNodeInfo.nodeSpecificPubKey.toString)

  def requestHostedChannel(view: View): Unit = {

  }
}
