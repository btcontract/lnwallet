package com.btcontract.wallettest

import android.content.{Context, Intent}
import scala.util.{Failure, Success, Try}
import immortan.crypto.Tools.{none, runAnd}
import android.net.{ConnectivityManager, NetworkCapabilities}
import info.guardianproject.netcipher.proxy.{OrbotHelper, StatusCallback}
import org.ndeftools.util.activity.NfcReaderActivity
import com.ornach.nobobutton.NoboButton
import immortan.utils.InputParser
import android.widget.TextView
import org.ndeftools.Message
import android.os.Bundle
import android.view.View
import immortan.LNParams


object ClassNames {
  val statActivityClass: Class[StatActivity] = classOf[StatActivity]
  val qrSplitActivityClass: Class[QRSplitActivity] = classOf[QRSplitActivity]
  val qrChainActivityClass: Class[QRChainActivity] = classOf[QRChainActivity]
  val settingsActivityClass: Class[SettingsActivity] = classOf[SettingsActivity]
  val qrInvoiceActivityClass: Class[QRInvoiceActivity] = classOf[QRInvoiceActivity]
  val remotePeerActivityClass: Class[RemotePeerActivity] = classOf[RemotePeerActivity]
  val mainActivityClass: Class[MainActivity] = classOf[MainActivity]
  val hubActivityClass: Class[HubActivity] = classOf[HubActivity]
}

class MainActivity extends NfcReaderActivity with BaseActivity { me =>
  lazy val skipOrbotCheck: NoboButton = findViewById(R.id.skipOrbotCheck).asInstanceOf[NoboButton]
  lazy val takeOrbotAction: NoboButton = findViewById(R.id.takeOrbotAction).asInstanceOf[NoboButton]
  lazy val mainOrbotMessage: TextView = findViewById(R.id.mainOrbotMessage).asInstanceOf[TextView]
  lazy val mainOrbotIssues: View = findViewById(R.id.mainOrbotIssues).asInstanceOf[View]
  lazy val mainOrbotCheck: View = findViewById(R.id.mainOrbotCheck).asInstanceOf[View]

  def INIT(state: Bundle): Unit = {
    setContentView(R.layout.activity_main)
    initNfc(state)
  }

  // NFC AND SHARE

  // This method is always run when `onResume` event is fired, should be a starting point for all subsequent checks
  def readNdefMessage(msg: Message): Unit = runInFutureProcessOnUI(InputParser recordValue ndefMessageString(msg), proceed)(proceed)

  override def onNoNfcIntentFound: Unit = {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(data => null != data)
    if (processIntent) runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, proceed)(proceed) else proceed(null)
  }

  def onNfcStateEnabled: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateChange(ok: Boolean): Unit = none
  def readEmptyNdefMessage: Unit = proceed(null)
  def readNonNdefMessage: Unit = proceed(null)

  def proceed(empty: Any): Unit = WalletApp.isAlive match {
    case false => runAnd(WalletApp.makeAlive)(me proceed null)
    case true if LNParams.isOperational => me exitTo ClassNames.hubActivityClass

    case true =>
      val step3 = new EnsureSeed
      val step2 = if (WalletApp.ensureTor) new EnsureTor(step3) else step3
      val step1 = if (WalletApp.useAuth) new EnsureAuth(step2) else step2
      step1.makeAttempt
  }

  // Tor and auth

  trait Step {
    def makeAttempt: Unit
  }

  class EnsureSeed extends Step {
    def makeAttempt: Unit = WalletApp.extDataBag.tryGetSecret match {
      case Failure(_: android.database.CursorIndexOutOfBoundsException) =>
        // Record is not present at all, this is probaby a fresh wallet
        me exitTo classOf[SetupActivity]

      case Failure(reason) =>
        // Notify user about it
        throw reason

      case Success(secret) =>
        WalletApp.makeOperational(secret)
        me exitTo ClassNames.hubActivityClass
    }
  }

  class EnsureAuth(next: Step) extends Step {
    def makeAttempt: Unit = new utils.BiometricAuth(findViewById(R.id.mainLayout), me) {
      def onHardwareUnavailable: Unit = WalletApp.app.quickToast(R.string.fp_not_available)
      def onNoHardware: Unit = WalletApp.app.quickToast(R.string.fp_no_support)
      def onCanAuthenticate: Unit = callAuthDialog
      def onAuthSucceeded: Unit = next.makeAttempt
      def onNoneEnrolled: Unit = next.makeAttempt
    }.checkAuth
  }

  class EnsureTor(next: Step) extends Step {
    private[this] val orbotHelper = OrbotHelper.get(me)
    private[this] val initCallback = new StatusCallback {
      def onStatusTimeout: Unit = showIssue(R.string.orbot_err_unclear, getString(R.string.orbot_action_open), closeAppExitOrbot).run
      def onNotYetInstalled: Unit = showIssue(R.string.orbot_err_not_installed, getString(R.string.orbot_action_install), closeAppInstallOrbot).run
      def onEnabled(intent: Intent): Unit = if (isVPNOn) runAnd(orbotHelper removeStatusCallback this)(next.makeAttempt) else onStatusTimeout
      def onStopping: Unit = onStatusTimeout
      def onDisabled: Unit = none
      def onStarting: Unit = none
    }

    def isVPNOn: Boolean = Try {
      val cm = WalletApp.app.getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]
      cm.getAllNetworks.exists(cm getNetworkCapabilities _ hasTransport NetworkCapabilities.TRANSPORT_VPN)
    } getOrElse false

    def closeAppExitOrbot: Unit = {
      val pack = OrbotHelper.ORBOT_PACKAGE_NAME
      val intent = getPackageManager getLaunchIntentForPackage pack
      Option(intent).foreach(startActivity)
      finishAffinity
      System exit 0
    }

    def closeAppInstallOrbot: Unit = {
      orbotHelper installOrbot me
      finishAffinity
      System exit 0
    }

    def proceedAnyway: Unit = {
      // We must disable Tor check because disconnect later will bring us here again
      WalletApp.app.prefs.edit.putBoolean(WalletApp.ENSURE_TOR, false).commit
      next.makeAttempt
    }

    private def showIssue(msgRes: Int, btnText: String, whenTapped: => Unit) = UITask {
      skipOrbotCheck setOnClickListener onButtonTap(proceedAnyway)
      takeOrbotAction setOnClickListener onButtonTap(whenTapped)
      mainOrbotIssues setVisibility View.VISIBLE
      mainOrbotCheck setVisibility View.GONE
      mainOrbotMessage setText msgRes
      takeOrbotAction setText btnText
      timer.cancel
    }

    def makeAttempt: Unit = {
      orbotHelper.addStatusCallback(initCallback)
      try timer.schedule(UITask(initCallback.onStatusTimeout), 2000) catch none
      try timer.schedule(UITask(mainOrbotCheck setVisibility View.VISIBLE), 2000) catch none
      if (!orbotHelper.init) initCallback.onNotYetInstalled
    }
  }
}
