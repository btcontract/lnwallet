package com.lightning.walletapp

import R.string._
import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import android.content.{Context, Intent}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import android.net.{ConnectivityManager, NetworkCapabilities}
import info.guardianproject.netcipher.proxy.{OrbotHelper, StatusCallback}
import org.ndeftools.util.activity.NfcReaderActivity
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.walletapp.helper.{AwaitService, FingerPrint}
import co.infinum.goldfinger.Goldfinger
import org.ndeftools.Message
import android.os.Bundle
import android.support.v4.content.ContextCompat
import android.view.View

import scala.util.Try


object MainActivity {
  var proceedOnSuccess: Runnable = _
  var actOnError: PartialFunction[Throwable, Unit] = _
  val wallet = classOf[WalletActivity]

  lazy val prepareKit = {
    val stream = new java.io.FileInputStream(app.walletFile)
    val proto = WalletProtobufSerializer parseToProto stream

    app.kit = new app.WalletKit {
      wallet = (new WalletProtobufSerializer).readWallet(app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)

      def startUp = try {
        setupAndStartDownload
        proceedOnSuccess.run
      } catch actOnError
    }
  }
}

class MainActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val takeOrbotAction = findViewById(R.id.takeOrbotAction).asInstanceOf[Button]
  lazy val mainOrbotMessage = findViewById(R.id.mainOrbotMessage).asInstanceOf[TextView]
  lazy val mainOrbotIssues = findViewById(R.id.mainOrbotIssues).asInstanceOf[View]
  lazy val mainOrbot = findViewById(R.id.mainOrbot).asInstanceOf[View]

  lazy val mainFingerprintImage = findViewById(R.id.mainFingerprintImage).asInstanceOf[ImageView]
  lazy val mainFingerprint = findViewById(R.id.mainFingerprint).asInstanceOf[View]
  lazy val mainChoice = findViewById(R.id.mainChoice).asInstanceOf[View]
  lazy val gf = new Goldfinger.Builder(me).build

  def INIT(state: Bundle) = {
    runAnd(me setContentView R.layout.activity_main)(me initNfc state)
    MainActivity.proceedOnSuccess = UITask { if (FingerPrint isOperational gf) proceedWithAuth else me exitTo MainActivity.wallet }
    MainActivity.actOnError = { case reThrowToEnterEmergencyActivity => UITask(throw reThrowToEnterEmergencyActivity).run }
    Utils clickableTextField findViewById(R.id.mainGreetings)
  }

  // NFC AND SHARE

  private[this] def readFail(readingError: Throwable) = runAnd(app quickToast err_nothing_useful)(next)
  def readNdefMessage(msg: Message) = <(app.TransData recordValue ndefMessageString(msg), readFail)(_ => next)

  override def onNoNfcIntentFound = {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(externalData => null != externalData)
    if (processIntent && getIntent.getAction == "android.intent.action.RECEIVE") runAnd(app.TransData.value = FragWallet.OPEN_RECEIVE_MENU)(next)
    else if (processIntent) <(dataOpt foreach app.TransData.recordValue, readFail)(_ => next)
    else next
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = me readFail null
  def readEmptyNdefMessage = me readFail null

  // STARTUP LOGIC

  def next = (app.walletFile.exists, app.isAlive) match {
    case (false, _) => mainChoice setVisibility View.VISIBLE
    case (true, true) => MainActivity.proceedOnSuccess.run

    case (true, false) =>
      MainActivity.prepareKit
      // First load wallet files, then setup, then rest
      LNParams setup app.kit.wallet.getKeyChainSeed.getSeedBytes
      val ensureTor = app.prefs.getBoolean(AbstractKit.ENSURE_TOR, false)
      if (ensureTor) ensureTorWorking else app.kit.startAsync
  }

  def ensureTorWorking = {
    val orbotHelper = OrbotHelper get app
    lazy val orbotCallback = new StatusCallback {
      def onEnabled(intent: Intent) = if (isVPNOn) app.kit.startAsync else onStatusTimeout
      def onStatusTimeout = showIssue(orbot_err_unclear, orbot_action_open, closeAppExitOrbot).run
      def onNotYetInstalled = showIssue(orbot_err_not_installed, orbot_action_install, closeAppInstallOrbot).run
      def onStopping = onStatusTimeout
      def onDisabled = none
      def onStarting = none
    }

    def closeAppExitOrbot = {
      val intent = getPackageManager getLaunchIntentForPackage OrbotHelper.ORBOT_PACKAGE_NAME
      Option(intent) foreach startActivity
      finishAffinity
      System exit 0
    }

    def closeAppInstallOrbot = {
      orbotHelper installOrbot me
      finishAffinity
      System exit 0
    }

    def showIssue(msgRes: Int, btnRes: Int, whenTapped: => Unit) = UITask {
      takeOrbotAction setOnClickListener onButtonTap(whenTapped)
      mainOrbotIssues setVisibility View.VISIBLE
      mainOrbot setVisibility View.GONE
      mainOrbotMessage setText msgRes
      takeOrbotAction setText btnRes
      timer.cancel
    }

    try timer.schedule(orbotCallback.onStatusTimeout, 20000) catch none
    try timer.schedule(mainOrbot setVisibility View.VISIBLE, 3000) catch none
    orbotHelper.addStatusCallback(orbotCallback).init
  }

  def proceedWithAuth =
    gf authenticate new Goldfinger.Callback {
      mainFingerprint setVisibility View.VISIBLE
      def onError(fpError: co.infinum.goldfinger.Error) = fpError match {
        case co.infinum.goldfinger.Error.LOCKOUT => mainFingerprintImage.setAlpha(0.25F)
        case co.infinum.goldfinger.Error.CANCELED => mainFingerprintImage.setAlpha(0.25F)
        case _ => app quickToast fpError.toString
      }

      def onSuccess(cipherText: String) = {
        mainFingerprint setVisibility View.GONE
        me exitTo MainActivity.wallet
      }
    }

  // MISC

  def isVPNOn = Try {
    val cm = getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]
    cm.getAllNetworks.exists(cm getNetworkCapabilities _ hasTransport NetworkCapabilities.TRANSPORT_VPN)
  } getOrElse false

  def skipOrbotCheck(view: View) = {
    mainOrbotIssues setVisibility View.GONE
    app.kit.startAsync
  }

  def exitCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
  def goRestoreWallet(view: View) = me exitTo classOf[WalletRestoreActivity]
}