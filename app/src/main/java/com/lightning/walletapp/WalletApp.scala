package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import android.content.{ClipboardManager, Context, Intent, SharedPreferences}
import android.app.{Application, NotificationChannel, NotificationManager}
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification}
import fr.acinq.bitcoin.{Block, ByteVector32}
import immortan.{LNParams, WalletExt}

import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import immortan.crypto.Tools.Fiat2Btc
import immortan.utils.BtcDenomination
import fr.acinq.eclair.MilliSatoshi
import immortan.crypto.Tools.runAnd
import androidx.multidex.MultiDex
import scala.concurrent.Future
import android.widget.Toast
import java.io.InputStream
import android.os.Build
import scala.util.Try


object WalletApp {
  var app: WalletApp = _
  var fiatCode: String = _
  var wallet: WalletExt = _

  // Should be automatically updated on receiving to current address, cached for performance
  var currentChainReceiveAddress: Future[String] = Future.failed(new RuntimeException)

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map { ratePerOneBtc => msat.toLong * ratePerOneBtc / BtcDenomination.factor }
  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi): String = msatInFiat(rates, code)(msat).map(amt => s"≈ ${formatFiat format amt} $code").getOrElse(s"≈ ? $code")
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(LNParams.fiatRatesInfo.rates, fiatCode, msat)

  object Vibrator {
    private var lastVibrated = 0L
    private val vibrator = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
    def canVibrate: Boolean = null != vibrator && vibrator.hasVibrator && lastVibrated < System.currentTimeMillis - 3000L

    def vibrate: Unit = if (canVibrate) {
      lastVibrated = System.currentTimeMillis
      vibrator.vibrate(Array(0L, 85, 200), -1)
    }
  }
}

class WalletApp extends Application { me =>
  lazy val foregroundServiceIntent = new Intent(me, AwaitService.awaitServiceClass)
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.2
  lazy val isTablet: Boolean = scrWidth > 3.5

  lazy val plur: (Array[String], Long) => String = getString(R.string.lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], _: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  override protected def attachBaseContext(base: Context): Unit = {
    super.attachBaseContext(base)
    MultiDex.install(me)
    WalletApp.app = me
  }

  override def onCreate: Unit = runAnd(super.onCreate) {
    // Currently night theme is the only option, should be set by default
    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES)

    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N_MR1) {
      val manager = this getSystemService classOf[NotificationManager]
      val chan1 = new NotificationChannel(AwaitService.CHANNEL_ID, "NC1", NotificationManager.IMPORTANCE_DEFAULT)
      val chan2 = new NotificationChannel(DelayedNotification.CHANNEL_ID, "NC2", NotificationManager.IMPORTANCE_DEFAULT)
      manager.createNotificationChannel(chan1)
      manager.createNotificationChannel(chan2)
    }
  }

  def showStickyPaymentNotification(titleRes: Int, amount: MilliSatoshi): Unit = {
    val bodyText = getString(incoming_notify_body).format(LNParams.denomination parsedWithSign amount)
    val withTitle = foregroundServiceIntent.putExtra(AwaitService.TITLE_TO_DISPLAY, me getString titleRes)
    val withBodyAction = withTitle.putExtra(AwaitService.BODY_TO_DISPLAY, bodyText).setAction(AwaitService.ACTION_SHOW)
    androidx.core.content.ContextCompat.startForegroundService(me, withBodyAction)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_SHORT).show
  def plur1OrZero(opts: Array[String], num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def englishWordList: Seq[String] = {
    val raw  = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(raw, "UTF-8").getLines.toList
  }

  def electrumCheckpoints(chainHash: ByteVector32): InputStream = chainHash match {
    case Block.LivenetGenesisBlock.hash => getAssets.open("checkpoints_mainnet.json")
    case Block.TestnetGenesisBlock.hash => getAssets.open("checkpoints_testnet.json")
    case _ => throw new RuntimeException
  }

  def electrumServers(chainHash: ByteVector32): InputStream = chainHash match {
    case Block.LivenetGenesisBlock.hash => getAssets.open("servers_mainnet.json")
    case Block.TestnetGenesisBlock.hash => getAssets.open("servers_testnet.json")
    case _ => throw new RuntimeException
  }
}
