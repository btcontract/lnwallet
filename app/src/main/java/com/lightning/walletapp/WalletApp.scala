package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification, UsedAddons}
import android.content.{ClipboardManager, Context, Intent, SharedPreferences}
import android.app.{Application, NotificationChannel, NotificationManager}
import immortan.crypto.Tools.{Bytes, Fiat2Btc, runAnd}
import fr.acinq.bitcoin.{Block, ByteVector32, Crypto}
import immortan.{LNParams, WalletExt}

import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.lightning.walletapp.sqlite.SQLiteDataExtended
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import immortan.utils.BtcDenomination
import com.blockstream.libwally.Wally
import fr.acinq.eclair.MilliSatoshi
import androidx.multidex.MultiDex
import scala.concurrent.Future
import scodec.bits.ByteVector
import android.widget.Toast
import java.io.InputStream
import android.os.Build
import scala.util.Try


object WalletApp { me =>
  var extDataBag: SQLiteDataExtended = _
  var lastWalletReady: WalletReady = _
  var usedAddons: UsedAddons = _
  var app: WalletApp = _

  // Should be automatically updated on receiving to current address, cached for performance
  var currentChainReceiveAddress: Future[String] = Future.failed(new RuntimeException)

  final val USE_AUTH = "useAuth"
  final val FIAT_CODE = "fiatCode"
  final val ENSURE_TOR = "ensureTor"
  final val FEE_RATES_DATA = "feeRatesData"
  final val FIAT_RATES_DATA = "fiatRatesData"
  final val ROUTING_DESIRED = "routingDesired"

  def useAuth: Boolean = app.prefs.getBoolean(USE_AUTH, false)
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)
  def feeRatesData: String = app.prefs.getString(FEE_RATES_DATA, new String)
  def fiatRatesData: String = app.prefs.getString(FIAT_RATES_DATA, new String)
  def routingDesired: Boolean = app.prefs.getBoolean(ROUTING_DESIRED, false)

  def syncAddonUpdate(fun: UsedAddons => UsedAddons): Unit = me synchronized {
    // Prevent races whenever multiple addons try to update data concurrently
    val usedAddons1: UsedAddons = fun(usedAddons)
    extDataBag.putAddons(usedAddons1)
    usedAddons = usedAddons1
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code).map(ratePerOneBtc => msat.toLong * ratePerOneBtc / BtcDenomination.factor)
  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi): String = msatInFiat(rates, code)(msat).map(amt => s"≈ ${formatFiat format amt} $code").getOrElse(s"≈ ? $code")
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(LNParams.fiatRatesInfo.rates, fiatCode, msat)

  def scryptDerive(email: String, pass: String): Bytes = {
    // An intentionally expensive key-stretching method
    // N = 2^19, r = 8, p = 2

    val derived = new Array[Byte](64)
    val emailBytes = ByteVector.view(email.getBytes)
    val salt = Crypto.hash256(emailBytes).take(16).toArray
    Wally.scrypt(pass.trim.getBytes, salt, Math.pow(2, 19).toLong, 8, 2, derived)
    derived
  }

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

  override def attachBaseContext(base: Context): Unit = {
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

  def showStickyNotification(titleRes: Int, amount: MilliSatoshi): Unit = {
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
