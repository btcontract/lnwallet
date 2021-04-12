package com.lightning.walletapp

import immortan.sqlite._
import com.lightning.walletapp.sqlite._
import com.lightning.walletapp.R.string._
import fr.acinq.bitcoin.{Block, Crypto, SatoshiLong}
import fr.acinq.eclair.{CltvExpiryDelta, MilliSatoshi}
import immortan.crypto.Tools.{Bytes, Fiat2Btc, none, runAnd}
import android.app.{Application, NotificationChannel, NotificationManager}
import fr.acinq.eclair.blockchain.electrum.{CheckPoint, ElectrumClientPool}
import android.content.{ClipboardManager, Context, Intent, SharedPreferences}
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification, UsedAddons, WebsocketBus}
import immortan.utils.{BtcDenomination, FeeRates, FeeRatesInfo, FiatRates, FiatRatesInfo, SatDenomination}
import immortan.{Channel, ChannelMaster, CommsTower, LNParams, MnemonicExtStorageFormat, PathFinder, RemoteNodeInfo, SyncParams}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import fr.acinq.eclair.wire.TrampolineOn
import com.blockstream.libwally.Wally
import androidx.multidex.MultiDex
import scodec.bits.ByteVector
import android.widget.Toast
import android.os.Build
import scala.util.Try


object WalletApp { me =>
  var extDataBag: SQLiteDataExtended = _
  var lastWalletReady: WalletReady = _
  var usedAddons: UsedAddons = _
  var app: WalletApp = _

  final val dbFileNameMisc = "misc.db"
  final val dbFileNameGraph = "graph.db"
  final val dbFileNameEssential = "essential.db"

  final val USE_AUTH = "useAuth"
  final val FIAT_CODE = "fiatCode"
  final val ENSURE_TOR = "ensureTor"
  final val USE_SAT_DENOM = "useSatDenom"
  final val MAKE_CHAN_BACKUP = "makeChanBackup"
  final val CAP_LN_FEE_TO_CHAIN = "capLNFeeToChain"
  final val LAST_TOTAL_GOSSIP_SYNC = "lastTotalGossipSync"
  final val LAST_NORMAL_GOSSIP_SYNC = "lastNormalGossipSync"

  def useAuth: Boolean = app.prefs.getBoolean(USE_AUTH, false)
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)
  def useSatDenom: Boolean = app.prefs.getBoolean(USE_SAT_DENOM, true)
  def makeChanBackup: Boolean = app.prefs.getBoolean(MAKE_CHAN_BACKUP, true)
  def capLNFeeToChain: Boolean = app.prefs.getBoolean(CAP_LN_FEE_TO_CHAIN, false)

  // Due to Android specifics any of these may be nullified at runtime, must check for liveness on every entry
  def isAlive: Boolean = null != extDataBag && null != lastWalletReady && null != usedAddons && null != app

  def freePossiblyUsedResouces: Unit = {
    // Drop whatever network connections we still have
    WebsocketBus.workers.keys.foreach(WebsocketBus.forget)
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)

    // Clear listeners
    try LNParams.cm.becomeShutDown catch none
    try LNParams.chainWallet.becomeShutDown catch none
    try FiatRates.subscription.unsubscribe catch none
    try FeeRates.subscription.unsubscribe catch none

    extDataBag = null
    lastWalletReady = null
    usedAddons = null
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      extDataBag = new SQLiteDataExtended(miscInterface)
      usedAddons = extDataBag.tryGetAddons getOrElse UsedAddons(List.empty)
      val emptyReady = WalletReady(0L.sat, 0L.sat, 0L, System.currentTimeMillis)
      lastWalletReady = extDataBag.tryGetLastWalletReady getOrElse emptyReady
    }
  }

  def makeOperational(format: MnemonicExtStorageFormat): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePayment(extDataBag.db, essentialInterface)
    val chanBag = new SQLiteChannel(essentialInterface)

    LNParams.format = format
    LNParams.routerConf = RouterConf(maxCltvDelta = CltvExpiryDelta(2016), mppMinPartAmount = MilliSatoshi(10000000L), routeHopDistance = 6)
    LNParams.denomination = if (useSatDenom) SatDenomination else BtcDenomination
    LNParams.syncParams = new SyncParams

    extDataBag.db txWrap {
      LNParams.fiatRatesInfo = extDataBag.tryGetFiatRatesInfo getOrElse FiatRatesInfo(Map.empty, Map.empty, stamp = 0L)
      LNParams.feeRatesInfo = extDataBag.tryGetFeeRatesInfo getOrElse FeeRatesInfo(FeeRates.defaultFeerates, stamp = 0L)
      LNParams.trampoline = extDataBag.tryGetTrampolineOn getOrElse TrampolineOn.default(LNParams.minPayment, LNParams.routingCltvExpiryDelta)
    }

    val pf = new PathFinder(normalBag, hostedBag) {
      override def getLastTotalResyncStamp: Long = app.prefs.getLong(LAST_TOTAL_GOSSIP_SYNC, System.currentTimeMillis) // TODO: set to 0L
      override def getLastNormalResyncStamp: Long = app.prefs.getLong(LAST_NORMAL_GOSSIP_SYNC, System.currentTimeMillis) // TODO: set to 0L
      override def updateLastTotalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_TOTAL_GOSSIP_SYNC, stamp).commit
      override def updateLastNormalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_NORMAL_GOSSIP_SYNC, stamp).commit
      override def getExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
      override def getPHCExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.allHosted.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
    }

    ElectrumClientPool.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json", sslEnabled = true)
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json", sslEnabled = true)
      case Block.RegtestGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_regtest.json", sslEnabled = true)
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case Block.RegtestGenesisBlock.hash => Vector.empty
      case _ => throw new RuntimeException
    }

    LNParams.cm = new ChannelMaster(payBag, chanBag, extDataBag, pf)
    LNParams.chainWallet = LNParams.createWallet(extDataBag, format.seed)

    // Take care of essential listeners
    LNParams.chainWallet.eventsCatcher ! LNParams.cm.chainChannelListener
    FeeRates.listeners += LNParams.cm.feeRatesListener
    pf.listeners += LNParams.cm.opm

    // Get up channels and payment FSMs
    LNParams.cm.all = Channel.load(Set(LNParams.cm), LNParams.cm.chanBag)
    // This is a critically important update which will create routed and incoming FSMs if we still have pending payments in channels
    LNParams.cm.notifyFSMs(LNParams.cm.allInChannelOutgoing, LNParams.cm.allIncomingResolutions, Nil, makeMissingOutgoingFSM = true)
  }

  def syncAddonUpdate(fun: UsedAddons => UsedAddons): Unit = me synchronized {
    // Prevent races whenever multiple addons try to update data concurrently
    val usedAddons1: UsedAddons = fun(usedAddons)
    extDataBag.putAddons(usedAddons1)
    usedAddons = usedAddons1
  }

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)

  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] =
    currentRate(rates, code).map(perBtc => msat.toLong * perBtc / BtcDenomination.factor)

  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi): String = {
    val fiatAmount = msatInFiat(rates, code)(msat).map(formatFiat.format).getOrElse("?")
    FiatRates.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
  }

  val currentMsatInFiatHuman: MilliSatoshi => String = msat =>
    msatInFiatHuman(LNParams.fiatRatesInfo.rates, fiatCode, msat)

  // Other utils

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
    val withTitle = foregroundServiceIntent.putExtra(AwaitService.TITLE_TO_DISPLAY, me getString titleRes)
    val bodyText = getString(incoming_notify_body).format(LNParams.denomination.asString(amount) + "\u00A0" + LNParams.denomination.sign)
    val withBodyAction = withTitle.putExtra(AwaitService.BODY_TO_DISPLAY, bodyText).setAction(AwaitService.ACTION_SHOW)
    androidx.core.content.ContextCompat.startForegroundService(me, withBodyAction)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def plur1OrZero(opts: Array[String], num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def englishWordList: Array[String] = {
    val raw = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(raw, "UTF-8").getLines.toArray
  }
}
