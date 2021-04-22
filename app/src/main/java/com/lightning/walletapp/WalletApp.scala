package com.lightning.walletapp

import java.util.Date

import immortan._
import immortan.utils._
import immortan.sqlite._
import fr.acinq.eclair._
import com.lightning.walletapp.sqlite._
import com.lightning.walletapp.R.string._
import immortan.crypto.Tools.{Fiat2Btc, none, runAnd}
import fr.acinq.bitcoin.{Block, Satoshi, SatoshiLong}
import android.app.{Application, NotificationChannel, NotificationManager}
import fr.acinq.eclair.blockchain.electrum.{CheckPoint, ElectrumClientPool}
import android.content.{ClipData, ClipboardManager, Context, Intent, SharedPreferences}
import android.icu.text.SimpleDateFormat
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification, UsedAddons, WebsocketBus}
import fr.acinq.eclair.channel.CMD_CHECK_FEERATE
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import fr.acinq.eclair.wire.TrampolineOn
import androidx.multidex.MultiDex
import android.provider.Settings
import android.widget.Toast
import android.os.Build
import android.net.Uri
import android.text.format.DateFormat

import scala.util.Try


object WalletApp { me =>
  var txDataBag: SQLiteTxExtended = _
  var extDataBag: SQLiteDataExtended = _
  var lastChainBalance: Satoshi = _
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
  def isAlive: Boolean = null != extDataBag && null != txDataBag && null != lastChainBalance && null != usedAddons && null != app

  def freePossiblyUsedResouces: Unit = {
    // Drop whatever network connections we still have
    WebsocketBus.workers.keys.foreach(WebsocketBus.forget)
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)

    // Clear listeners
    try LNParams.cm.becomeShutDown catch none
    try LNParams.chainWallet.becomeShutDown catch none
    try FiatRates.becomeShutDown catch none
    try FeeRates.becomeShutDown catch none

    txDataBag = null
    extDataBag = null
    lastChainBalance = null
    usedAddons = null
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      extDataBag = new SQLiteDataExtended(miscInterface)
      txDataBag = new SQLiteTxExtended(app, miscInterface)
      lastChainBalance = extDataBag.tryGetLastBalance getOrElse 0L.sat
      usedAddons = extDataBag.tryGetAddons getOrElse UsedAddons(Nil)
      if (app.isTablet) Table.DEFAULT_LIMIT.set(10)
      else Table.DEFAULT_LIMIT.set(20)
    }
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePaymentExtended(app, extDataBag.db, essentialInterface)
    val chanBag = new SQLiteChannel(essentialInterface)

    LNParams.secret = secret
    LNParams.syncParams = new TestNetSyncParams
    LNParams.chainHash = Block.TestnetGenesisBlock.hash
    LNParams.routerConf = RouterConf(maxCltvDelta = CltvExpiryDelta(2016), mppMinPartAmount = MilliSatoshi(10000000L), routeHopDistance = 6)
    LNParams.denomination = if (useSatDenom) SatDenomination else BtcDenomination

    extDataBag.db txWrap {
      LNParams.fiatRatesInfo = extDataBag.tryGetFiatRatesInfo getOrElse FiatRatesInfo(Map.empty, Map.empty, stamp = 0L)
      LNParams.feeRatesInfo = extDataBag.tryGetFeeRatesInfo getOrElse FeeRatesInfo(FeeRates.defaultFeerates, stamp = 0L)
      LNParams.trampoline = extDataBag.tryGetTrampolineOn getOrElse TrampolineOn.default(LNParams.minPayment, LNParams.routingCltvExpiryDelta)
    }

    val pf = new PathFinder(normalBag, hostedBag) {
      override def getLastTotalResyncStamp: Long = app.prefs.getLong(LAST_TOTAL_GOSSIP_SYNC, 0L)
      override def getLastNormalResyncStamp: Long = app.prefs.getLong(LAST_NORMAL_GOSSIP_SYNC, 0L)
      override def updateLastTotalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_TOTAL_GOSSIP_SYNC, stamp).commit
      override def updateLastNormalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_NORMAL_GOSSIP_SYNC, stamp).commit
      override def getExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
      override def getPHCExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.allHosted.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
    }

    ElectrumClientPool.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_mainnet.json", sslEnabled = true)
      case Block.TestnetGenesisBlock.hash => ElectrumClientPool.readServerAddresses(app.getAssets open "servers_testnet.json", sslEnabled = true)
      case _ => throw new RuntimeException
    }

    CheckPoint.loadFromChainHash = {
      case Block.LivenetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_mainnet.json")
      case Block.TestnetGenesisBlock.hash => CheckPoint.load(app.getAssets open "checkpoints_testnet.json")
      case _ => throw new RuntimeException
    }

    LNParams.cm = new ChannelMaster(payBag, chanBag, extDataBag, pf)
    LNParams.chainWallet = LNParams.createWallet(extDataBag, secret.seed)

    // Take care of essential listeners of all kinds
    // Listeners added here will be called first
    pf.listeners += LNParams.cm.opm

    FeeRates.listeners += new FeeRatesListener {
      def onFeeRates(newFeeRates: FeeRatesInfo): Unit = {
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newFeeRates)
      }
    }

    FiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newFiatRates: FiatRatesInfo): Unit = {
        extDataBag.putFiatRatesInfo(newFiatRates)
      }
    }

    LNParams.chainWallet.eventsCatcher ! new WalletEventsListener {
      // CurrentBlockCount is handled separately is Channel.Receiver
      override def onChainSynchronized(event: WalletReady): Unit = {
        // Sync is complete now, we can start channel connections
        // Invalidate last disconnect stamp since we're up again
        LNParams.lastDisconnect.set(Long.MaxValue)
        LNParams.blockCount.set(event.height)
        LNParams.cm.initConnect

        if (event.totalBalance != lastChainBalance) {
          extDataBag.putLastBalance(event.totalBalance)
          lastChainBalance = event.totalBalance
        }
      }

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        // Vibrate to let user know this comes from Bitcoin network, so is real
        if (event.depth < 1) app.notify(Vibrator.uri)

        if (event.received >= event.sent) {
          val description = TxDescription.defineDescription(LNParams.cm.all.values, event.walletAddreses, event.tx)
          txDataBag.putTx(event, isIncoming = 1L, description, lastChainBalance.toMilliSatoshi, LNParams.fiatRatesInfo.rates)
        } else {
          val description = TxDescription.defineDescription(LNParams.cm.all.values, Nil, event.tx)
          // Outgoing tx should already be present in db so this will fail silently unless sent from other wallet
          txDataBag.putTx(event, 0L, description, lastChainBalance.toMilliSatoshi, LNParams.fiatRatesInfo.rates)
        }
      }

      override def onChainDisconnected: Unit = {
        // Remember to eventually stop accepting payments
        // note that there may be many these events in a row
        LNParams.lastDisconnect.set(System.currentTimeMillis)
      }
    }

    // Get up channels and payment FSMs
    LNParams.cm.all = Channel.load(listeners = Set(LNParams.cm), chanBag)
    // This is a critically important update which will create all in/routed/out FSMs if we still have pending payments in channels
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
}

object Vibrator {
  var lastVibrated: Long = 0L
  val uri: Uri = Settings.System.getUriFor(Settings.System.VIBRATE_ON)
  val vibrator: android.os.Vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def canVibrate: Boolean = null != vibrator && vibrator.hasVibrator && lastVibrated < System.currentTimeMillis - 3000L

  def vibrate: Unit = if (canVibrate) {
    lastVibrated = System.currentTimeMillis
    vibrator.vibrate(Array(0L, 85, 200), -1)
  }
}

class WalletApp extends Application { me =>
  lazy val foregroundServiceIntent = new Intent(me, AwaitService.awaitServiceClass)
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.2
  lazy val isTablet: Boolean = scrWidth > 3.5

  import android.provider.Settings.System.{getFloat, FONT_SCALE}
  lazy val bigFont: Boolean = getFloat(getContentResolver, FONT_SCALE, 1) > 1

  lazy val timeFormat: SimpleDateFormat = {
    val format = DateFormat.is24HourFormat(me) match {
      case false if scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
      case false if scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

      case false if scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
      case false if scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"
      case false => "MMM dd, yyyy' <small>'h:mma'</small>'"

      case true if scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
      case true if scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

      case true if scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
      case true if scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"
      case true => "d MMM yyyy' <small>'HH:mm'</small>'"
    }

    new SimpleDateFormat(format)
  }

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

  def when(thenDate: Date, now: Long = System.currentTimeMillis): String = thenDate.getTime match {
    case ago if now - ago < 129600000 => android.text.format.DateUtils.getRelativeTimeSpanString(ago, now, 0).toString
    case _ => timeFormat.format(thenDate)
  }

  def quickToast(code: Int): Unit = quickToast(me getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def plur1OrZero(opts: Array[String], num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }

  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def sqlPath(targetTable: String): Uri = Uri.parse(s"sqlite://com.lightning.walletapp/table/$targetTable")
  def sqlNotify(targetTable: String): Unit = getContentResolver.notifyChange(sqlPath(targetTable), null)
  def notify(uri: Uri): Unit = getContentResolver.notifyChange(uri, null)
}
