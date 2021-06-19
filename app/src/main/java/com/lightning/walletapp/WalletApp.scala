package com.lightning.walletapp

import immortan._
import immortan.utils._
import immortan.sqlite._
import fr.acinq.eclair._
import scala.concurrent.duration._
import com.lightning.walletapp.sqlite._
import com.lightning.walletapp.R.string._
import android.os.{Build, VibrationEffect}
import immortan.crypto.Tools.{Fiat2Btc, none, runAnd}
import android.net.{ConnectivityManager, NetworkCapabilities}
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair.channel.{CMD_CHECK_FEERATE, PersistentChannelData}
import android.app.{Application, NotificationChannel, NotificationManager}
import android.content.{ClipData, ClipboardManager, Context, Intent, SharedPreferences}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import fr.acinq.eclair.blockchain.electrum.{CheckPoint, ElectrumClientPool, ElectrumWallet84}
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification, LocalBackup, UsedAddons, WebsocketBus}
import fr.acinq.eclair.blockchain.CurrentBlockCount
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import android.icu.text.SimpleDateFormat
import android.text.format.DateFormat
import androidx.multidex.MultiDex
import rx.lang.scala.Observable
import android.widget.Toast
import java.util.Date
import scala.util.Try


object WalletApp {
  var txDataBag: SQLiteTx = _
  var payMarketBag: SQLitePayMarket = _
  var extDataBag: SQLiteDataExtended = _
  var lastChainBalance: LastChainBalance = _
  var usedAddons: UsedAddons = _
  var app: WalletApp = _

  // When sending a tx locally we know recipent address and user provided memo
  // store this info here to use it when chain wallet receives a sent tx
  var txDescriptions: Map[ByteVector32, TxDescription] = Map.empty

  final val dbFileNameMisc = "misc1.db" // TODO: put back
  final val dbFileNameGraph = "graph1.db" // TODO: put back
  final val dbFileNameEssential = "essential3.db" // TODO: put back

  final val chainWalletType = new ElectrumWallet84

  val backupSaveWorker: ThrottledWork[String, Any] = new ThrottledWork[String, Any] {
    private def attemptStore: Unit = LocalBackup.encryptAndWritePlainBackup(app, dbFileNameEssential, LNParams.chainHash, LNParams.secret.seed)
    def process(cmd: String, unitAfterDelay: Any): Unit = if (makeChanBackup) try attemptStore catch none
    def work(cmd: String): Observable[Any] = Rx.ioQueue.delay(4.seconds)
  }

  final val USE_AUTH = "useAuth"
  final val FIAT_CODE = "fiatCode"
  final val ENSURE_TOR = "ensureTor"
  final val USE_SAT_DENOM = "useSatDenom"
  final val MAKE_CHAN_BACKUP = "makeChanBackup"
  final val CAP_LN_FEE_TO_CHAIN = "capLNFeeToChain"
  final val LAST_TOTAL_GOSSIP_SYNC = "lastTotalGossipSync"
  final val LAST_NORMAL_GOSSIP_SYNC = "lastNormalGossipSync"
  final val SHOW_RATE_US = "showRateUs"

  def useAuth: Boolean = app.prefs.getBoolean(USE_AUTH, false)
  def fiatCode: String = app.prefs.getString(FIAT_CODE, "usd")
  def ensureTor: Boolean = app.prefs.getBoolean(ENSURE_TOR, false)
  def useSatDenom: Boolean = app.prefs.getBoolean(USE_SAT_DENOM, true)
  def makeChanBackup: Boolean = app.prefs.getBoolean(MAKE_CHAN_BACKUP, true)
  def capLNFeeToChain: Boolean = app.prefs.getBoolean(CAP_LN_FEE_TO_CHAIN, false)
  def showRateUs: Boolean = app.prefs.getBoolean(SHOW_RATE_US, true)

  def isAlive: Boolean =
    null != txDataBag && null != payMarketBag && null != extDataBag &&
      null != lastChainBalance && null != usedAddons && null != app

  def freePossiblyUsedResouces: Unit = {
    // Drop whatever network connections we still have
    WebsocketBus.workers.keys.foreach(WebsocketBus.forget)
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)
    // Clear listeners, destroy actors, finalize state machines
    try LNParams.chainWallet.becomeShutDown catch none
    try LNParams.fiatRates.becomeShutDown catch none
    try LNParams.feeRates.becomeShutDown catch none
    try LNParams.cm.becomeShutDown catch none
    // Make non-alive and non-operational
    LNParams.secret = null
    txDataBag = null
  }

  def restartApplication: Unit = if (isAlive) {
    // This may be called multiple times from different threads
    // execute once if app is alive, otherwise do nothing

    freePossiblyUsedResouces
    app.quickToast(orbot_err_disconnect)
    require(!LNParams.isOperational, "Still operational")
    val intent = new Intent(app, ClassNames.mainActivityClass)
    app.startActivity(Intent makeRestartActivityTask intent.getComponent)
  }

  def makeAlive: Unit = {
    // Make application minimally operational (so we can check for seed in db)
    val miscInterface = new DBInterfaceSQLiteAndroidMisc(app, dbFileNameMisc)

    miscInterface txWrap {
      txDataBag = new SQLiteTx(miscInterface)
      payMarketBag = new SQLitePayMarket(miscInterface)
      extDataBag = new SQLiteDataExtended(miscInterface)
      usedAddons = extDataBag.tryGetAddons getOrElse UsedAddons(addons = List.empty)
      lastChainBalance = extDataBag.tryGetLastChainBalance(chainWalletType.tag) getOrElse LastChainBalance(0L.sat, 0L.sat)
    }
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePayment(extDataBag.db, preimageDb = essentialInterface)

    val chanBag = new SQLiteChannel(essentialInterface, channelTxFeesDb = extDataBag.db) {
      override def put(data: PersistentChannelData): PersistentChannelData = {
        backupSaveWorker.replaceWork("RESTART-DELAYED-BACKUP-SAVING")
        super.put(data)
      }
    }

    LNParams.secret = secret
    LNParams.syncParams = new TestNetSyncParams
    LNParams.chainHash = Block.TestnetGenesisBlock.hash
    LNParams.routerConf = RouterConf(initRouteMaxLength = 6, CltvExpiryDelta(2016), maxParts = 10)
    LNParams.denomination = if (useSatDenom) SatDenomination else BtcDenomination
    LNParams.ourInit = LNParams.createInit

    extDataBag.db txWrap {
      LNParams.feeRates = new FeeRates(extDataBag)
      LNParams.fiatRates = new FiatRates(extDataBag)
      LNParams.trampoline = extDataBag.tryGetTrampolineOn getOrElse LNParams.defaultTrampolineOn
    }

    val pf = new PathFinder(normalBag, hostedBag) {
      override def getLastTotalResyncStamp: Long = app.prefs.getLong(LAST_TOTAL_GOSSIP_SYNC, 0L)
      override def getLastNormalResyncStamp: Long = app.prefs.getLong(LAST_NORMAL_GOSSIP_SYNC, 0L)
      override def updateLastTotalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_TOTAL_GOSSIP_SYNC, stamp).commit
      override def updateLastNormalResyncStamp(stamp: Long): Unit = app.prefs.edit.putLong(LAST_NORMAL_GOSSIP_SYNC, stamp).commit
      override def getExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
      override def getPHCExtraNodes: Set[RemoteNodeInfo] = LNParams.cm.allHosted.flatMap(Channel.chanAndCommitsOpt).map(_.commits.remoteInfo).toSet
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

    LNParams.cm = new ChannelMaster(payBag, chanBag, extDataBag, pf) {
      // There will be a disconnect if VPN (Orbot) suddenly stops working
      // we halt all connections and go to main activity if Tor is required
      // in turn main activity won't proceed further if Tor check fails there
      override def onDisconnect(worker: CommsTower.Worker): Unit = {
        if (ensureTor && !app.isVPNOn) restartApplication
        else super.onDisconnect(worker)
      }
    }

    // Initialize chain wallet after ChannelMaster since it starts automatically
    LNParams.chainWallet = LNParams.createWallet(extDataBag, secret.seed, chainWalletType)

    // Take care of essential listeners of all kinds
    // Listeners added here will be called first
    pf.listeners += LNParams.cm.opm

    LNParams.feeRates.listeners += new FeeRatesListener {
      def onFeeRates(newRates: FeeRatesInfo): Unit = {
        // We may get fresh feerates after channels become OPEN
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newRates)
      }
    }

    LNParams.fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRates: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRates)
    }

    LNParams.chainWallet.eventsCatcher ! new WalletEventsListener {
      override def onChainTipKnown(event: CurrentBlockCount): Unit =
        LNParams.cm.initConnect

      override def onChainSynchronized(event: WalletReady): Unit = {
        // The main point of this is to use unix timestamp instead of chain tip stamp to define whether we are deeply in past
        lastChainBalance = LastChainBalance(event.confirmedBalance, event.unconfirmedBalance, System.currentTimeMillis)
        extDataBag.putLastChainBalance(lastChainBalance, chainWalletType.tag)
      }

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def replaceChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          txDataBag.replaceTx(event.tx, event.depth, received, sent, event.feeOpt, description, isIncoming, lastChainBalance.totalBalance, LNParams.fiatRates.info.rates)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
        }

        val fee = event.feeOpt.getOrElse(0L.sat)
        val defSentTxDescription = TxDescription.define(LNParams.cm.all.values, Nil, event.tx)
        val sentTxDescription = txDescriptions.getOrElse(event.tx.txid, defSentTxDescription)

        if (event.sent == event.received + fee) replaceChainTx(event.received, event.sent - fee, sentTxDescription, isIncoming = 0L)
        else if (event.sent > event.received) replaceChainTx(received = 0L.sat, event.sent - event.received - fee, sentTxDescription, isIncoming = 0L)
        else replaceChainTx(event.received - event.sent, sent = 0L.sat, TxDescription.define(LNParams.cm.all.values, event.walletAddreses, event.tx), isIncoming = 1L)
      }
    }

    // Get up channels and payment FSMs
    LNParams.cm.all = Channel.load(listeners = Set(LNParams.cm), chanBag)
    // This inital notification will create all in/routed/out FSMs
    LNParams.cm.notifyResolvers
  }

  def syncAddonUpdate(fun: UsedAddons => UsedAddons): Unit = synchronized {
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
    val fiatAmount = msatInFiat(rates, code)(msat).map(formatFiat.format).getOrElse(default = "?")
    LNParams.fiatRates.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
  }

  val currentMsatInFiatHuman: MilliSatoshi => String = msat =>
    msatInFiatHuman(LNParams.fiatRates.info.rates, fiatCode, msat)
}

object Vibrator {
  private val waveForm = VibrationEffect.createWaveform(Array(0L, 85, 200), -1)
  private val vibrator = WalletApp.app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  def vibrate: Unit = if (null != vibrator && vibrator.hasVibrator) vibrator.vibrate(waveForm)
}

class WalletApp extends Application { me =>
  lazy val foregroundServiceIntent = new Intent(me, AwaitService.awaitServiceClass)
  lazy val prefs: SharedPreferences = getSharedPreferences("prefs", Context.MODE_PRIVATE)

  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.2
  lazy val isTablet: Boolean = scrWidth > 3.5

  lazy val timeFormat: SimpleDateFormat = {
    val is24hour = DateFormat.is24HourFormat(me)
    val format = if (is24hour) "d MMM yyyy" else "MMM dd, yyyy"
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
  def plurOrZero(opts: Array[String] = Array.empty)(num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def isVPNOn: Boolean = Try {
    val manager = getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]
    manager.getAllNetworks.exists(manager getNetworkCapabilities _ hasTransport NetworkCapabilities.TRANSPORT_VPN)
  } getOrElse false

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}
