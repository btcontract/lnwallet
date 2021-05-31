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
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair.channel.{CMD_CHECK_FEERATE, PersistentChannelData}
import android.app.{Application, NotificationChannel, NotificationManager}
import fr.acinq.eclair.blockchain.electrum.{CheckPoint, ElectrumClientPool}
import android.content.{ClipData, ClipboardManager, Context, Intent, SharedPreferences}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import com.lightning.walletapp.utils.{AwaitService, DelayedNotification, LocalBackup, UsedAddons, WebsocketBus}
import fr.acinq.eclair.blockchain.fee.FeeratesPerKw
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import android.icu.text.SimpleDateFormat
import fr.acinq.eclair.wire.TrampolineOn
import android.text.format.DateFormat
import androidx.multidex.MultiDex
import rx.lang.scala.Observable
import android.widget.Toast
import java.util.Date
import scala.util.Try


object WalletApp {
  var txDataBag: SQLiteTx = _
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

  val backupSaveWorker: ThrottledWork[String, Any] = new ThrottledWork[String, Any] {
    def process(cmd: String, result: Any): Unit = if (makeChanBackup && LocalBackup.isExternalStorageWritable) {
      try LocalBackup.encryptAndWritePlainBackup(app, dbFileNameEssential, LNParams.chainHash, LNParams.secret.seed)
      catch none
    }

    // File saving gets delayed in case of frequent rapid updates
    def work(cmd: String): Observable[Any] = Rx.ioQueue.delay(4.seconds)
    def error(canNotHappen: Throwable): Unit = none
  }

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
      txDataBag = new SQLiteTx(miscInterface)
      extDataBag = new SQLiteDataExtended(miscInterface)
      lastChainBalance = extDataBag.tryGetLastChainBalance getOrElse LastChainBalance(0L.sat, 0L.sat, 0L)
      usedAddons = extDataBag.tryGetAddons getOrElse UsedAddons(addons = List.empty)
    }
  }

  def makeOperational(secret: WalletSecret): Unit = {
    require(isAlive, "Application is not alive, hence can not become operational")
    val essentialInterface = new DBInterfaceSQLiteAndroidEssential(app, dbFileNameEssential)
    val graphInterface = new DBInterfaceSQLiteAndroidGraph(app, dbFileNameGraph)

    val normalBag = new SQLiteNetwork(graphInterface, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hostedBag = new SQLiteNetwork(graphInterface, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    val payBag = new SQLitePayment(extDataBag.db, essentialInterface)

    val chanBag = new SQLiteChannel(essentialInterface) {
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
      LNParams.fiatRatesInfo = extDataBag.tryGetFiatRatesInfo getOrElse FiatRatesInfo(rates = Map.empty, oldRates = Map.empty, stamp = 0L)
      LNParams.feeRatesInfo = extDataBag.tryGetFeeRatesInfo getOrElse FeeRatesInfo(FeeratesPerKw(FeeRatesHelpers.defaultFeerates), history = Nil, stamp = 0L)
      LNParams.trampoline = extDataBag.tryGetTrampolineOn getOrElse TrampolineOn.byDefault(LNParams.minPayment, LNParams.routingCltvExpiryDelta)
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
      def onFeeRates(newRates: FeeRatesInfo): Unit = {
        // We may get fresh feerates after channels become OPEN
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newRates)
      }
    }

    FiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRates: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRates)
    }

    LNParams.chainWallet.eventsCatcher ! new WalletEventsListener {
      override def onChainSynchronized(event: WalletReady): Unit = {
        // The main point of this is to use unix timestamp instead of chain tip stamp to define whether we are deeply in past
        lastChainBalance = LastChainBalance(event.confirmedBalance, event.unconfirmedBalance, System.currentTimeMillis)
        extDataBag.putLastChainBalance(lastChainBalance)

        // Sync is complete now, we can start channel connections
        // Invalidate last disconnect stamp since we're up again
        LNParams.lastDisconnect.set(Long.MaxValue)
        LNParams.blockCount.set(event.height)
        LNParams.cm.initConnect
      }

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def replaceChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          txDataBag.replaceTx(event.tx, event.depth, received, sent, event.feeOpt, description, isIncoming, lastChainBalance.totalBalance, LNParams.fiatRatesInfo.rates)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
        }

        val fee = event.feeOpt.getOrElse(0L.sat)
        val defSentTxDescription = TxDescription.define(LNParams.cm.all.values, Nil, event.tx)
        val sentTxDescription = txDescriptions.getOrElse(event.tx.txid, defSentTxDescription)

        if (event.sent == event.received + fee) replaceChainTx(event.received, event.sent - fee, sentTxDescription, isIncoming = 0L)
        else if (event.sent > event.received) replaceChainTx(received = 0L.sat, event.sent - event.received - fee, sentTxDescription, isIncoming = 0L)
        else replaceChainTx(event.received - event.sent, sent = 0L.sat, TxDescription.define(LNParams.cm.all.values, event.walletAddreses, event.tx), isIncoming = 1L)
      }

      override def onChainDisconnected: Unit = {
        // Remember to eventually stop accepting payments
        // note that there may be many these events in a row
        LNParams.lastDisconnect.set(System.currentTimeMillis)
      }
    }

    // Get up channels and payment FSMs
    LNParams.cm.all = Channel.load(listeners = Set(LNParams.cm), chanBag)
    // This inital update will create all in/routed/out FSMs
    LNParams.cm.stateUpdated(rejects = Nil)
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
    FiatRates.customFiatSymbols.get(code).map(sign => s"$sign$fiatAmount").getOrElse(s"$fiatAmount $code")
  }

  val currentMsatInFiatHuman: MilliSatoshi => String = msat =>
    msatInFiatHuman(LNParams.fiatRatesInfo.rates, fiatCode, msat)
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

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}
