package com.btcontract.wallet

import immortan._
import immortan.utils._
import immortan.sqlite._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.btcontract.wallet.sqlite._
import com.btcontract.wallet.R.string._
import fr.acinq.eclair.blockchain.electrum._

import android.widget.{EditText, Toast}
import android.os.{Build, VibrationEffect}
import android.net.{ConnectivityManager, NetworkCapabilities}
import fr.acinq.bitcoin.{Block, ByteVector32, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.{CurrentBlockCount, EclairWallet}
import fr.acinq.eclair.channel.{CMD_CHECK_FEERATE, PersistentChannelData}
import android.app.{Application, NotificationChannel, NotificationManager}
import android.content.{ClipData, ClipboardManager, Context, Intent, SharedPreferences}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.{TransactionReceived, WalletReady}
import com.btcontract.wallet.utils.{AwaitService, DelayedNotification, LocalBackup, WebsocketBus}
import fr.acinq.eclair.blockchain.electrum.db.{CompleteChainWalletInfo, SigningWallet, WatchingWallet}
import android.view.inputmethod.InputMethodManager
import fr.acinq.eclair.router.Router.RouterConf
import androidx.appcompat.app.AppCompatDelegate
import immortan.utils.Denomination.formatFiat
import android.icu.text.SimpleDateFormat
import android.text.format.DateFormat
import androidx.multidex.MultiDex
import rx.lang.scala.Observable
import scodec.bits.ByteVector
import akka.actor.Props
import java.util.Date
import scala.util.Try


object WalletApp {
  var txDataBag: SQLiteTx = _
  var payMarketBag: SQLitePayMarket = _
  var extDataBag: SQLiteDataExtended = _
  var app: WalletApp = _

  // When sending a tx locally we know recipent address and user provided memo
  // store this info here to use it when chain wallet receives a sent tx
  var txDescriptions: Map[ByteVector32, TxDescription] = Map.empty

  final val dbFileNameMisc = "misc.db"
  final val dbFileNameGraph = "graph.db"
  final val dbFileNameEssential = "essential.db"

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

  def isAlive: Boolean = null != txDataBag && null != payMarketBag && null != extDataBag && null != app

  def freePossiblyUsedResouces: Unit = {
    // Drop whatever network connections we still have
    WebsocketBus.workers.keys.foreach(WebsocketBus.forget)
    CommsTower.workers.values.map(_.pair).foreach(CommsTower.forget)
    // Clear listeners, destroy actors, finalize state machines
    try LNParams.chainWallets.becomeShutDown catch none
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
        backupSaveWorker.replaceWork("LN-TRIGGERED-DELAYED-BACKUP-SAVING")
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

    // Initialize chain wallet
    import LNParams.{ec, timeout, system}
    val walletBag = new SQLiteChainWallet(essentialInterface) {
      override def addChainWallet(info: CompleteChainWalletInfo): Unit = {
        backupSaveWorker.replaceWork("CHAIN-TRIGGERED-DELAYED-BACKUP-SAVING")
        super.addChainWallet(info)
      }
    }

    val params = WalletParameters(extDataBag, walletBag, LNParams.minDustLimit, swipeRange = 10, allowSpendUnconfirmed = true)
    val pool = system.actorOf(Props apply new ElectrumClientPool(LNParams.blockCount, LNParams.chainHash), "connection-pool")
    val sync = system.actorOf(Props apply new ElectrumChainSync(pool, params.headerDb, LNParams.chainHash), "chain-sync")
    val watcher = system.actorOf(Props apply new ElectrumWatcher(LNParams.blockCount, pool), "channel-tx-watcher")
    val catcher = system.actorOf(Props(new WalletEventsCatcher), "events-catcher")
    val storedWallets = walletBag.listWallets

    val readyWallets = {
      if (storedWallets.isEmpty) {
        val signingWallet = SigningWallet(EclairWallet.BIP84, isRemovable = false)
        val ewt = ElectrumWalletType.makeSigningType(EclairWallet.BIP84, secret.keys.master, LNParams.chainHash)
        val wallet = system.actorOf(Props apply new ElectrumWallet(pool, sync, params, ewt), s"signing-chain-wallet-single")
        val infoEmptyData = CompleteChainWalletInfo(signingWallet, ewt.xPub.publicKey, params.emptyPersistentDataBytes, 0L.sat, ewt.tag)
        val electrumWallet = ElectrumEclairWallet(wallet, ewt, infoEmptyData)
        walletBag.addChainWallet(infoEmptyData)
        wallet ! infoEmptyData.data
        List(electrumWallet)
      } else {
        storedWallets.zipWithIndex.toList map {
          case CompleteChainWalletInfo(core: SigningWallet, pub, persistent, lastBalance, label) ~ idx =>
            val ewt = ElectrumWalletType.makeSigningType(tag = core.walletType, secret.keys.master, LNParams.chainHash)
            val wallet = system.actorOf(Props apply new ElectrumWallet(pool, sync, params, ewt), s"signing-wallet-$idx")
            val infoNoPersistent = CompleteChainWalletInfo(core, pub, ByteVector.empty, lastBalance, label)
            val result = ElectrumEclairWallet(wallet, ewt, infoNoPersistent)
            wallet ! persistent
            result

          case CompleteChainWalletInfo(core: WatchingWallet, pub, persistent, lastBalance, label) ~ idx =>
            val ewt = ElectrumWalletType.makeWatchingType(tag = core.walletType, core.xPub, LNParams.chainHash)
            val wallet = system.actorOf(Props apply new ElectrumWallet(pool, sync, params, ewt), s"watching-wallet-$idx")
            val infoNoPersistent = CompleteChainWalletInfo(core, pub, ByteVector.empty, lastBalance, label)
            val result = ElectrumEclairWallet(wallet, ewt, infoNoPersistent)
            wallet ! persistent
            result
        }
      }
    }

    // Chain wallets and sync have already been initialized, they will start interacting with network
    LNParams.chainWallets = LNParams.WalletExt(readyWallets, catcher, sync, pool, watcher, params.walletDb)
    // IMPORTANT: listeners added here must be called first
    pf.listeners += LNParams.cm.opm

    LNParams.feeRates.listeners += new FeeRatesListener {
      def onFeeRates(newRatesInfo: FeeRatesInfo): Unit = {
        // We may get fresh feerates after channels become OPEN
        LNParams.cm.all.values.foreach(_ process CMD_CHECK_FEERATE)
        extDataBag.putFeeRatesInfo(newRatesInfo)
      }
    }

    LNParams.fiatRates.listeners += new FiatRatesListener {
      def onFiatRates(newRatesInfo: FiatRatesInfo): Unit =
        extDataBag.putFiatRatesInfo(newRatesInfo)
    }

    LNParams.chainWallets.catcher ! new WalletEventsListener {
      override def onChainTipKnown(event: CurrentBlockCount): Unit = LNParams.cm.initConnect

      override def onChainSynchronized(event: WalletReady): Unit = LNParams.chainWallets = LNParams.chainWallets.lastBalanceUpdated(event)

      override def onTransactionReceived(event: TransactionReceived): Unit = {
        def addChainTx(received: Satoshi, sent: Satoshi, description: TxDescription, isIncoming: Long): Unit = txDataBag.db txWrap {
          val lastWalletBalance = LNParams.chainWallets.wallets.find(event.sameXpub).map(_.info.lastBalance).getOrElse(0L.sat).toMilliSatoshi
          txDataBag.addTx(event.tx, event.depth, received, sent, event.feeOpt, event.xPub, description, isIncoming, lastWalletBalance, LNParams.fiatRates.info.rates)
          txDataBag.addSearchableTransaction(description.queryText(event.tx.txid), event.tx.txid)
        }

        val fee = event.feeOpt.getOrElse(0L.sat)
        val defSentTxDescription = TxDescription.define(LNParams.cm.all.values, Nil, event.tx)
        val sentTxDescription = txDescriptions.getOrElse(event.tx.txid, defSentTxDescription)

        if (event.sent == event.received + fee) addChainTx(event.received, event.sent - fee, sentTxDescription, isIncoming = 0L)
        else if (event.sent > event.received) addChainTx(received = 0L.sat, event.sent - event.received - fee, sentTxDescription, isIncoming = 0L)
        else addChainTx(event.received - event.sent, sent = 0L.sat, TxDescription.define(LNParams.cm.all.values, event.walletAddreses, event.tx), isIncoming = 1L)
      }
    }

    // Get up channels and payment FSMs
    LNParams.cm.all = Channel.load(listeners = Set(LNParams.cm), chanBag)
    // This inital notification will create all in/routed/out FSMs
    LNParams.cm.notifyResolvers
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

  def showKeyboard(field: EditText): Unit = Try {
    val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    imm.showSoftInput(field, InputMethodManager.SHOW_IMPLICIT)
  }

  def hideKeyboard(field: EditText): Unit = Try {
    val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    imm.hideSoftInputFromWindow(field.getWindowToken, 0)
  }

  def copy(text: String): Unit = {
    val bufferContent = ClipData.newPlainText("wallet", text)
    clipboardManager.setPrimaryClip(bufferContent)
    quickToast(copied_to_clipboard)
  }
}
