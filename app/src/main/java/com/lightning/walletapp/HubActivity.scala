package com.lightning.walletapp

import java.util.Timer
import java.util.concurrent.atomic.AtomicLong

import fr.acinq.eclair._
import android.database.ContentObserver
import immortan.crypto.Tools._
import com.lightning.walletapp.R.string._
import immortan.{ChannelMaster, LNParams, RemoteNodeInfo}
import android.widget.{LinearLayout, ListView, RelativeLayout, TextView}
import immortan.utils.{BitcoinUri, FiatRates, FiatRatesInfo, FiatRatesListener, InputParser, LNUrl, PaymentRequestExt, WalletEventsCatcher, WalletEventsListener}
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.mmin18.widget.RealtimeBlurView
import org.ndeftools.Message
import android.os.{Bundle, Handler}
import android.view.View
import androidx.recyclerview.widget.RecyclerView
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import immortan.sqlite.{PaymentTable, RelayTable, TxTable}
import rx.lang.scala.schedulers.{ComputationScheduler, IOScheduler}
import rx.lang.scala.{Observable, Subject, Subscriber, Subscription}



class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver { me =>
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val offlineIndicator = findViewById(R.id.offlineIndicator).asInstanceOf[TextView]

  private[this] lazy val topInfoLayout = findViewById(R.id.topInfoLayout).asInstanceOf[LinearLayout]
  private[this] lazy val topBlurringArea = findViewById(R.id.topBlurringArea).asInstanceOf[RealtimeBlurView]

  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]

  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[RecyclerView]
  private[this] lazy val totalBalance = findViewById(R.id.totalBalance).asInstanceOf[TextView]
  private[this] lazy val totalFiatBalance = findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
  private[this] lazy val fiatUnitPriceAndChange = findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
  private val CHOICE_RECEIVE_TAG = "choiceReceiveTag"

  // LISTENERS

  private val paymentObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(askedFromSelf: Boolean): Unit = none
  }

  private val relayObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(askedFromSelf: Boolean): Unit = none
  }

  private val txObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(askedFromSelf: Boolean): Unit = none
  }

  private val netListener: Monitor.ConnectivityListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      offlineIndicator setVisibility BaseActivity.viewMap(!isConnected)
    }.run
  }

  private val chainListener: WalletEventsListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = UITask {
      updateTotalBalance
    }.run
  }

  private val fiatRatesListener: FiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask {
      updateTotalBalance
      updateFiatRates
    }.run
  }

  // NFC

  def readEmptyNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def readNonNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def onNfcStateChange(ok: Boolean): Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcStateEnabled: Unit = none

  def readNdefMessage(nfcMessage: Message): Unit =
    runInFutureProcessOnUI(InputParser recordValue ndefMessageString(nfcMessage),
      _ => WalletApp.app quickToast error_nothing_useful)(_ => me checkExternalData noneRunnable)

  // IMPLEMENTATIONS

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def onDestroy: Unit = {
    getContentResolver.unregisterContentObserver(paymentObserver)
    getContentResolver.unregisterContentObserver(relayObserver)
    getContentResolver.unregisterContentObserver(txObserver)

    LNParams.chainWallet.eventsCatcher ! WalletEventsCatcher.Remove(chainListener)
    FiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def checkExternalData(whenNone: Runnable): Unit =
    InputParser.checkAndMaybeErase {
      case _: RemoteNodeInfo =>
      case _: PaymentRequestExt =>
      case _: BitcoinUri =>
      case _: LNUrl =>
      case _ => whenNone.run
    }

  override def onChoiceMade(tag: String, pos: Int): Unit = {
//    val body = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null)
//    val rateManager = new RateManager(body, None, Map("usd" -> 57500D), "usd")
//    val alertBuilder = titleBodyAsViewBuilder(getString(dialog_receive_ln_title), rateManager.content)
//    mkCheckFormNeutral(_.dismiss, none, _.dismiss, alertBuilder, dialog_ok, dialog_cancel, dialog_split)
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      topInfoLayout post UITask(topBlurringArea setHeightTo topInfoLayout)
      bottomActionBar post UITask(bottomBlurringArea setHeightTo bottomActionBar)

      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(PaymentTable.table), true, paymentObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(RelayTable.table), true, relayObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(TxTable.table), true, txObserver)

      LNParams.chainWallet.eventsCatcher ! chainListener
      FiatRates.listeners += fiatRatesListener
      Tovuti.from(me).monitor(netListener)

      updateTotalBalance
      updateFiatRates
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringSettings(view: View): Unit = {
//    WalletApp.app.getContentResolver.notifyChange(WalletApp.app.sqlPath(PaymentTable.table), null)
    me goTo ClassNames.chainQrActivityClass
  }

  def bringSendFromClipboard(view: View): Unit = {
    def explain: Unit = snack(contentWindow, getString(error_nothing_in_clipboard).html, dialog_ok, _.dismiss)
    runInFutureProcessOnUI(InputParser.parse(WalletApp.app.getBufferUnsafe), _ => explain)(_ => me checkExternalData explain)
  }

  def bringScanner(view: View): Unit = {
//    ChannelMaster.stateUpdateStream.onNext(ChannelMaster.updateCounter.incrementAndGet)
    //callScanner(me)
  }

  def bringSearch(view: View): Unit = {

  }

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(res => getString(res).html)
    val list = makeChoiceList(options, android.R.layout.simple_expandable_list_item_1)
    val sheet = new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me)
    sheet.show(getSupportFragmentManager, CHOICE_RECEIVE_TAG)
  }

  // VIEW UPDATERS

  def updateFiatRates: Unit = {
    val change = LNParams.fiatRatesInfo.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
    val unitPriceAndChange = s"<small>$change</small>" + WalletApp.currentMsatInFiatHuman(100000000000L.msat)
    fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
  }

  def updateTotalBalance: Unit = {
    val chainBalanceMsat = WalletApp.lastChainBalance.toMilliSatoshi
    totalBalance.setText(LNParams.denomination.parsedWithSign(chainBalanceMsat, "#333333").html)
    totalFiatBalance.setText(WalletApp.currentMsatInFiatHuman(chainBalanceMsat).html)
  }
}
