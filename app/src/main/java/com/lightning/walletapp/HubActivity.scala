package com.lightning.walletapp

import immortan._
import immortan.utils._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.lightning.walletapp.R.string._

import android.os.{Bundle, Handler}
import android.view.{View, ViewGroup}
import rx.lang.scala.{Subject, Subscription}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import android.widget.{LinearLayout, RelativeLayout, TextView}
import immortan.sqlite.{PaymentTable, RelayTable, Table, TxTable}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import android.database.ContentObserver
import org.ndeftools.Message


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

  // PAYMENT LIST

  private var txInfos = Iterable.empty[TxInfo]
  private var paymentInfos = Iterable.empty[PaymentInfo]
  private var relayedPreimageInfos = Iterable.empty[RelayedPreimageInfo]
  private var allInfos = List.empty[TransactionDetails]

  def reloadTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(Table.DEFAULT_LIMIT.get).map(WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos = LNParams.cm.payBag.listRecentPayments(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos = LNParams.cm.payBag.listRecentRelays(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toRelayedPreimageInfo)
  def updAllInfos: Unit = allInfos = (paymentInfos ++ relayedPreimageInfos ++ txInfos).toList.sortBy(_.seenAt)(Ordering[Long].reverse)

  val adapter: RecyclerView.Adapter[PaymentLineViewHolder] = new RecyclerView.Adapter[PaymentLineViewHolder] {
    override def onBindViewHolder(holder: PaymentLineViewHolder, pos: Int): Unit = allInfos(pos)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = allInfos.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): PaymentLineViewHolder = {
      val paymentLineContainer = getLayoutInflater.inflate(R.layout.frag_payment_line, parent, false)
      new PaymentLineViewHolder(paymentLineContainer)
    }
  }

  val paymentTypeIconIds = List(R.id.btcIncoming, R.id.btcOutgoing, R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn, R.id.lnBtc)
  def setPaymentTypeVis(views: Map[Int, View], visible: Int): Unit = for (id <- paymentTypeIconIds) views(id) setVisibility BaseActivity.viewMap(id == visible)

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val cardContainer: LinearLayout = itemView.findViewById(R.id.cardContainer).asInstanceOf[LinearLayout]
    val contentContainer: LinearLayout = itemView.findViewById(R.id.contentContainer).asInstanceOf[LinearLayout]
    val typeAndStatus: TextView = itemView.findViewById(R.id.typeAndStatus).asInstanceOf[TextView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]

    val paymentTypeViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    val typeMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeViews).toMap
  }

  // LISTENERS

  private var streamSubscription = Option.empty[Subscription]
  private val paymentEventStream = Subject[Long]
  private val relayEventStream = Subject[Long]
  private val txEventStream = Subject[Long]

  private val paymentObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(self: Boolean): Unit = paymentEventStream.onNext(ChannelMaster.updateCounter.incrementAndGet)
  }

  private val relayObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(self: Boolean): Unit = relayEventStream.onNext(ChannelMaster.updateCounter.incrementAndGet)
  }

  private val txObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(self: Boolean): Unit = txEventStream.onNext(ChannelMaster.updateCounter.incrementAndGet)
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
    streamSubscription.foreach(_.unsubscribe)

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

      WalletApp.txDataBag.db txWrap {
        reloadRelayedPreimageInfos
        reloadPaymentInfos
        reloadTxInfos
        updAllInfos
      }

      itemsList.setHasFixedSize(true)
      itemsList.setAdapter(adapter)

      updateTotalBalance
      updateFiatRates

      // Throttle all types of burst updates, but make sure the last one is always called
      val chanEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, 1.second)
      val txEvents = Rx.uniqueFirstAndLastWithinWindow(txEventStream, 1.second).doOnNext(_ => reloadTxInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(paymentEventStream, 1.second).doOnNext(_ => reloadPaymentInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(relayEventStream, 1.second).doOnNext(_ => reloadRelayedPreimageInfos)
      val allEvents = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(chanEvents)
      streamSubscription = allEvents.subscribe(_ => UITask(adapter.notifyDataSetChanged).run).toSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringSettings(view: View): Unit = {

  }

  def bringSearch(view: View): Unit = {

  }

  def bringSendFromClipboard(view: View): Unit = {
    def explain: Unit = snack(contentWindow, getString(error_nothing_in_clipboard).html, dialog_ok, _.dismiss)
    runInFutureProcessOnUI(InputParser.parse(WalletApp.app.getBufferUnsafe), _ => explain)(_ => me checkExternalData explain)
  }

  def bringScanner(view: View): Unit = callScanner(me)

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
