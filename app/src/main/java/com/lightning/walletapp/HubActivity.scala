package com.lightning.walletapp

import immortan._
import immortan.utils._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.lightning.walletapp.Colors._
import com.lightning.walletapp.R.string._

import android.os.{Bundle, Handler}
import android.view.{View, ViewGroup}
import rx.lang.scala.{Subject, Subscription}
import fr.acinq.bitcoin.{ByteVector32, Crypto}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import immortan.sqlite.{PaymentTable, RelayTable, Table, TxTable}
import android.widget.{BaseAdapter, ImageView, LinearLayout, ListView, RelativeLayout, TextView}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import fr.acinq.eclair.payment.PaymentRequest.PaymentRequestFeatures
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import fr.acinq.eclair.payment.PaymentRequest
import androidx.transition.TransitionManager
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import fr.acinq.eclair.wire.PaymentTagTlv
import android.database.ContentObserver
import immortan.fsm.NCFunderOpenHandler
import org.ndeftools.Message


class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver { me =>
  def lnBalance: MilliSatoshi = LNParams.cm.all.values.filter(Channel.isOperationalOrWaiting).map(Channel.estimateBalance).sum
  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] lazy val walletCards = new WalletCardsViewHolder
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

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]

      getItem(position) match {
        case relayedInfo: RelayedPreimageInfo =>
          holder.detailsAndStatus setVisibility View.GONE
          holder.amount setText LNParams.denomination.directedWithSign(relayedInfo.earned, 0L.msat, cardZero, isPlus = true).html
          holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.meta setText WalletApp.app.when(relayedInfo.date).html
          holder setPaymentTypeVisibility R.id.lnRouted

        case txInfo: TxInfo =>
          holder.detailsAndStatus setVisibility View.VISIBLE
          holder.description setText txDescription(txInfo).html
          holder.statusIcon setImageResource txStatusIcon(txInfo)
          holder.amount setText LNParams.denomination.directedWithSign(txInfo.receivedSat.toMilliSatoshi, txInfo.sentSat.toMilliSatoshi, cardZero, txInfo.isIncoming).html
          holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.meta setText txMeta(txInfo).html
          setTypeIcon(holder, txInfo)

        case paymentInfo: PaymentInfo =>
          holder.cardContainer setOnClickListener onButtonTap {
            me goTo ClassNames.qrInvoiceActivityClass
            InputParser.value = paymentInfo.prExt
          }
      }

      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    def setPaymentTypeVisibility(visible: Int): Unit = for (id <- paymentTypeIconIds) typeMap(id) setVisibility BaseActivity.viewMap(id == visible)
    private val paymentTypeIconIds = List(R.id.btcIncoming, R.id.btcOutgoing, R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn, R.id.lnBtc)
    private val paymentTypeViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    private val typeMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeViews).toMap

    val cardContainer: LinearLayout = itemView.findViewById(R.id.cardContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = itemView.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = itemView.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]
    itemView.setTag(self)
  }

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]

    val totalBalance: TextView = view.findViewById(R.id.totalBalance).asInstanceOf[TextView]
    val totalFiatBalance: TextView = view.findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]

    val listCaption: RelativeLayout = view.findViewById(R.id.listCaption).asInstanceOf[RelativeLayout]
    val totalBitcoinBalance: TextView = view.findViewById(R.id.totalBitcoinBalance).asInstanceOf[TextView]
    val receiveBitcoinTip: ImageView = view.findViewById(R.id.receiveBitcoinTip).asInstanceOf[ImageView]
    val offlineIndicator: TextView = view.findViewById(R.id.offlineIndicator).asInstanceOf[TextView]
    val syncIndicator: TextView = view.findViewById(R.id.syncIndicator).asInstanceOf[TextView]

    val totalLightningBalance: TextView = view.findViewById(R.id.totalLightningBalance).asInstanceOf[TextView]
    val channelStateIndicators: LinearLayout = view.findViewById(R.id.channelStateIndicators).asInstanceOf[LinearLayout]
    val channelIndicator: ChannelIndicatorLine = view.findViewById(R.id.channelIndicator).asInstanceOf[ChannelIndicatorLine]

    val inFlightIncoming: TextView = view.findViewById(R.id.inFlightIncoming).asInstanceOf[TextView]
    val inFlightOutgoing: TextView = view.findViewById(R.id.inFlightOutgoing).asInstanceOf[TextView]
    val inFlightRouted: TextView = view.findViewById(R.id.inFlightRouted).asInstanceOf[TextView]
    val addChannelTip: ImageView = view.findViewById(R.id.addChannelTip).asInstanceOf[ImageView]

    val pendingRefunds: View = view.findViewById(R.id.pendingRefunds).asInstanceOf[View]
    val pendingRefundsSum: TextView = view.findViewById(R.id.pendingRefundsSum).asInstanceOf[TextView]

    def updateFiatRates: Unit = {
      val change = LNParams.fiatRatesInfo.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
    }

    def setCaptionVisibility: Unit = {
      val somePaymentsPresent = BaseActivity.viewMap(allInfos.nonEmpty)
      listCaption.setVisibility(somePaymentsPresent)
    }

    def updateView: Unit = lnBalance match { case currentLnBalance =>
      val walletBalance = currentLnBalance + WalletApp.lastChainBalance.totalBalance
      val states = LNParams.cm.all.values.map(_.state).take(8)

      TransitionManager.beginDelayedTransition(view)
      channelIndicator.createIndicators(states.toArray)
      channelStateIndicators setVisibility BaseActivity.viewMap(states.nonEmpty)
      totalFiatBalance setText WalletApp.currentMsatInFiatHuman(walletBalance).html
      totalBalance setText LNParams.denomination.parsedWithSign(walletBalance, totalZero).html
      syncIndicator setVisibility BaseActivity.viewMap(!WalletApp.lastChainBalance.isTooLongAgo)

      totalLightningBalance setVisibility BaseActivity.viewMap(states.nonEmpty)
      totalLightningBalance setText LNParams.denomination.parsedWithSign(currentLnBalance, lnCardZero).html
      totalBitcoinBalance setText LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, btcCardZero).html
      totalBitcoinBalance setVisibility BaseActivity.viewMap(WalletApp.lastChainBalance.totalBalance != 0L.msat)
      receiveBitcoinTip setVisibility BaseActivity.viewMap(WalletApp.lastChainBalance.totalBalance == 0L.msat)
      addChannelTip setVisibility BaseActivity.viewMap(states.isEmpty)

      val localInCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.FINAL_INCOMING }
      val localOutCount = LNParams.cm.opm.data.payments.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.LOCALLY_SENT }
      val trampolineCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED }
      val hideAll = localInCount + localOutCount + trampolineCount == 0

      inFlightIncoming setAlpha { if (hideAll) 0F else if (localInCount > 0) 1F else 0.3F }
      inFlightOutgoing setAlpha { if (hideAll) 0F else if (localOutCount > 0) 1F else 0.3F }
      inFlightRouted setAlpha { if (hideAll) 0F else if (trampolineCount > 0) 1F else 0.3F }

      inFlightIncoming setText localInCount.toString
      inFlightOutgoing setText localOutCount.toString
      inFlightRouted setText trampolineCount.toString

      val currentPublished = LNParams.cm.closingsPublished
      val currentRefunds = LNParams.cm.pendingRefundsAmount(currentPublished).toMilliSatoshi
      pendingRefundsSum setText LNParams.denomination.parsedWithSign(currentRefunds, cardZero).html
      pendingRefunds setVisibility BaseActivity.viewMap(currentPublished.nonEmpty)
    }
  }

  // LISTENERS

  private var streamSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]

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

  private val vibratorObserver: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(self: Boolean): Unit = Vibrator.vibrate
  }

  private val netListener: Monitor.ConnectivityListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      walletCards.offlineIndicator setVisibility BaseActivity.viewMap(!isConnected)
      // This will make channels SLEEPING right away instead of after no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
    }.run
  }

  private val chainListener: WalletEventsListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = UITask {
      walletCards.syncIndicator setVisibility View.GONE
      updatePendingChainTxStatus
      walletCards.updateView
    }.run
  }

  private val fiatRatesListener: FiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask {
      walletCards.updateFiatRates
      walletCards.updateView
    }.run
  }

  def updatePendingChainTxStatus: Unit = for {
    txInfo <- txInfos if !txInfo.isDeeplyBuried && !txInfo.isDoubleSpent
    (newDepth, newDoubleSpent) <- LNParams.chainWallet.wallet.doubleSpent(txInfo.tx)
    if newDepth != txInfo.depth || newDoubleSpent != txInfo.isDoubleSpent
  } WalletApp.txDataBag.updStatus(txInfo.txid, newDepth, newDoubleSpent)

  def setTypeIcon(holder: PaymentLineViewHolder, info: TxInfo): Unit = info.description match {
    case _: PlainTxDescription if info.isIncoming => holder.setPaymentTypeVisibility(visible = R.id.btcIncoming)
    case _: OpReturnTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcOutgoing)
    case _: ChanRefundingTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
    case _: ChanFundingTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcLn)
    case _: PlainTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcOutgoing)
    case _: HtlcClaimTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
    case _: PenaltyTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
  }

  def txDescription(info: TxInfo): String = info.description match {
    case _: ChanRefundingTxDescription => getString(tx_description_refunding)
    case _: HtlcClaimTxDescription => getString(tx_description_htlc_claim)
    case _: ChanFundingTxDescription => getString(tx_description_funding)
    case _: OpReturnTxDescription => getString(tx_description_op_return)
    case _: PenaltyTxDescription => getString(tx_description_penalty)
    case plainTxDescription: PlainTxDescription =>
      val shortOpt = plainTxDescription.addresses.headOption.map(_.shortAddress)
      val htmlOpt = shortOpt.map(short => s" <font color=$cardZero>$short</font>")
      getString(tx_btc) + htmlOpt.getOrElse(new String)
  }

  def txMeta(info: TxInfo): String =
    if (info.isDeeplyBuried) WalletApp.app.when(info.date)
    else if (info.isDoubleSpent) getString(tx_state_double_spent)
    else if (info.depth > 0) getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks)
    else getString(tx_state_unconfirmed)

  def txStatusIcon(info: TxInfo): Int =
    if (info.isDeeplyBuried) R.drawable.baseline_done_24
    else if (info.isDoubleSpent) R.drawable.baseline_block_24
    else R.drawable.baseline_hourglass_empty_24

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
    statusSubscription.foreach(_.unsubscribe)
    streamSubscription.foreach(_.unsubscribe)

    getContentResolver.unregisterContentObserver(vibratorObserver)
    getContentResolver.unregisterContentObserver(paymentObserver)
    getContentResolver.unregisterContentObserver(relayObserver)
    getContentResolver.unregisterContentObserver(txObserver)

    LNParams.chainWallet.eventsCatcher ! WalletEventsCatcher.Remove(chainListener)
    FiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case _: RemoteNodeInfo => me goTo ClassNames.remotePeerActivityClass
    case _: PaymentRequestExt =>
    case uri: BitcoinUri if uri.isValid =>
    case _: LNUrl =>
    case _ => whenNone.run
  }

  // Important: order of cases matters here
  override def onChoiceMade(tag: String, pos: Int): Unit = (tag, pos) match {
    case (CHOICE_RECEIVE_TAG, 1) if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) =>
      snack(contentWindow, getString(error_ln_receive_no_chans).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.all.values.forall(Channel.isWaiting) =>
      snack(contentWindow, getString(error_ln_receive_waiting).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.allSortedReceivable.isEmpty =>
      snack(contentWindow, getString(error_ln_receive_no_update).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.allSortedReceivable.last.commits.availableForReceive < MilliSatoshi(0L) =>
      val reserveHuman = LNParams.denomination.parsedWithSign(-LNParams.cm.allSortedReceivable.head.commits.availableForReceive, cardZero)
      snack(contentWindow, getString(error_ln_receive_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) =>
      val body = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null)
      val commits = LNParams.cm.maxReceivable(LNParams.cm.allSortedReceivable).head.commits
      val manager = new RateManager(body, getString(dialog_optional_ln_description).toSome, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      val canReceive = LNParams.denomination.parsedWithSign(commits.availableForReceive, Colors.cardZero)
      val canReceiveFiat = WalletApp.currentMsatInFiatHuman(commits.availableForReceive)

      def useMax(alert: AlertDialog): Unit = {
        val balanceSat = commits.availableForReceive.truncateToSatoshi
        manager.inputAmount.setText(balanceSat.toLong.toString)
      }

      def attempt(alert: AlertDialog): Unit = {
        val preimage: ByteVector32 = randomBytes32
        val hash: ByteVector32 = Crypto.sha256(preimage)
        val invoiceKey = LNParams.secret.keys.fakeInvoiceKey(hash)
        val description = PlainDescription(manager.extraInput.getText.toString)
        val hop = List(commits.updateOpt.map(_ extraHop commits.remoteInfo.nodeId).toList)
        val features = PaymentRequestFeatures(Features.VariableLengthOnion.optional, Features.PaymentSecret.optional, Features.BasicMultiPartPayment.optional).toSome
        val prExt = PaymentRequestExt from PaymentRequest(LNParams.chainHash, Some(manager.resultMsat), hash, invoiceKey, description.invoiceText, LNParams.incomingFinalCltvExpiry, hop, features)
        LNParams.cm.payBag.replaceIncomingPayment(prExt, preimage, description, lnBalance, LNParams.fiatRatesInfo.rates, NCFunderOpenHandler.typicalFee)
        me goTo ClassNames.qrInvoiceActivityClass
        InputParser.value = prExt
        alert.dismiss
      }

      val builder = titleBodyAsViewBuilder(title = getString(dialog_receive_ln).html, manager.content)
      val alert = mkCheckFormNeutral(attempt, none, useMax, builder, dialog_ok, dialog_cancel, dialog_max)
      manager.hintFiatDenom.setText(getString(dialog_can_receive).format(canReceiveFiat).html)
      manager.hintDenom.setText(getString(dialog_can_receive).format(canReceive).html)
      manager.updateOkButton(getPositiveButton(alert), isEnabled = false)

      manager.inputAmount addTextChangedListener onTextChange { _ =>
        val notTooLow: Boolean = LNParams.minPayment <= manager.resultMsat
        val notTooHigh: Boolean = commits.availableForReceive >= manager.resultMsat
        manager.updateOkButton(getPositiveButton(alert), notTooLow && notTooHigh)
      }

    case (CHOICE_RECEIVE_TAG, 0) =>
      me goTo ClassNames.qrChainActivityClass

    case _ =>
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)

      getContentResolver.registerContentObserver(Vibrator.uri, true, vibratorObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(PaymentTable.table), true, paymentObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(RelayTable.table), true, relayObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(TxTable.table), true, txObserver)

      LNParams.chainWallet.eventsCatcher ! chainListener
      FiatRates.listeners += fiatRatesListener
      Tovuti.from(me).monitor(netListener)

      // Throttle all types of burst updates, but make sure the last one is always called
      val statusEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, 1.second)
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, 1.second)

      val txEvents = Rx.uniqueFirstAndLastWithinWindow(txEventStream, 1.second).doOnNext(_ => reloadTxInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(paymentEventStream, 1.second).doOnNext(_ => reloadPaymentInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(relayEventStream, 1.second).doOnNext(_ => reloadRelayedPreimageInfos)
      val paymentRelatedEvents = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(stateEvents)
      statusSubscription = statusEvents.merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).toSome
      streamSubscription = paymentRelatedEvents.subscribe(_ => UITask(updatePaymentList).run).toSome

      WalletApp.txDataBag.db txWrap {
        reloadRelayedPreimageInfos
        reloadPaymentInfos
        reloadTxInfos
        updAllInfos
      }

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      walletCards.setCaptionVisibility
      walletCards.updateFiatRates
      walletCards.updateView
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringSettings(view: View): Unit = {
    ChannelMaster.notifyStateUpdated
  }

  def bringSearch(view: View): Unit = {

  }

  private def explainClipboardFailure = UITask {
    val message = getString(error_nothing_in_clipboard).html
    snack(contentWindow, message, dialog_ok, _.dismiss)
  }

  def bringSendFromClipboard(view: View): Unit =
    runInFutureProcessOnUI(InputParser.recordValue(WalletApp.app.getBufferUnsafe),
      _ => explainClipboardFailure.run)(_ => me checkExternalData explainClipboardFailure)

  def bringScanner(view: View): Unit = callScanner(me)

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(res => getString(res).html)
    val list = makeChoiceList(options, android.R.layout.simple_expandable_list_item_1)
    val sheet = new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me)
    sheet.show(getSupportFragmentManager, CHOICE_RECEIVE_TAG)
  }

  def goToReceiveBitcoinPage(view: View): Unit =
    me goTo ClassNames.qrChainActivityClass

  // VIEW UPDATERS

  def updatePaymentList: Unit = {
    paymentsAdapter.notifyDataSetChanged
    walletCards.setCaptionVisibility
  }
}
