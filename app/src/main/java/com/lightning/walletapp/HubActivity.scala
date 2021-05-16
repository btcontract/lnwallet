package com.lightning.walletapp

import immortan._
import immortan.utils._
import android.widget._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.lightning.walletapp.Colors._
import com.lightning.walletapp.R.string._

import android.os.{Bundle, Handler}
import android.view.{MenuItem, View, ViewGroup}
import rx.lang.scala.{Observable, Subject, Subscription}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import immortan.sqlite.{PaymentTable, RelayTable, Table, TxTable}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, FeeratePerVByte}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import fr.acinq.eclair.transactions.Transactions
import com.google.android.material.slider.Slider
import fr.acinq.eclair.payment.PaymentRequest
import androidx.transition.TransitionManager
import fr.acinq.eclair.blockchain.TxAndFee
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import fr.acinq.eclair.wire.PaymentTagTlv
import android.database.ContentObserver
import org.ndeftools.Message
import java.util.TimerTask


class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver { me =>
  def lnBalance: MilliSatoshi = LNParams.cm.all.values.filter(Channel.isOperationalOrWaiting).map(Channel.estimateBalance).sum

  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] lazy val walletCards = new WalletCardsViewHolder
  private val CHOICE_RECEIVE_TAG = "choiceReceiveTag"

  private[this] lazy val paymentTypeIconIds =
    List(R.id.btcIncoming, R.id.btcOutgoing, R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn, R.id.lnBtc,
      R.id.lnOutgoingBasic, R.id.lnOutgoingAction, R.id.btcOutgoingNormal, R.id.btcOutgoingToSelf)

  private[this] lazy val partsInFlight = getResources.getStringArray(R.array.parts_in_flight)
  private[this] lazy val pctCollected = getResources.getStringArray(R.array.pct_collected)
  private[this] lazy val lnSplitNotice = getString(tx_ln_notice_split)
  private[this] lazy val lnDefTitle = getString(tx_ln)

  // PAYMENT LIST

  private var txInfos = Iterable.empty[TxInfo]
  private var paymentInfos = Iterable.empty[PaymentInfo]
  private var relayedPreimageInfos = Iterable.empty[RelayedPreimageInfo]
  private var allInfos = List.empty[TransactionDetails]

  def reloadTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(Table.DEFAULT_LIMIT.get).map(WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos = LNParams.cm.payBag.listRecentPayments(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos = LNParams.cm.payBag.listRecentRelays(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toRelayedPreimageInfo)
  def updAllInfos: Unit = allInfos = (paymentInfos ++ relayedPreimageInfos ++ txInfos).toList.sortBy(_.seenAt)(Ordering[Long].reverse)

  def loadRecentInfos: Unit = WalletApp.txDataBag.db.txWrap {
    reloadRelayedPreimageInfos
    reloadPaymentInfos
    reloadTxInfos
    updAllInfos
  }

  def loadSearchInfos(query: String): Unit = WalletApp.txDataBag.db.txWrap {
    txInfos = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
    paymentInfos = LNParams.cm.payBag.searchPayments(query).map(LNParams.cm.payBag.toPaymentInfo)
    relayedPreimageInfos = Nil
    updAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    def work(query: String): Observable[Unit] = Rx.ioQueue.map(_ => if (query.nonEmpty) loadSearchInfos(query) else loadRecentInfos)
    def process(query: String, searchLoadResult: Unit): Unit = UITask(updatePaymentList).run
    def error(exc: Throwable): Unit = none
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]

      getItem(position) match {
        case info: RelayedPreimageInfo =>
          holder.detailsAndStatus setVisibility View.GONE
          holder.amount setText LNParams.denomination.directedWithSign(info.earned, 0L.msat, cardZero, isPlus = true).html
          if (LNParams.cm.inProcessors contains info.fullTag) holder.cardContainer setBackgroundResource R.drawable.panel_payment_active_bg
          else holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.meta setText WalletApp.app.when(info.date).html
          holder.setVisibleIcon(id = R.id.lnRouted)

        case info: TxInfo =>
          holder.detailsAndStatus setVisibility View.VISIBLE
          holder.description setText txDescription(info).html
          holder.amount setText LNParams.denomination.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardZero, info.isIncoming).html
          holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.statusIcon setImageResource txStatusIcon(info)
          holder.meta setText txMeta(info).html
          setTxTypeIcon(holder, info)

        case info: PaymentInfo =>
          holder.detailsAndStatus setVisibility View.VISIBLE
          holder.description setText paymentDescription(info).html
          holder.amount setText LNParams.denomination.directedWithSign(info.received, info.sent, cardZero, info.isIncoming).html
          holder.cardContainer setBackgroundResource paymentBackground(info)
          holder.statusIcon setImageResource paymentStatusIcon(info)
          holder.meta setText paymentMeta(info).html
          setPaymentTypeIcon(holder, info)
      }

      view
    }

    // Chain helpers

    private def txDescription(info: TxInfo): String = info.description match {
      case _: ChanRefundingTxDescription => getString(tx_description_refunding)
      case _: HtlcClaimTxDescription => getString(tx_description_htlc_claim)
      case _: ChanFundingTxDescription => getString(tx_description_funding)
      case _: OpReturnTxDescription => getString(tx_description_op_return)
      case _: PenaltyTxDescription => getString(tx_description_penalty)
      case plain: PlainTxDescription => labelOrFallback(plain)
    }

    private def setTxTypeIcon(holder: PaymentLineViewHolder, info: TxInfo): Unit = info.description match {
      case _: PlainTxDescription if info.isIncoming => holder.setVisibleIcon(id = R.id.btcIncoming)
      case _: OpReturnTxDescription => holder.setVisibleIcon(id = R.id.btcOutgoing)
      case _: ChanRefundingTxDescription => holder.setVisibleIcon(id = R.id.lnBtc)
      case _: ChanFundingTxDescription => holder.setVisibleIcon(id = R.id.btcLn)
      case _: HtlcClaimTxDescription => holder.setVisibleIcon(id = R.id.lnBtc)
      case _: PenaltyTxDescription => holder.setVisibleIcon(id = R.id.lnBtc)
      case _: PlainTxDescription =>
        // See WalletApp.WalletEventsListener.onTransactionReceived for explanation
        holder.iconMap(R.id.btcOutgoingToSelf) setVisibility BaseActivity.goneMap(info.sentSat == info.receivedSat)
        holder.iconMap(R.id.btcOutgoingNormal) setVisibility BaseActivity.goneMap(info.sentSat != info.receivedSat)
        holder.setVisibleIcon(id = R.id.btcOutgoing)
    }

    private def labelOrFallback(plainTxDescription: PlainTxDescription): String = plainTxDescription.label.getOrElse {
      val htmlOpt = plainTxDescription.addresses.headOption.map(_.shortAddress).map(short => s" <font color=$cardZero>$short</font>")
      getString(tx_btc) + htmlOpt.getOrElse(new String)
    }

    private def txMeta(info: TxInfo): String =
      if (info.isDeeplyBuried) WalletApp.app.when(info.date)
      else if (info.isDoubleSpent) getString(tx_state_double_spent)
      else if (info.depth > 0) getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks)
      else getString(tx_state_unconfirmed)

    private def txStatusIcon(info: TxInfo): Int =
      if (info.isDeeplyBuried) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24

    // LN helpers

    private def paymentDescription(info: PaymentInfo): String = info.description match {
      case PlainDescription(None, invoiceText) => Some(invoiceText).find(_.nonEmpty).getOrElse(lnDefTitle)
      case PlainMetaDescription(None, invoiceText, meta) => List(meta, invoiceText).find(_.nonEmpty).getOrElse(lnDefTitle)
      case PlainDescription(Some(split), invoiceText) => lnSplitNotice.format(split.sentRatio) + Some(invoiceText).find(_.nonEmpty).getOrElse(lnDefTitle)
      case PlainMetaDescription(Some(split), invoiceText, meta) => lnSplitNotice.format(split.sentRatio) + List(meta, invoiceText).find(_.nonEmpty).getOrElse(lnDefTitle)
    }

    private def setPaymentTypeIcon(holder: PaymentLineViewHolder, info: PaymentInfo): Unit =
      if (info.isIncoming) holder.setVisibleIcon(id = R.id.lnIncoming)
      else setOutgoingPaymentIcons(holder, info)

    private def setOutgoingPaymentIcons(holder: PaymentLineViewHolder, info: PaymentInfo): Unit = {
      holder.iconMap(R.id.lnOutgoingAction) setVisibility BaseActivity.goneMap(info.actionString != PaymentInfo.NO_ACTION)
      holder.iconMap(R.id.lnOutgoingBasic) setVisibility BaseActivity.goneMap(info.actionString == PaymentInfo.NO_ACTION)
      holder.setVisibleIcon(id = R.id.lnOutgoing)
    }

    private def paymentStatusIcon(info: PaymentInfo): Int =
      if (PaymentStatus.SUCCEEDED == info.status) R.drawable.baseline_done_24
      else if (PaymentStatus.ABORTED == info.status) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24

    private def paymentBackground(info: PaymentInfo): Int = info.isIncoming match {
      case true if LNParams.cm.inProcessors.contains(info.fullTag) => R.drawable.panel_payment_active_bg
      case false if LNParams.cm.opm.data.payments.contains(info.fullTag) => R.drawable.panel_payment_active_bg
      case _ => R.drawable.panel_payment_passive_bg
    }

    private def paymentMeta(info: PaymentInfo): String = if (info.isIncoming) {
      val valueHuman = LNParams.cm.inProcessors.get(info.fullTag).map(info.receivedRatio).map(WalletApp.app plurOrZero pctCollected)
      if (PaymentStatus.SUCCEEDED == info.status && valueHuman.isDefined) pctCollected.last // Notify user that we are not exactly done yet
      else if (PaymentStatus.SUCCEEDED == info.status) WalletApp.app.when(info.date) // Payment has been cleared in channels, show timestamp
      else valueHuman getOrElse pctCollected.head // Show either value collected so far or that we are still waiting
    } else {
      val partsHuman = LNParams.cm.opm.data.payments.get(info.fullTag).map(_.data.inFlightParts.size.toLong).map(WalletApp.app plurOrZero partsInFlight)
      if (PaymentStatus.PENDING == info.status) partsHuman getOrElse partsInFlight.head // Show either in-flight parts or that we are still preparing
      else partsHuman getOrElse WalletApp.app.when(info.date) // Payment has succeeded or failed, show either in-flight part leftovers or timestamp
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    private val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    private var lastVisibleIconId: Int = -1

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    val cardContainer: LinearLayout = itemView.findViewById(R.id.cardContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = itemView.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = itemView.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]
    itemView.setTag(self)
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val defaultHeader: LinearLayout = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]

    val totalBalance: TextView = view.findViewById(R.id.totalBalance).asInstanceOf[TextView]
    val totalFiatBalance: TextView = view.findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]

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
    val listCaption: RelativeLayout = view.findViewById(R.id.listCaption).asInstanceOf[RelativeLayout]
    val searchWrap: RelativeLayout = view.findViewById(R.id.searchWrap).asInstanceOf[RelativeLayout]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]

    def updateFiatRates: Unit = {
      val change = LNParams.fiatRatesInfo.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
    }

    def setCaptionVisibility: Unit = {
      val somePaymentsPresent = BaseActivity.goneMap(allInfos.nonEmpty)
      listCaption.setVisibility(somePaymentsPresent)
    }

    def updateView: Unit = lnBalance match { case currentLnBalance =>
      val walletBalance = currentLnBalance + WalletApp.lastChainBalance.totalBalance
      val states = LNParams.cm.all.values.map(_.state).take(8)

      TransitionManager.beginDelayedTransition(view)
      channelIndicator.createIndicators(states.toArray)
      channelStateIndicators setVisibility BaseActivity.goneMap(states.nonEmpty)
      totalFiatBalance setText WalletApp.currentMsatInFiatHuman(walletBalance).html
      totalBalance setText LNParams.denomination.parsedWithSign(walletBalance, totalZero).html
      syncIndicator setVisibility BaseActivity.invisMap(!WalletApp.lastChainBalance.isTooLongAgo)

      totalLightningBalance setVisibility BaseActivity.invisMap(states.nonEmpty)
      totalLightningBalance setText LNParams.denomination.parsedWithSign(currentLnBalance, lnCardZero).html
      totalBitcoinBalance setText LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, btcCardZero).html
      totalBitcoinBalance setVisibility BaseActivity.invisMap(WalletApp.lastChainBalance.totalBalance != 0L.msat)
      receiveBitcoinTip setVisibility BaseActivity.invisMap(WalletApp.lastChainBalance.totalBalance == 0L.msat)
      addChannelTip setVisibility BaseActivity.invisMap(states.isEmpty)

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
      pendingRefunds setVisibility BaseActivity.goneMap(currentPublished.nonEmpty)
    }
  }

  // LISTENERS

  private var streamSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]
  private var successSubscription = Option.empty[Subscription]

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
      walletCards.offlineIndicator setVisibility BaseActivity.goneMap(!isConnected)
      // This will make channels SLEEPING right away instead of after no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
    }.run
  }

  private val chainListener: WalletEventsListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = UITask {
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
    transactionInfo <- txInfos if !transactionInfo.isDeeplyBuried && !transactionInfo.isDoubleSpent
    (newDepth, newDoubleSpent) <- LNParams.chainWallet.wallet.doubleSpent(transactionInfo.tx)
    if newDepth != transactionInfo.depth || newDoubleSpent != transactionInfo.isDoubleSpent
    _ = WalletApp.txDataBag.updStatus(transactionInfo.txid, newDepth, newDoubleSpent)
    // Trigger preimage revealed using a txid to throttle multiple vibrations
  } ChannelMaster.preimageRevealStream.onNext(transactionInfo.txid)

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
    successSubscription.foreach(_.unsubscribe)
    getContentResolver.unregisterContentObserver(paymentObserver)
    getContentResolver.unregisterContentObserver(relayObserver)
    getContentResolver.unregisterContentObserver(txObserver)

    LNParams.chainWallet.eventsCatcher ! WalletEventsCatcher.Remove(chainListener)
    FiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def onBackPressed: Unit = {
    val isSearching = walletCards.searchWrap.getVisibility == View.VISIBLE
    if (isSearching) cancelSearch(null) else super.onBackPressed
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case prExt: PaymentRequestExt if !LNParams.ourInit.features.areSupported(prExt.pr.features.features) =>
      snack(contentWindow, getString(error_ln_send_features).html, dialog_ok, _.dismiss)

    case _: PaymentRequestExt if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) =>
      snack(contentWindow, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)

    case _: PaymentRequestExt if LNParams.cm.all.values.forall(Channel.isWaiting) =>
      snack(contentWindow, getString(error_ln_waiting).html, dialog_ok, _.dismiss)

    case _: PaymentRequestExt if LNParams.isChainDisconnectedTooLong =>
      snack(contentWindow, getString(error_ln_send_chain_disconnect).html, dialog_ok, _.dismiss)

    case _: PaymentRequestExt if LNParams.cm.allSortedSendable.last.commits.availableForSend < LNParams.minPayment =>
      val reserveHuman = LNParams.denomination.parsedWithSign(-LNParams.cm.allSortedSendable.head.commits.availableForSend, cardZero)
      snack(contentWindow, getString(error_ln_send_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)

    case prExt: PaymentRequestExt if prExt.pr.prefix != PaymentRequest.prefixes(LNParams.chainHash) =>
      snack(contentWindow, getString(error_ln_send_network).html, dialog_ok, _.dismiss)

    case prExt: PaymentRequestExt if prExt.pr.amount.exists(_ < LNParams.minPayment) =>
      val requestedHuman = LNParams.denomination.parsedWithSign(prExt.pr.amount.get, cardZero)
      val minHuman = LNParams.denomination.parsedWithSign(LNParams.minPayment, cardZero)
      val msg = getString(error_ln_send_small).format(requestedHuman, minHuman)
      snack(contentWindow, msg.html, dialog_ok, _.dismiss)

    case prExt: PaymentRequestExt if prExt.pr.isExpired =>
      snack(contentWindow, getString(error_ln_send_expired).html, dialog_ok, _.dismiss)

    case prExt: PaymentRequestExt if prExt.hasSplitIssue =>
      snack(contentWindow, getString(error_ln_send_split).html, dialog_ok, _.dismiss)

    case prExt: PaymentRequestExt =>
      LNParams.cm.checkIfSendable(prExt.pr.paymentHash) match {
        case Some(PaymentInfo.NOT_SENDABLE_IN_FLIGHT) => snack(contentWindow, getString(error_ln_send_in_flight).html, dialog_ok, _.dismiss)
        case Some(PaymentInfo.NOT_SENDABLE_SUCCESS) => snack(contentWindow, getString(error_ln_send_done_already).html, dialog_ok, _.dismiss)
        case _ => // Can send it
      }

    case uri: BitcoinUri if uri.isValid =>
      val body = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
      val manager = new RateManager(body, getString(dialog_add_memo).toSome, dialog_visibility_private, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, Colors.cardZero)
      val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.totalBalance)

      def switch(alert: AlertDialog): Unit = {
        uri.prExt.foreach(ext => InputParser.value = ext)
        checkExternalData(noneRunnable)
        alert.dismiss
      }

      def warnSendingFailed: TimerTask = UITask {
        val builder = new AlertDialog.Builder(me).setMessage(error_btc_broadcast_fail)
        showForm(builder.setNegativeButton(dialog_ok, null).create)
      }

      def attempt(alert: AlertDialog): Unit = {
        // On success tx will be recorded in a listener
        // on failure user will be notified right away
        alert.dismiss

        for {
          txAndFee <- LNParams.chainWallet.wallet.sendPayment(manager.resultSat, uri.address, feeView.rate)
          // Record this description before attempting to send, we won't be able to know a memo otherwise
          knownDescription = PlainTxDescription(uri.address :: Nil, manager.resultExtraInput)
          _ = WalletApp.txDataBag.descriptions += (txAndFee.tx.txid -> knownDescription)
          isDefinitelyCommitted <- LNParams.chainWallet.wallet.commit(txAndFee.tx)
          if !isDefinitelyCommitted
        } warnSendingFailed.run
      }

      lazy val alert = {
        val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
        val builder = titleBodyAsViewBuilder(getString(dialog_send_btc).format(uri.address.shortAddress, BaseActivity formattedBitcoinUri uri).html, manager.content)
        if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, _ => manager.updateText(WalletApp.lastChainBalance.totalBalance), builder, dialog_pay, dialog_cancel, neutralRes)
        else mkCheckFormNeutral(attempt, none, switch, builder, dialog_pay, dialog_cancel, lightning_wallet)
      }

      lazy val feeView = new FeeView(body) {
        override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): TimerTask = {
          manager.updateOkButton(getPositiveButton(alert), feeOpt.isDefined).run
          super.update(feeOpt, showIssue)
        }

        rate = {
          val target = LNParams.feeRatesInfo.onChainFeeConf.feeTargets.mutualCloseBlockTarget
          LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(target)
        }
      }

      lazy val worker = new ThrottledWork[Satoshi, TxAndFee] {
        def work(amount: Satoshi): Observable[TxAndFee] = Rx fromFutureOnIo LNParams.chainWallet.wallet.sendPayment(amount, uri.address, feeView.rate)
        def process(amount: Satoshi, txAndFee: TxAndFee): Unit = feeView.update(feeOpt = Some(txAndFee.fee.toMilliSatoshi), showIssue = false).run
        def error(exc: Throwable): Unit = feeView.update(feeOpt = None, showIssue = manager.resultSat >= LNParams.minDustLimit).run
      }

      feeView.customFeerate addOnChangeListener new Slider.OnChangeListener {
        override def onValueChange(slider: Slider, value: Float, fromUser: Boolean): Unit = {
          val newFeerate = FeeratePerVByte(value.toLong.sat)
          feeView.rate = FeeratePerKw(newFeerate)
          worker addWork manager.resultSat
        }
      }

      manager.inputAmount addTextChangedListener onTextChange { _ =>
        worker addWork manager.resultSat
      }

      uri.amount.foreach { asked =>
        manager.updateText(value = asked)
        manager.inputAmount.setEnabled(false)
        manager.fiatInputAmount.setEnabled(false)
      }

      manager.hintDenom.setText(getString(dialog_can_send).format(canSend).html)
      manager.hintFiatDenom.setText(getString(dialog_can_send).format(canSendFiat).html)
      feeView.update(feeOpt = None, showIssue = false).run

    case _: LNUrl =>

    case _: RemoteNodeInfo => me goTo ClassNames.remotePeerActivityClass

    case _ => whenNone.run
  }

  // Important: order of cases matters here
  override def onChoiceMade(tag: String, pos: Int): Unit = (tag, pos) match {
    case (CHOICE_RECEIVE_TAG, 1) if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) =>
      snack(contentWindow, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.all.values.forall(Channel.isWaiting) =>
      snack(contentWindow, getString(error_ln_waiting).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.allSortedReceivable.isEmpty =>
      snack(contentWindow, getString(error_ln_receive_no_update).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) if LNParams.cm.allSortedReceivable.last.commits.availableForReceive < MilliSatoshi(0L) =>
      val reserveHuman = LNParams.denomination.parsedWithSign(-LNParams.cm.allSortedReceivable.head.commits.availableForReceive, cardZero)
      snack(contentWindow, getString(error_ln_receive_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)

    case (CHOICE_RECEIVE_TAG, 1) =>
      val commits = LNParams.cm.maxReceivable(LNParams.cm.allSortedReceivable).head.commits
      val body = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
      val manager = new RateManager(body, getString(dialog_add_description).toSome, dialog_visibility_public, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      val canReceive = LNParams.denomination.parsedWithSign(commits.availableForReceive, Colors.cardZero)
      val canReceiveFiat = WalletApp.currentMsatInFiatHuman(commits.availableForReceive)

      def attempt(alert: AlertDialog): Unit = {
        val preimage: ByteVector32 = randomBytes32
        val hash: ByteVector32 = Crypto.sha256(preimage)
        val invoiceKey = LNParams.secret.keys.fakeInvoiceKey(hash)
        val hop = List(commits.updateOpt.map(_ extraHop commits.remoteInfo.nodeId).toList)
        val description = PlainDescription(split = None, manager.resultExtraInput getOrElse new String)
        val prExt = PaymentRequestExt from PaymentRequest(LNParams.chainHash, Some(manager.resultMsat), hash, invoiceKey, description.invoiceText, LNParams.incomingFinalCltvExpiry, hop)
        val chainFee = Transactions.weight2fee(LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget), 700)
        LNParams.cm.payBag.replaceIncomingPayment(prExt, preimage, description, lnBalance, LNParams.fiatRatesInfo.rates, chainFee.toMilliSatoshi)
        me goTo ClassNames.qrInvoiceActivityClass
        InputParser.value = prExt
        alert.dismiss
      }

      val alert = mkCheckFormNeutral(attempt, none, _ => manager.updateText(commits.availableForReceive),
        titleBodyAsViewBuilder(getString(dialog_receive_ln).html, manager.content), dialog_ok, dialog_cancel, dialog_max)

      manager.hintFiatDenom.setText(getString(dialog_can_receive).format(canReceiveFiat).html)
      manager.hintDenom.setText(getString(dialog_can_receive).format(canReceive).html)
      manager.updateOkButton(getPositiveButton(alert), isEnabled = false).run

      manager.inputAmount addTextChangedListener onTextChange { _ =>
        val notTooLow: Boolean = LNParams.minPayment <= manager.resultMsat
        val notTooHigh: Boolean = commits.availableForReceive >= manager.resultMsat
        manager.updateOkButton(getPositiveButton(alert), notTooLow && notTooHigh).run
      }

    case (CHOICE_RECEIVE_TAG, 0) =>
      me goTo ClassNames.qrChainActivityClass

    case _ =>
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(PaymentTable.table), true, paymentObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(RelayTable.table), true, relayObserver)
      getContentResolver.registerContentObserver(WalletApp.app.sqlPath(TxTable.table), true, txObserver)
      LNParams.chainWallet.eventsCatcher ! chainListener
      FiatRates.listeners += fiatRatesListener
      Tovuti.from(me).monitor(netListener)

      walletCards.searchField addTextChangedListener onTextChange { query =>
        searchWorker addWork query.toString
      }

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      runInFutureProcessOnUI(loadRecentInfos, none) { _ =>
        updatePaymentList
      }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      walletCards.updateFiatRates
      walletCards.updateView

      // Throttle all types of burst updates, but make sure the last one is always called
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, 1.second)
      val txEvents = Rx.uniqueFirstAndLastWithinWindow(txEventStream, 1.second).doOnNext(_ => reloadTxInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(paymentEventStream, 1.second).doOnNext(_ => reloadPaymentInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(relayEventStream, 1.second).doOnNext(_ => reloadRelayedPreimageInfos)

      streamSubscription = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(stateEvents).subscribe(_ => UITask(updatePaymentList).run).toSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, 1.second).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).toSome
      successSubscription = ChannelMaster.preimageRevealStream.merge(ChannelMaster.preimageObtainStream).throttleFirst(1.second).subscribe(_ => Vibrator.vibrate).toSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringMenu(view: View): Unit = {
    val popupMenu = new PopupMenu(me, view)
    popupMenu setOnMenuItemClickListener new PopupMenu.OnMenuItemClickListener {
      override def onMenuItemClick(selectedMenuItem: MenuItem): Boolean = false
    }

    popupMenu.getMenu.add(0, 0, 0, menu_settings)
    popupMenu.getMenu.add(0, 1, 1, menu_view_chans)
    popupMenu.show
  }

  def bringSearch(view: View): Unit = {
    TransitionManager.beginDelayedTransition(walletCards.view)
    walletCards.defaultHeader setVisibility View.GONE
    walletCards.searchWrap setVisibility View.VISIBLE
    walletCards.searchField.requestFocus
  }

  def cancelSearch(view: View): Unit = {
    TransitionManager.beginDelayedTransition(walletCards.view)
    walletCards.defaultHeader setVisibility View.VISIBLE
    walletCards.searchWrap setVisibility View.GONE
    walletCards.searchField.setText(new String)
  }

  def bringSendFromClipboard(view: View): Unit = {
    def explainClipboardFailure: TimerTask = UITask {
      val message = getString(error_nothing_in_clipboard).html
      snack(contentWindow, message, dialog_ok, _.dismiss)
    }

    runInFutureProcessOnUI(InputParser.recordValue(WalletApp.app.getBufferUnsafe),
      _ => explainClipboardFailure.run)(_ => me checkExternalData explainClipboardFailure)
  }

  def bringScanner(view: View): Unit = callScanner(me)

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(res => getString(res).html)
    val list = makeChoiceList(options, android.R.layout.simple_expandable_list_item_1)
    val sheet = new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me)
    sheet.show(getSupportFragmentManager, CHOICE_RECEIVE_TAG)
  }

  def goToReceiveBitcoinPage(view: View): Unit = onChoiceMade(CHOICE_RECEIVE_TAG, 0)
  def bringLnReceivePopup(view: View): Unit = onChoiceMade(CHOICE_RECEIVE_TAG, 1)

  def updatePaymentList: Unit = {
    paymentsAdapter.notifyDataSetChanged
    walletCards.setCaptionVisibility
  }
}
