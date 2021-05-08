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
import android.view.{View, ViewGroup}
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
          if (!txInfo.isIncoming && Satoshi(0L) == txInfo.sentSat) holder.amount setText getString(tx_notice_sent_to_self).html
          else holder.amount setText LNParams.denomination.directedWithSign(txInfo.receivedSat.toMilliSatoshi, txInfo.sentSat.toMilliSatoshi, cardZero, txInfo.isIncoming).html
          holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.meta setText txMeta(txInfo).html
          setTypeIcon(holder, txInfo)

        case _: PaymentInfo =>
      }

      view
    }

    private def setTypeIcon(holder: PaymentLineViewHolder, info: TxInfo): Unit = info.description match {
      case _: PlainTxDescription if info.isIncoming => holder.setPaymentTypeVisibility(visible = R.id.btcIncoming)
      case _: OpReturnTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcOutgoing)
      case _: ChanRefundingTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
      case _: ChanFundingTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcLn)
      case _: PlainTxDescription => holder.setPaymentTypeVisibility(visible = R.id.btcOutgoing)
      case _: HtlcClaimTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
      case _: PenaltyTxDescription => holder.setPaymentTypeVisibility(visible = R.id.lnBtc)
    }

    private def txDescription(info: TxInfo): String = info.description match {
      case plainTxDescription: PlainTxDescription => labelOrFallback(plainTxDescription)
      case _: ChanRefundingTxDescription => getString(tx_description_refunding)
      case _: HtlcClaimTxDescription => getString(tx_description_htlc_claim)
      case _: ChanFundingTxDescription => getString(tx_description_funding)
      case _: OpReturnTxDescription => getString(tx_description_op_return)
      case _: PenaltyTxDescription => getString(tx_description_penalty)
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
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) { self =>
    def setPaymentTypeVisibility(visible: Int): Unit = for (id <- paymentTypeIconIds) typeMap(id) setVisibility BaseActivity.goneMap(id == visible)
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
      walletCards.offlineIndicator setVisibility BaseActivity.goneMap(!isConnected)
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

  override def onBackPressed: Unit = {
    val isSearching = walletCards.searchWrap.getVisibility == View.VISIBLE
    if (isSearching) cancelSearch(null) else super.onBackPressed
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case _: RemoteNodeInfo => me goTo ClassNames.remotePeerActivityClass

    case _: PaymentRequestExt =>

    case uri: BitcoinUri if uri.isValid =>
      val body = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
      val manager = new RateManager(body, getString(dialog_add_memo).toSome, dialog_visibility_private, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, Colors.cardZero)
      val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.totalBalance)

      def attempt(alert: AlertDialog): Unit = {
        def warnTxSendingFailed: TimerTask = UITask {
          val builder = new AlertDialog.Builder(me).setMessage(error_btc_broadcast_fail)
          showForm(builder.setNegativeButton(dialog_ok, null).create)
        }

        for {
          txAndFee <- LNParams.chainWallet.wallet.sendPayment(manager.resultSat, uri.address, feeView.rate)
          // Record this description before attempting to send, we won't be able to know a memo otherwise
          knownDescription = PlainTxDescription(uri.address :: Nil, manager.resultExtraInput)
          _ = WalletApp.txDataBag.descriptions += Tuple2(txAndFee.tx.txid, knownDescription)
          isDefinitelyCommitted <- LNParams.chainWallet.wallet.commit(txAndFee.tx)
          if !isDefinitelyCommitted
        } warnTxSendingFailed.run
        alert.dismiss
      }

      lazy val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      lazy val builder = titleBodyAsViewBuilder(getString(dialog_send_btc).format(uri.address.shortAddress, BaseActivity formattedBitcoinUri uri).html, manager.content)
      lazy val alert = mkCheckFormNeutral(attempt, none, _ => manager.updateText(WalletApp.lastChainBalance.totalBalance), builder, dialog_pay, dialog_cancel, neutralRes)

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
      val commits = LNParams.cm.maxReceivable(LNParams.cm.allSortedReceivable).head.commits
      val body = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
      val manager = new RateManager(body, getString(dialog_add_description).toSome, dialog_visibility_public, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      val canReceive = LNParams.denomination.parsedWithSign(commits.availableForReceive, Colors.cardZero)
      val canReceiveFiat = WalletApp.currentMsatInFiatHuman(commits.availableForReceive)

      def attempt(alert: AlertDialog): Unit = {
        val preimage: ByteVector32 = randomBytes32
        val hash: ByteVector32 = Crypto.sha256(preimage)
        val invoiceKey = LNParams.secret.keys.fakeInvoiceKey(hash)
        val description = PlainDescription(manager.resultExtraInput getOrElse new String)
        val hop = List(commits.updateOpt.map(_ extraHop commits.remoteInfo.nodeId).toList)
        val prExt = PaymentRequestExt from PaymentRequest(LNParams.chainHash, Some(manager.resultMsat), hash, invoiceKey, description.invoiceText, LNParams.incomingFinalCltvExpiry, hop)
        val typicalChainFee = Transactions.weight2fee(LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget), 700)
        LNParams.cm.payBag.replaceIncomingPayment(prExt, preimage, description, lnBalance, LNParams.fiatRatesInfo.rates, typicalChainFee.toMilliSatoshi)
        me goTo ClassNames.qrInvoiceActivityClass
        InputParser.value = prExt
        alert.dismiss
      }

      val alert = mkCheckFormNeutral(attempt, none, _ => manager.updateText(commits.availableForReceive),
        titleBodyAsViewBuilder(getString(dialog_receive_ln).html, manager.content), dialog_ok, dialog_cancel, dialog_max)

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

      walletCards.searchField addTextChangedListener onTextChange { query =>
        searchWorker addWork query.toString
      }

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      runInFutureProcessOnUI(loadRecentInfos, none) { _ =>
        itemsList.setAdapter(paymentsAdapter)
        walletCards.setCaptionVisibility
      }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      walletCards.updateFiatRates
      walletCards.updateView

      // Throttle all types of burst updates, but make sure the last one is always called
      val statusEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, 1.second)
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, 1.second)

      val txEvents = Rx.uniqueFirstAndLastWithinWindow(txEventStream, 1.second).doOnNext(_ => reloadTxInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(paymentEventStream, 1.second).doOnNext(_ => reloadPaymentInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(relayEventStream, 1.second).doOnNext(_ => reloadRelayedPreimageInfos)
      val paymentRelatedEvents = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(stateEvents)
      statusSubscription = statusEvents.merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).toSome
      streamSubscription = paymentRelatedEvents.subscribe(_ => UITask(updatePaymentList).run).toSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringSettings(view: View): Unit = {
    ChannelMaster.notifyStateUpdated
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

  def goToReceiveBitcoinPage(view: View): Unit =
    me goTo ClassNames.qrChainActivityClass

  // VIEW UPDATERS

  def updatePaymentList: Unit = {
    paymentsAdapter.notifyDataSetChanged
    walletCards.setCaptionVisibility
  }
}
