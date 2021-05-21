package com.lightning.walletapp

import immortan._
import immortan.utils._
import android.widget._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.softwaremill.quicklens._
import com.lightning.walletapp.Colors._
import com.lightning.walletapp.R.string._
import immortan.utils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{Satoshi, SatoshiLong}
import android.view.{MenuItem, View, ViewGroup}
import rx.lang.scala.{Observable, Subscription}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, FeeratePerVByte}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.lightning.walletapp.BaseActivity.StringOps
import com.google.android.material.snackbar.Snackbar
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.slider.Slider
import androidx.transition.TransitionManager
import fr.acinq.eclair.blockchain.TxAndFee
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import fr.acinq.eclair.wire.PaymentTagTlv
import immortan.sqlite.Table
import org.ndeftools.Message
import java.util.TimerTask
import android.os.Bundle


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
          holder.labelIcon setVisibility View.GONE
          holder.detailsAndStatus setVisibility View.GONE
          holder.amount setText LNParams.denomination.directedWithSign(info.earned, 0L.msat, cardZero, isPlus = true).html
          if (LNParams.cm.inProcessors contains info.fullTag) holder.cardContainer setBackgroundResource R.drawable.panel_payment_active_bg
          else holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.meta setText WalletApp.app.when(info.date).html
          holder.setVisibleIcon(id = R.id.lnRouted)

        case info: TxInfo =>
          holder.detailsAndStatus setVisibility View.VISIBLE
          holder.description setText txDescription(info).html
          setVis(info.description.label.isDefined, holder.labelIcon)
          holder.amount setText LNParams.denomination.directedWithSign(info.receivedSat.toMilliSatoshi, info.sentSat.toMilliSatoshi, cardZero, info.isIncoming).html
          holder.cardContainer setBackgroundResource R.drawable.panel_payment_passive_bg
          holder.statusIcon setImageResource txStatusIcon(info)
          holder.meta setText txMeta(info).html
          setTxTypeIcon(holder, info)

        case info: PaymentInfo =>
          holder.detailsAndStatus setVisibility View.VISIBLE
          holder.description setText paymentDescription(info).html
          setVis(info.description.label.isDefined, holder.labelIcon)
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

    private def labelOrFallback(plainTxDescription: PlainTxDescription): String = plainTxDescription.label.getOrElse {
      val htmlOpt = plainTxDescription.addresses.headOption.map(_.shortAddress).map(short => s"&#160;<font color=$cardZero>$short</font>")
      getString(tx_btc) + htmlOpt.getOrElse(new String)
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
        setVis(info.sentSat != info.receivedSat, holder iconMap R.id.btcOutgoingNormal)
        setVis(info.sentSat == info.receivedSat, holder iconMap R.id.btcOutgoingToSelf)
        holder.setVisibleIcon(id = R.id.btcOutgoing)
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

    private def paymentDescription(info: PaymentInfo): String = info.description.split match {
      case Some(split) => lnSplitNotice.format(split.sentRatio) + info.description.finalDescription.getOrElse(lnDefTitle)
      case None => info.description.finalDescription.getOrElse(lnDefTitle)
    }

    private def setPaymentTypeIcon(holder: PaymentLineViewHolder, info: PaymentInfo): Unit =
      if (info.isIncoming) holder.setVisibleIcon(id = R.id.lnIncoming)
      else setOutgoingPaymentIcons(holder, info)

    private def setOutgoingPaymentIcons(holder: PaymentLineViewHolder, info: PaymentInfo): Unit = {
      setVis(info.actionString != PaymentInfo.NO_ACTION, holder iconMap R.id.lnOutgoingAction)
      setVis(info.actionString == PaymentInfo.NO_ACTION, holder iconMap R.id.lnOutgoingBasic)
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
    val labelIcon: ImageView = itemView.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
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

    def updateView: Unit = lnBalance match { case currentLnBalance =>
      val walletBalance = currentLnBalance + WalletApp.lastChainBalance.totalBalance
      val states = LNParams.cm.all.values.map(_.state).take(8)

      TransitionManager.beginDelayedTransition(view)
      setVis(states.nonEmpty, channelStateIndicators)
      setVis(WalletApp.lastChainBalance.isTooLongAgo, syncIndicator)
      totalFiatBalance setText WalletApp.currentMsatInFiatHuman(walletBalance).html
      totalBalance setText LNParams.denomination.parsedWithSign(walletBalance, totalZero).html
      channelIndicator.createIndicators(states.toArray)

      totalLightningBalance setText LNParams.denomination.parsedWithSign(currentLnBalance, lnCardZero).html
      totalBitcoinBalance setText LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, btcCardZero).html
      setVis(WalletApp.lastChainBalance.totalBalance != 0L.msat, totalBitcoinBalance)
      setVis(WalletApp.lastChainBalance.totalBalance == 0L.msat, receiveBitcoinTip)
      setVis(states.nonEmpty, totalLightningBalance)
      setVis(states.isEmpty, addChannelTip)

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
      setVis(currentPublished.nonEmpty, pendingRefunds)
    }
  }

  // LISTENERS

  private var streamSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]
  private var successSubscription = Option.empty[Subscription]

  private val netListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      // This will make channels SLEEPING right away instead of a bit later when we receive no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
      setVis(!isConnected, walletCards.offlineIndicator)
    }.run
  }

  private val chainListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = {
      // Check if any of pending chain txs got confirmations
      UITask(walletCards.updateView).run

      for {
        transactionInfo <- txInfos if !transactionInfo.isDeeplyBuried && !transactionInfo.isDoubleSpent
        (newDepth, newDoubleSpent) <- LNParams.chainWallet.wallet.doubleSpent(transactionInfo.tx)
        if newDepth != transactionInfo.depth || newDoubleSpent != transactionInfo.isDoubleSpent
        _ = WalletApp.txDataBag.updStatus(transactionInfo.txid, newDepth, newDoubleSpent)
        // Trigger preimage revealed using a txid to throttle multiple vibrations
      } ChannelMaster.hashRevealStream.onNext(transactionInfo.txid)
    }
  }

  private val fiatRatesListener = new FiatRatesListener {
    def onFiatRates(rates: FiatRatesInfo): Unit = UITask {
      walletCards.updateFiatRates
      walletCards.updateView
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
    statusSubscription.foreach(_.unsubscribe)
    streamSubscription.foreach(_.unsubscribe)
    successSubscription.foreach(_.unsubscribe)

    LNParams.chainWallet.eventsCatcher ! WalletEventsCatcher.Remove(chainListener)
    FiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def onBackPressed: Unit =
    if (walletCards.searchWrap.getVisibility == View.VISIBLE) cancelSearch(null)
    else if (currentSnackbar.isDefined) removeCurrentSnack.run
    else super.onBackPressed

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case bitcoinUri: BitcoinUri if bitcoinUri.isValid => bringSendBitcoinPopup(bitcoinUri)
    case _: RemoteNodeInfo => me goTo ClassNames.remotePeerActivityClass

    case prExt: PaymentRequestExt =>
      lnSendGuard(prExt, contentWindow) {
        case Some(origAmount) if prExt.splits.nonEmpty =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable.min(prExt.splitLeftover * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < prExt.splitLeftover - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= prExt.splitLeftover && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)

            override def send(alert: AlertDialog): Unit = {
              val cmd = makeSendCmd(prExt, toSend = manager.resultMsat).modify(_.split.totalSum).setTo(origAmount)
              val description = PlainDescription(cmd.split.toSome, label = manager.resultExtraInput, invoiceText = prExt.descriptionOrEmpty)
              replaceOutgoingPayment(prExt, description, action = None, cmd.split.myPart)
              LNParams.cm.opm process cmd
              alert.dismiss
            }

            override val alert: AlertDialog = {
              val leftHuman = LNParams.denomination.parsedWithSign(prExt.splitLeftover, Colors.cardZero)
              val totalHuman = LNParams.denomination.parsedWithSign(origAmount, Colors.cardZero)
              val total = getString(dialog_split_ln_total).format(s"&#160;$totalHuman")
              val left = getString(dialog_split_ln_left).format(s"&#160;$leftHuman")
              val details = s"${prExt.brDescription}<br><br>$total<br>$left"

              val title = updateView2Color(new String, getString(dialog_split_ln).format(details), R.color.cardLightning)
              mkCheckFormNeutral(send, none, neutral, titleBodyAsViewBuilder(title, manager.content), dialog_pay, dialog_cancel, dialog_split)
            }

            // Prefill with what's left to pay
            manager.updateText(prExt.splitLeftover)
          }

        case Some(origAmount) =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable.min(origAmount * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < origAmount - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= origAmount && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)

            override val alert: AlertDialog = {
              val title = updateView2Color(new String, getString(dialog_send_ln).format(prExt.brDescription), R.color.cardLightning)
              mkCheckFormNeutral(send, none, neutral, titleBodyAsViewBuilder(title, manager.content), dialog_pay, dialog_cancel, dialog_split)
            }

            // Prefill with asked amount
            manager.updateText(origAmount)
          }

        case None =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable, minSendable = LNParams.minPayment) {
            override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = manager.updateText(maxSendable)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)
            override def isNeutralEnabled: Boolean = true

            override val alert: AlertDialog = {
              val title = updateView2Color(new String, getString(dialog_send_ln).format(prExt.brDescription), R.color.cardLightning)
              mkCheckFormNeutral(send, none, neutral, titleBodyAsViewBuilder(title, manager.content), dialog_pay, dialog_cancel, dialog_max)
            }

            // Do not prefill since amount is unknown, disable pay button
            manager.updateButton(getPositiveButton(alert), isEnabled = false).run
          }
      }

    case lnUrl: LNUrl =>
      lnUrl.fastWithdrawAttempt.toOption match {
        case Some(withdraw) => bringWithdrawPopup(withdraw)
        case None if lnUrl.isAuth => showAuthForm(lnUrl)
        case None => resolveLnurl(lnUrl)
      }

    case _ =>
      whenNone.run
  }

  def resolveLnurl(lnUrl: LNUrl): Unit = {
    val resolve: PartialFunction[LNUrlData, Unit] = {
      case pay: PayRequest => UITask(me bringPayPopup pay).run
      case withdraw: WithdrawRequest => UITask(me bringWithdrawPopup withdraw).run
      case _ => UITask(WalletApp.app quickToast error_nothing_useful).run
    }

    val msg = getString(dialog_lnurl_processing).format(lnUrl.uri.getHost).html
    val subscription = lnUrl.level1DataResponse.doOnTerminate(removeCurrentSnack.run).subscribe(resolve, onFail)
    def cancel(snackBar: Snackbar): Unit = runAnd(subscription.unsubscribe)(snackBar.dismiss)
    snack(contentWindow, msg, dialog_cancel, cancel)
  }

  def showAuthForm(lnUrl: LNUrl): Unit = {

  }

  override def onChoiceMade(tag: String, pos: Int): Unit = (tag, pos) match {
    case (CHOICE_RECEIVE_TAG, 0) => me goTo ClassNames.qrChainActivityClass
    case (CHOICE_RECEIVE_TAG, 1) => bringReceivePopup
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      LNParams.chainWallet.eventsCatcher ! chainListener
      FiatRates.listeners += fiatRatesListener
      Tovuti.from(me).monitor(netListener)

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
      runInFutureProcessOnUI(loadRecentInfos, none) { _ => updatePaymentList }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      walletCards.updateFiatRates
      walletCards.updateView

      val window = 500.millis
      // Throttle all types of burst updates, but make sure the last one is always called
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window)
      val txEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.txDbStream, window).doOnNext(_ => reloadTxInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.paymentDbStream, window).doOnNext(_ => reloadPaymentInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.relayDbStream, window).doOnNext(_ => reloadRelayedPreimageInfos)

      streamSubscription = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(stateEvents).subscribe(_ => UITask(updatePaymentList).run).toSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).toSome
      successSubscription = ChannelMaster.hashRevealStream.merge(ChannelMaster.hashObtainStream).throttleFirst(window).subscribe(_ => removeSnackAndVibrate).toSome
      // Run this check after establishing subscriptions since it will trigger an event stream
      LNParams.cm.markAsFailed(paymentInfos, LNParams.cm.allInChannelOutgoing)
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

  def bringSendBitcoinPopup(uri: BitcoinUri): Unit = {
    val body = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
    val manager = new RateManager(body, getString(dialog_add_btc_memo).toSome, dialog_visibility_private, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
    val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, Colors.cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(WalletApp.lastChainBalance.totalBalance)

    def switchToLn(alert: AlertDialog): Unit = {
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
        _ = WalletApp.txDescriptions += Tuple2(txAndFee.tx.txid, knownDescription)
        isDefinitelyCommitted <- LNParams.chainWallet.wallet.commit(txAndFee.tx)
        if !isDefinitelyCommitted
      } warnSendingFailed.run
    }

    lazy val alert = {
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
      val message = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
      val builder = titleBodyAsViewBuilder(updateView2Color(new String, getString(dialog_send_btc).format(uri.address.shortAddress, label + message), R.color.cardBitcoin), manager.content)
      if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, _ => manager.updateText(WalletApp.lastChainBalance.totalBalance), builder, dialog_pay, dialog_cancel, neutralRes)
      else mkCheckFormNeutral(attempt, none, switchToLn, builder, dialog_pay, dialog_cancel, lightning_wallet)
    }

    lazy val feeView = new FeeView(body) {
      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): TimerTask = {
        manager.updateButton(getPositiveButton(alert), feeOpt.isDefined).run
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
  }

  def bringReceivePopup: Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat, lnBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).toSome, dialog_visibility_public, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainDescription(split = None, label = None, invoiceText = manager.resultExtraInput getOrElse new String)
      override def getTitleText: CharSequence = getString(dialog_receive_ln).html

      override def processInvoice(prExt: PaymentRequestExt): Unit = {
        me goTo ClassNames.qrInvoiceActivityClass
        InputParser.value = prExt
      }
    }
  }

  def bringWithdrawPopup(data: WithdrawRequest): Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = data.maxWithdrawable.msat, initMinReceivable = data.minCanReceive, lnBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_ln_memo).toSome, dialog_visibility_private, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainMetaDescription(split = None, label = manager.resultExtraInput, invoiceText = new String, meta = data.descriptionOrEmpty)
      override def getTitleText: CharSequence = getString(dialog_lnurl_withdraw).format(data.callbackUri.getHost, data.brDescription)

      override def processInvoice(prExt: PaymentRequestExt): Unit = {
        val amountHuman = LNParams.denomination.parsedWithSign(prExt.pr.amount.get, Colors.cardZero)
        val message = getString(dialog_lnurl_withdrawing).format(amountHuman, data.callbackUri.getHost)
        // Note: there is no logic here to remove a snack on success, it should be done on receiving of payment
        data.requestWithdraw(prExt).doOnError(_ => removeCurrentSnack.run).foreach(none, onFail)
        snack(contentWindow, message.html, dialog_ok, _.dismiss)
      }
    }
  }

  def bringPayPopup(data: PayRequest): Unit =
    new OffChainSender(maxSendable = LNParams.cm.maxSendable min data.maxSendable.msat, minSendable = LNParams.minPayment max data.minSendable.msat) {
      override val manager = new RateManager(body, getString(dialog_add_comment).toSome, dialog_visibility_public, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
      override def isNeutralEnabled: Boolean = manager.resultMsat >= LNParams.minPayment && manager.resultMsat <= minSendable - LNParams.minPayment
      override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable

      override def neutral(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): Unit = lnSendGuard(pf.prExt, contentWindow) { _ =>
          val cmd = makeSendCmd(pf.prExt, toSend = manager.resultMsat).modify(_.split.totalSum).setTo(minSendable)
          val description = PlainMetaDescription(cmd.split.toSome, label = None, invoiceText = new String, meta = data.metaDataTextPlain)
          InputParser.value = SplitParams(pf.prExt, pf.successAction, description, cmd, typicalChainTxFee)
          me goTo ClassNames.qrSplitActivityClass
          alert.dismiss
        }
      }

      override def send(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): Unit = lnSendGuard(pf.prExt, contentWindow) { _ =>
          val cmd = makeSendCmd(pf.prExt, toSend = manager.resultMsat).modify(_.split.totalSum).setTo(manager.resultMsat)
          val description = PlainMetaDescription(split = None, label = None, invoiceText = new String, meta = data.metaDataTextPlain)
          replaceOutgoingPayment(pf.prExt, description, pf.successAction, cmd.split.myPart)
          LNParams.cm.opm process cmd
          alert.dismiss
        }
      }

      override val alert: AlertDialog = {
        val text = getString(dialog_lnurl_pay).format(data.callbackUri.getHost, s"<br><br>${data.metaDataTextPlain}")
        val title = titleBodyAsViewBuilder(updateView2Color(new String, text, R.color.cardLightning), manager.content)
        mkCheckFormNeutral(send, none, neutral, title, dialog_pay, dialog_cancel, dialog_split)
      }

      private def getFinal(amount: MilliSatoshi) =
        data.requestFinal(amount).map { rawResponse =>
          val payRequestFinal = to[PayRequestFinal](rawResponse)
          val descriptionHash = payRequestFinal.prExt.pr.description.right
          require(descriptionHash.toOption.contains(data.metaDataHash), s"Metadata hash mismatch, original=${data.metaDataHash}, provided=$descriptionHash")
          require(payRequestFinal.prExt.pr.amount.contains(amount), s"Payment amount mismatch, provided=${payRequestFinal.prExt.pr.amount}, requested=$amount")
          for (additionalEdge <- payRequestFinal.additionalRoutes) LNParams.cm.pf process additionalEdge
          payRequestFinal.modify(_.successAction.each.domain).setTo(data.callbackUri.getHost.toSome)
        }

      // Either prefill if amount is fixed or disable PAY buttom
      if (data.minSendable == data.maxSendable) manager.updateText(minSendable)
      else manager.updateButton(getPositiveButton(alert), isEnabled = false).run
    }

  def updatePaymentList: Unit = {
    setVis(allInfos.nonEmpty, walletCards.listCaption)
    paymentsAdapter.notifyDataSetChanged
  }

  def removeSnackAndVibrate: Unit = {
    // Haptic feedback on successfully sent and received payments
    // Also removes snackbar (if present) for lnurl-withdraw/pay
    removeCurrentSnack.run
    Vibrator.vibrate
  }
}
