package com.lightning.walletapp

import immortan._
import immortan.utils._
import android.widget._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import com.softwaremill.quicklens._
import com.lightning.walletapp.Colors._
import com.lightning.walletapp.R.string._
import immortan.utils.ImplicitJsonFormats._
import com.lightning.walletapp.HubActivity._

import scala.util.{Success, Try}
import java.lang.{Integer => JInt}
import android.view.{MenuItem, View, ViewGroup}
import rx.lang.scala.{Observable, Subscription}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import immortan.ChannelMaster.{OutgoingAdds, RevealedLocalFulfills}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, FeeratePerVByte}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.google.android.material.button.MaterialButton
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.transactions.RemoteFulfill
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.slider.Slider
import androidx.transition.TransitionManager
import com.google.common.cache.CacheBuilder
import immortan.ChannelListener.Malfunction
import fr.acinq.eclair.blockchain.TxAndFee
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import java.util.concurrent.TimeUnit
import android.content.Intent
import immortan.sqlite.Table
import org.ndeftools.Message
import java.util.TimerTask
import android.os.Bundle
import android.net.Uri


object HubActivity {
  var txInfos: Iterable[TxInfo] = Iterable.empty
  var paymentInfos: Iterable[PaymentInfo] = Iterable.empty
  var payLinkInfos: Iterable[PayLinkInfo] = Iterable.empty
  var relayedPreimageInfos: Iterable[RelayedPreimageInfo] = Iterable.empty
  var hashToReveals: Map[ByteVector32, RevealedLocalFulfills] = Map.empty
  var allInfos: Seq[TransactionDetails] = Nil

  var lnPaymentsAdded: Set[ByteVector32] = Set.empty
  var chainTxsAdded: Set[ByteVector32] = Set.empty
  var relaysAdded: Set[ByteVector32] = Set.empty
  var payLinksAdded: Set[String] = Set.empty

  // Run clear up method once on app start, do not re-run it every time this activity gets restarted
  lazy val markAsFailedOnce: Unit = LNParams.cm.markAsFailed(paymentInfos, LNParams.cm.allInChannelOutgoing)
}

class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver with ChannelListener { me =>
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

  def reloadTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(Table.DEFAULT_LIMIT.get).map(WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos = LNParams.cm.payBag.listRecentPayments(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos = LNParams.cm.payBag.listRecentRelays(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toRelayedPreimageInfo)

  def updAllInfos: Unit = {
    val lnRefunds = LNParams.cm.pendingRefundsAmount.toMilliSatoshi
    val delayedRefunds = if (lnRefunds > 0L.sat) DelayedRefunds(lnRefunds).asSome else None
    hashToReveals = LNParams.cm.allHosted.flatMap(Channel.chanAndCommitsOpt).flatMap(_.commits.revealedFulfills).groupBy(_.theirAdd.paymentHash)
    allInfos = (paymentInfos ++ relayedPreimageInfos ++ txInfos ++ delayedRefunds).toList.sortBy(_.seenAt)(Ordering[Long].reverse)
  }

  def loadRecent: Unit =
    WalletApp.txDataBag.db.txWrap {
      reloadRelayedPreimageInfos
      reloadPaymentInfos
      reloadTxInfos
      updAllInfos
    }

  def loadSearch(query: String): Unit = WalletApp.txDataBag.db.txWrap {
    txInfos = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
    paymentInfos = LNParams.cm.payBag.searchPayments(query).map(LNParams.cm.payBag.toPaymentInfo)
    relayedPreimageInfos = Nil
    updAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    def work(query: String): Observable[Unit] = Rx.ioQueue.map(_ => if (query.nonEmpty) loadSearch(query) else loadRecent)
    def process(query: String, searchLoadResultEffect: Unit): Unit = UITask(updatePaymentList).run
  }

  var lastSeenViewHolders = Set.empty[PaymentLineViewHolder]
  var lastSeenInChannelOutgoing = Map.empty[FullPaymentTag, OutgoingAdds]

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def notifyDataSetChanged: Unit = {
      lastSeenViewHolders = Set.empty[PaymentLineViewHolder]
      lastSeenInChannelOutgoing = LNParams.cm.allInChannelOutgoing
      super.notifyDataSetChanged
    }

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]
      holder.currentDetails = getItem(position)
      lastSeenViewHolders += holder
      holder.updDetails
      holder.updMeta
      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val cardContainer: LinearLayout = itemView.findViewById(R.id.cardContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = itemView.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = itemView.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = itemView.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val labelIcon: ImageView = itemView.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = itemView.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = itemView.findViewById(R.id.meta).asInstanceOf[TextView]
    itemView.setTag(this)

    private val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(itemView.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: TransactionDetails = _
    private var lastVisibleIconId: Int = -1

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    def updDetails: Unit =
      currentDetails match {
        case info: RelayedPreimageInfo =>
          labelIcon setVisibility View.GONE
          detailsAndStatus setVisibility View.GONE
          amount setText LNParams.denomination.directedWithSign(info.earned, 0L.msat, cardOut, cardIn, cardZero, isPlus = true).html
          cardContainer setBackgroundResource paymentBackground(info.fullTag)
          setVisibleIcon(id = R.id.lnRouted)

        case info: TxInfo =>
          detailsAndStatus setVisibility View.VISIBLE
          description setText txDescription(info).html
          setVis(info.description.label.isDefined, labelIcon)
          amount setText LNParams.denomination.directedWithSign(info.receivedSat, info.sentSat, cardOut, cardIn, cardZero, info.isIncoming).html
          cardContainer setBackgroundResource chainTxBackground(info)
          statusIcon setImageResource txStatusIcon(info)
          setTxTypeIcon(info)

        case info: PaymentInfo =>
          detailsAndStatus setVisibility View.VISIBLE
          description setText paymentDescription(info).html
          setVis(info.description.label.isDefined, labelIcon)
          amount setText LNParams.denomination.directedWithSign(info.received, info.sent, cardOut, cardIn, cardZero, info.isIncoming).html
          cardContainer setBackgroundResource paymentBackground(info.fullTag)
          statusIcon setImageResource paymentStatusIcon(info)
          setPaymentTypeIcon(info)

        case info: DelayedRefunds =>
          labelIcon setVisibility View.GONE
          description setText delayed_refund
          detailsAndStatus setVisibility View.VISIBLE
          amount setText LNParams.denomination.directedWithSign(info.totalAmount, 0L.msat, cardIn, cardIn, cardZero, isPlus = true).html
          cardContainer setBackgroundResource R.drawable.border_lite_gray
          statusIcon setImageResource R.drawable.baseline_feedback_24
          setVisibleIcon(id = R.id.lnBtc)
      }

    def updMeta: Unit =
      currentDetails match {
        case _: DelayedRefunds => meta setText getString(delayed_pending).html
        case info: RelayedPreimageInfo => meta setText WalletApp.app.when(info.date).html

        case info: TxInfo =>
          if (info.isDeeplyBuried) meta setText WalletApp.app.when(info.date).html
          else if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
          else if (info.depth > 0) meta setText getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks).html
          else meta setText getString(tx_state_unconfirmed).html

        case info: PaymentInfo if info.isIncoming =>
          val valueHuman = LNParams.cm.inProcessors.get(info.fullTag).map(info.receivedRatio).map(WalletApp.app plurOrZero pctCollected)
          if (PaymentStatus.SUCCEEDED == info.status && valueHuman.isDefined) meta setText pctCollected.last.html // Notify user that we are not exactly done yet
          else if (PaymentStatus.SUCCEEDED == info.status) meta setText WalletApp.app.when(info.date).html // Payment has been cleared in channels, show timestamp
          else meta setText valueHuman.getOrElse(pctCollected.head).html // Show either value collected so far or that we are still waiting

        case info: PaymentInfo =>
          val currentInChannel = lastSeenInChannelOutgoing.getOrElse(info.fullTag, Nil)
          val isActive = PaymentStatus.PENDING == info.status || currentInChannel.nonEmpty
          if (isActive) meta setText WalletApp.app.plurOrZero(partsInFlight)(currentInChannel.size).html // Show either number of parts or that we are still preparing
          else meta setText WalletApp.app.when(info.date).html // Payment has either succeeded or failed AND no leftovers are present in FSM, show timestamp
      }

    private def txDescription(info: TxInfo): String = info.description match {
      case _: ChanRefundingTxDescription => getString(tx_description_refund)
      case _: HtlcClaimTxDescription => getString(tx_description_htlc_claim)
      case _: ChanFundingTxDescription => getString(tx_description_funding)
      case _: OpReturnTxDescription => getString(tx_description_op_return)
      case _: PenaltyTxDescription => getString(tx_description_penalty)
      case plain: PlainTxDescription => labelOrFallback(plain)
    }

    private def chainTxBackground(info: TxInfo): Int = info.description match {
      case _: HtlcClaimTxDescription if info.depth <= 0L => R.drawable.border_yellow
      case _: PenaltyTxDescription if info.depth <= 0L => R.drawable.border_yellow
      case _ => R.drawable.border_dark_gray
    }

    private def labelOrFallback(plainTxDescription: PlainTxDescription): String = plainTxDescription.label.getOrElse {
      val htmlOpt = plainTxDescription.addresses.headOption.map(_.shortAddress).map(short => s"&#160;<font color=$cardZero>$short</font>")
      getString(tx_btc) + htmlOpt.getOrElse(new String)
    }

    private def setTxTypeIcon(info: TxInfo): Unit = info.description match {
      case _: PlainTxDescription if info.isIncoming => setVisibleIcon(id = R.id.btcIncoming)
      case _: OpReturnTxDescription => setVisibleIcon(id = R.id.btcOutgoing)
      case _: ChanRefundingTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: ChanFundingTxDescription => setVisibleIcon(id = R.id.btcLn)
      case _: HtlcClaimTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: PenaltyTxDescription => setVisibleIcon(id = R.id.lnBtc)
      case _: PlainTxDescription =>
        // See WalletApp.WalletEventsListener.onTransactionReceived for explanation
        setVis(view = iconMap(R.id.btcOutgoingNormal), isVisible = info.sentSat != info.receivedSat)
        setVis(view = iconMap(R.id.btcOutgoingToSelf), isVisible = info.sentSat == info.receivedSat)
        setVisibleIcon(id = R.id.btcOutgoing)
    }

    private def txStatusIcon(info: TxInfo): Int =
      if (info.isDeeplyBuried) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24

    // LN helpers

    private def paymentDescription(info: PaymentInfo): String = info.description.split match {
      case Some(split) => lnSplitNotice.format(split.sentRatio) + info.description.finalDescription.getOrElse(lnDefTitle)
      case None => info.description.finalDescription.getOrElse(lnDefTitle)
    }

    private def setPaymentTypeIcon(info: PaymentInfo): Unit =
      if (info.isIncoming) setVisibleIcon(id = R.id.lnIncoming)
      else setOutgoingPaymentIcons(info)

    private def setOutgoingPaymentIcons(info: PaymentInfo): Unit = {
      setVis(view = iconMap(R.id.lnOutgoingAction), isVisible = info.actionString != PaymentInfo.NO_ACTION)
      setVis(view = iconMap(R.id.lnOutgoingBasic), isVisible = info.actionString == PaymentInfo.NO_ACTION)
      setVisibleIcon(id = R.id.lnOutgoing)
    }

    private def paymentStatusIcon(info: PaymentInfo): Int =
      if (PaymentStatus.SUCCEEDED == info.status) R.drawable.baseline_done_24
      else if (PaymentStatus.ABORTED == info.status) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24

    private def paymentBackground(fullTag: FullPaymentTag): Int =
      if (ChannelMaster.dangerousHCRevealed(hashToReveals, LNParams.blockCount.get, fullTag.paymentHash).nonEmpty) R.drawable.border_red
      else if (LNParams.cm.opm.data.payments contains fullTag) R.drawable.border_blue // An active outgoing FSM is present for this tag
      else if (lastSeenInChannelOutgoing contains fullTag) R.drawable.border_blue // Payments in channel are present for this tag
      else if (LNParams.cm.inProcessors contains fullTag) R.drawable.border_blue // An active incoming FSM exists for this tag
      else R.drawable.border_dark_gray
  }

  // LIST CAPTION CLASS

  class WalletCardsViewHolder {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_wallet_cards, null).asInstanceOf[LinearLayout]
    val defaultHeader: LinearLayout = view.findViewById(R.id.defaultHeader).asInstanceOf[LinearLayout]
    val rateTeaser: TextView = view.findViewById(R.id.rateTeaser).asInstanceOf[TextView]

    val totalBalance: TextView = view.findViewById(R.id.totalBalance).asInstanceOf[TextView]
    val totalFiatBalance: TextView = view.findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
    val fiatUnitPriceAndChange: TextView = view.findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]

    val totalBitcoinBalance: TextView = view.findViewById(R.id.totalBitcoinBalance).asInstanceOf[TextView]
    val receiveBitcoinTip: ImageView = view.findViewById(R.id.receiveBitcoinTip).asInstanceOf[ImageView]
    val offlineIndicator: TextView = view.findViewById(R.id.offlineIndicator).asInstanceOf[TextView]
    val btcSyncIndicator: TextView = view.findViewById(R.id.btcSyncIndicator).asInstanceOf[TextView]

    val totalLightningBalance: TextView = view.findViewById(R.id.totalLightningBalance).asInstanceOf[TextView]
    val channelStateIndicators: LinearLayout = view.findViewById(R.id.channelStateIndicators).asInstanceOf[LinearLayout]
    val channelIndicator: ChannelIndicatorLine = view.findViewById(R.id.channelIndicator).asInstanceOf[ChannelIndicatorLine]

    val inFlightIncoming: TextView = view.findViewById(R.id.inFlightIncoming).asInstanceOf[TextView]
    val inFlightOutgoing: TextView = view.findViewById(R.id.inFlightOutgoing).asInstanceOf[TextView]
    val inFlightRouted: TextView = view.findViewById(R.id.inFlightRouted).asInstanceOf[TextView]
    val addChannelTip: ImageView = view.findViewById(R.id.addChannelTip).asInstanceOf[ImageView]

    val listCaption: RelativeLayout = view.findViewById(R.id.listCaption).asInstanceOf[RelativeLayout]
    val paymentHistory: MaterialButton = view.findViewById(R.id.paymentHistory).asInstanceOf[MaterialButton]
    val routedPayments: MaterialButton = view.findViewById(R.id.routedPayments).asInstanceOf[MaterialButton]
    val paymentLinks: MaterialButton = view.findViewById(R.id.paymentLinks).asInstanceOf[MaterialButton]
    val searchWrap: RelativeLayout = view.findViewById(R.id.searchWrap).asInstanceOf[RelativeLayout]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]

    def updateFiatRates: Unit = {
      val change = LNParams.fiatRates.info.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
    }

    def updateView: Unit = lnBalance match { case currentLnBalance =>
      val walletBalance = currentLnBalance + WalletApp.lastChainBalance.totalBalance
      val allChannels = LNParams.cm.all.values.take(8)

      TransitionManager.beginDelayedTransition(view)
      setVis(allChannels.nonEmpty, channelStateIndicators)
      setVis(WalletApp.lastChainBalance.isTooLongAgo, btcSyncIndicator)
      totalFiatBalance setText WalletApp.currentMsatInFiatHuman(walletBalance).html
      totalBalance setText LNParams.denomination.parsedWithSign(walletBalance, cardIn, totalZero).html
      channelIndicator.createIndicators(allChannels.toArray)

      totalLightningBalance setText LNParams.denomination.parsedWithSign(currentLnBalance, cardIn, lnCardZero).html
      totalBitcoinBalance setText LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, cardIn, btcCardZero).html
      setVis(WalletApp.lastChainBalance.totalBalance != 0L.msat, totalBitcoinBalance)
      setVis(WalletApp.lastChainBalance.totalBalance == 0L.msat, receiveBitcoinTip)
      setVis(allChannels.nonEmpty, totalLightningBalance)
      setVis(allChannels.isEmpty, addChannelTip)

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
    }
  }

  // LISTENERS

  private var stateSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]
  private var paymentSubscription = Option.empty[Subscription]
  private var preimageSubscription = Option.empty[Subscription]
  private var unknownReestablishSubscription = Option.empty[Subscription]
  private var lnPaymentAddedSubscription = Option.empty[Subscription]
  private var payLinkAddedSubscription = Option.empty[Subscription]
  private var chainTxAddedSubscription = Option.empty[Subscription]
  private var relayAddedSubscription = Option.empty[Subscription]

  private val netListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      // This will make channels SLEEPING right away instead of a bit later when we receive no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
      setVis(!isConnected, walletCards.offlineIndicator)
    }.run
  }

  private val chainListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = UITask {
      // First, refresh bitcoin card balance which might have been updated by chain txs while we were offline
      // Second, update payments to highlight nearly expired revealed incoming now that chain tip it known
      // Third, check if any of unconfirmed chain transactions became confirmed or double-spent
      walletCards.updateView
      updatePaymentList

      for {
        transactionInfo <- txInfos if !transactionInfo.isDeeplyBuried && !transactionInfo.isDoubleSpent
        (newDepth, newDoubleSpent) <- LNParams.chainWallet.wallet.doubleSpent(transactionInfo.tx)
        if newDepth != transactionInfo.depth || newDoubleSpent != transactionInfo.isDoubleSpent
        _ = WalletApp.txDataBag.updStatus(transactionInfo.txid, newDepth, newDoubleSpent)
        // Simulate preimage revealed using a txid to throttle multiple vibrations
      } ChannelMaster.hashRevealStream.onNext(transactionInfo.txid)
    }.run
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
      _ => readEmptyNdefMessage)(_ => me checkExternalData noneRunnable)

  // Channel errors

  private val MAX_ERROR_COUNT_WITHIN_WINDOW = 4
  private val channelErrors = CacheBuilder.newBuilder.expireAfterAccess(30, TimeUnit.SECONDS).maximumSize(500).build[ByteVector32, JInt]

  def chanUnknown(unknown: UnknownReestablish): Unit = UITask {
    def closeAndBreak(alert: AlertDialog): Unit = runAnd(alert.dismiss)(unknown.requestClose)
    val errorCount = Option(channelErrors getIfPresent unknown.reestablish.channelId).getOrElse(default = 0: JInt)
    val msg = getString(error_channel_unknown).format(unknown.reestablish.channelId.toHex, unknown.worker.info.nodeId.toString)
    val builder = new AlertDialog.Builder(me).setCustomTitle(getString(error_channel).asDefView).setCancelable(true).setMessage(msg.html)
    if (errorCount < MAX_ERROR_COUNT_WITHIN_WINDOW) mkCheckFormNeutral(_.dismiss, share(msg), closeAndBreak, builder, dialog_ok, dialog_share, dialog_break)
    channelErrors.put(unknown.reestablish.channelId, errorCount + 1)
  }.run

  def chanError(chanId: ByteVector32, msg: String, info: RemoteNodeInfo): Unit = UITask {
    val errorCount = Option(channelErrors getIfPresent chanId).getOrElse(default = 0: JInt)
    val builder = new AlertDialog.Builder(me).setCustomTitle(getString(error_channel).asDefView).setMessage(msg.html)
    if (errorCount < MAX_ERROR_COUNT_WITHIN_WINDOW) mkCheckForm(_.dismiss, share(msg), builder, dialog_ok, dialog_share)
    channelErrors.put(chanId, errorCount + 1)
  }.run

  override def onException: PartialFunction[Malfunction, Unit] = {
    case (error: ChannelTransitionFail, _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_closed).format(error.stackTraceAsString), data.commitments.remoteInfo)
    case (error: ChannelTransitionFail, _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_suspended).format(error.stackTraceAsString), hc.remoteInfo)
    case (RemoteErrorException(details), _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_remote).format(details), data.commitments.remoteInfo)
    case (RemoteErrorException(details), _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_remote).format(details), hc.remoteInfo)
    case (CMDException(reason, _: CMD_CLOSE), _, data: HasNormalCommitments) => chanError(data.channelId, reason, data.commitments.remoteInfo)
    case (CMDException(reason, _: CMD_HOSTED_STATE_OVERRIDE), _, hc: HostedCommits) => chanError(hc.channelId, reason, hc.remoteInfo)
    case (error, _, data: HasNormalCommitments) => chanError(data.channelId, error.stackTraceAsString, data.commitments.remoteInfo)
    case (error, _, hc: HostedCommits) => chanError(hc.channelId, error.stackTraceAsString, hc.remoteInfo)
  }

  // Lifecycle methods

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def onDestroy: Unit = {
    stateSubscription.foreach(_.unsubscribe)
    statusSubscription.foreach(_.unsubscribe)
    paymentSubscription.foreach(_.unsubscribe)
    preimageSubscription.foreach(_.unsubscribe)
    unknownReestablishSubscription.foreach(_.unsubscribe)
    lnPaymentAddedSubscription.foreach(_.unsubscribe)
    payLinkAddedSubscription.foreach(_.unsubscribe)
    chainTxAddedSubscription.foreach(_.unsubscribe)
    relayAddedSubscription.foreach(_.unsubscribe)

    LNParams.chainWallet.eventsCatcher ! WalletEventsCatcher.Remove(chainListener)
    for (channel <- LNParams.cm.all.values) channel.listeners -= me
    LNParams.fiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def onBackPressed: Unit = {
    if (itemsList.getFirstVisiblePosition > 0) itemsList.smoothScrollToPositionFromTop(0, 1)
    else if (walletCards.searchWrap.getVisibility == View.VISIBLE) cancelSearch(null)
    else if (currentSnackbar.isDefined) removeCurrentSnack.run
    else super.onBackPressed
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case bitcoinUri: BitcoinUri if bitcoinUri.isValid => bringSendBitcoinPopup(bitcoinUri)

    case info: RemoteNodeInfo =>
      LNParams.cm.all.values.foreach(_ process info)
      me goTo ClassNames.remotePeerActivityClass

    case prExt: PaymentRequestExt =>
      lnSendGuard(prExt, contentWindow) {
        case Some(origAmount) if prExt.splits.nonEmpty =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(prExt.splitLeftover * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < prExt.splitLeftover - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= prExt.splitLeftover && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)

            override def send(alert: AlertDialog): Unit = {
              val cmd = LNParams.cm.makeSendCmd(prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(origAmount)
              replaceOutgoingPayment(prExt, PlainDescription(cmd.split.asSome, label = manager.resultExtraInput, invoiceText = prExt.descriptionOrEmpty), action = None, sentAmount = cmd.split.myPart)
              LNParams.cm.localSend(cmd)
              alert.dismiss
            }

            override val alert: AlertDialog = {
              val title = new TitleView(getString(dialog_split_ln) format prExt.brDescription)
              val leftHuman = LNParams.denomination.parsedWithSign(prExt.splitLeftover, cardIn, cardZero)
              val totalHuman = LNParams.denomination.parsedWithSign(origAmount, cardIn, cardZero)
              title.addChipText(getString(dialog_ln_requested) format s"&#160;$totalHuman")
              title.addChipText(getString(dialog_ln_left) format s"&#160;$leftHuman")

              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              mkCheckFormNeutral(send, none, neutral, builder, dialog_pay, dialog_cancel, dialog_split)
            }

            // Prefill with what's left to pay
            manager.updateText(prExt.splitLeftover)
          }

        case Some(origAmount) =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(origAmount * 2), minSendable = LNParams.minPayment) {
            override def isNeutralEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat < origAmount - minSendable
            override def isPayEnabled: Boolean = manager.resultMsat >= origAmount && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = proceedSplit(prExt, origAmount, alert)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)

            override val alert: AlertDialog = {
              val title = new TitleView(getString(dialog_send_ln) format prExt.brDescription)
              val totalHuman = LNParams.denomination.parsedWithSign(origAmount, cardIn, cardZero)
              title.addChipText(getString(dialog_ln_requested) format s"&#160;$totalHuman")

              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              mkCheckFormNeutral(send, none, neutral, builder, dialog_pay, dialog_cancel, dialog_split)
            }

            // Prefill with asked amount
            manager.updateText(origAmount)
          }

        case None =>
          new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values), minSendable = LNParams.minPayment) {
            override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable
            override def neutral(alert: AlertDialog): Unit = manager.updateText(maxSendable)
            override def send(alert: AlertDialog): Unit = baseSendNow(prExt, alert)
            override def isNeutralEnabled: Boolean = true

            override val alert: AlertDialog = {
              val title = getString(dialog_send_ln).format(prExt.brDescription).asColoredView(R.color.cardLightning)
              mkCheckFormNeutral(send, none, neutral, titleBodyAsViewBuilder(title, manager.content), dialog_pay, dialog_cancel, dialog_max)
            }

            // Do not prefill since amount is unknown, disable pay button
            manager.updateButton(getPositiveButton(alert), isEnabled = false)
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
      case nc: NormalChannelRequest => runAnd { InputParser.value = nc } { me goTo ClassNames.remotePeerActivityClass }
      case hc: HostedChannelRequest => runAnd { InputParser.value = hc } { me goTo ClassNames.remotePeerActivityClass }
      case _ => UITask(WalletApp.app quickToast error_nothing_useful).run
    }

    val msg = getString(dialog_lnurl_processing).format(lnUrl.uri.getHost).html
    val obs = lnUrl.level1DataResponse.doOnTerminate(removeCurrentSnack.run)
    cancellingSnack(contentWindow, obs.subscribe(resolve, onFail), msg)
  }

  def showAuthForm(lnUrl: LNUrl): Unit = lnUrl.k1.foreach { k1 =>
    val linkingPrivKey = LNParams.secret.keys.makeLinkingKey(lnUrl.uri.getHost)
    val linkingPubKey = linkingPrivKey.publicKey.toString
    val dataToSign = ByteVector32.fromValidHex(k1)

    val (successRes, actionRes) = lnUrl.authAction match {
      case "register" => (lnurl_auth_register_ok, lnurl_auth_register)
      case "auth" => (lnurl_auth_auth_ok, lnurl_auth_auth)
      case "link" => (lnurl_auth_link_ok, lnurl_auth_link)
      case _ => (lnurl_auth_login_ok, lnurl_auth_login)
    }

    val title = titleBodyAsViewBuilder(s"<big>${lnUrl.uri.getHost}</big>".asColoredView(R.color.cardLightning), null)
    mkCheckFormNeutral(doAuth, none, displayInfo, title, actionRes, dialog_cancel, dialog_info)

    def displayInfo(alert: AlertDialog): Unit = {
      val explanation = getString(lnurl_auth_info).format(lnUrl.uri.getHost, linkingPubKey.humanFour).html
      mkCheckFormNeutral(_.dismiss, none, _ => share(linkingPubKey), new AlertDialog.Builder(me).setMessage(explanation), dialog_ok, -1, dialog_share)
    }

    def doAuth(alert: AlertDialog): Unit = {
      val signature = Crypto.sign(dataToSign, linkingPrivKey)
      val msg = getString(dialog_lnurl_processing).format(lnUrl.uri.getHost).html
      val uri = lnUrl.uri.buildUpon.appendQueryParameter("sig", Crypto.compact2der(signature).toHex).appendQueryParameter("key", linkingPubKey)
      val sub = LNUrl.level2DataResponse(uri).doOnTerminate(removeCurrentSnack.run).subscribe(_ => UITask(WalletApp.app quickToast successRes).run, onFail)
      cancellingSnack(contentWindow, sub, msg)
      alert.dismiss
    }
  }

  override def onChoiceMade(tag: String, pos: Int): Unit = (tag, pos) match {
    case (CHOICE_RECEIVE_TAG, 0) => me goTo ClassNames.qrChainActivityClass
    case (CHOICE_RECEIVE_TAG, 1) => bringReceivePopup
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      for (channel <- LNParams.cm.all.values) channel.listeners += me
      LNParams.chainWallet.eventsCatcher ! chainListener
      LNParams.fiatRates.listeners += fiatRatesListener
      Tovuti.from(me).monitor(netListener)

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      runInFutureProcessOnUI(loadRecent, none) { _ =>
        // We suggest user to rate us if: no rate attempt has been made before, LN payments were successful, user has been using an app for certain period
        setVis(WalletApp.showRateUs && paymentInfos.forall(_.status == PaymentStatus.SUCCEEDED) && allInfos.size > 4 && allInfos.size < 8, walletCards.rateTeaser)
        walletCards.searchField addTextChangedListener onTextChange(searchWorker.addWork)
        updatePaymentList
        updateToggleMenu
      }

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

      preimageSubscription = ChannelMaster.remoteFulfillStream.subscribe(resolveAction, none).asSome
      stateSubscription = txEvents.merge(paymentEvents).merge(relayEvents).doOnNext(_ => updAllInfos).merge(stateEvents).subscribe(_ => UITask(updatePaymentList).run).asSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).asSome
      paymentSubscription = ChannelMaster.hashRevealStream.merge(ChannelMaster.remoteFulfillStream).throttleFirst(window).subscribe(_ => Vibrator.vibrate).asSome
      unknownReestablishSubscription = ChannelMaster.unknownReestablishStream.subscribe(chanUnknown, none).asSome

      // Start with empty sets and update them once a unique related record is added to db
      lnPaymentAddedSubscription = ChannelMaster.lnPaymentAddedStream.doOnNext(lnPaymentsAdded += _).subscribe(_ => UITask(updateToggleMenu).run, none).asSome
      payLinkAddedSubscription = ChannelMaster.payLinkAddedStream.doOnNext(payLinksAdded += _).subscribe(_ => UITask(updateToggleMenu).run, none).asSome
      chainTxAddedSubscription = ChannelMaster.chainTxAddedStream.doOnNext(chainTxsAdded += _).subscribe(_ => UITask(updateToggleMenu).run, none).asSome
      relayAddedSubscription = ChannelMaster.relayAddedStream.doOnNext(relaysAdded += _).subscribe(_ => UITask(updateToggleMenu).run, none).asSome

      // Run this check after establishing subscriptions since it will trigger an event stream
      timer.scheduleAtFixedRate(UITask { for (holder <- lastSeenViewHolders) holder.updMeta }, 30000, 30000)
      markAsFailedOnce
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringRateDialog(view: View): Unit = {
    val marketUri = Uri.parse(s"market://details?id=$getPackageName")
    WalletApp.app.prefs.edit.putBoolean(WalletApp.SHOW_RATE_US, false)
    me startActivity new Intent(Intent.ACTION_VIEW, marketUri)
    view.setVisibility(View.GONE)
  }

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
    val manager = new RateManager(body, getString(dialog_add_btc_memo).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    val canSend = LNParams.denomination.parsedWithSign(WalletApp.lastChainBalance.totalBalance, cardIn, cardZero)
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
      val builder = titleBodyAsViewBuilder(getString(dialog_send_btc).format(uri.address.shortAddress, label + message).asColoredView(R.color.cardBitcoin), manager.content)
      if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, _ => manager.updateText(WalletApp.lastChainBalance.totalBalance), builder, dialog_pay, dialog_cancel, neutralRes)
      else mkCheckFormNeutral(attempt, none, switchToLn, builder, dialog_pay, dialog_cancel, lightning_wallet)
    }

    lazy val feeView = new FeeView(body) {
      override def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = UITask {
        manager.updateButton(getPositiveButton(alert), feeOpt.isDefined)
        super.update(feeOpt, showIssue)
      }.run

      rate = {
        val target = LNParams.feeRates.info.onChainFeeConf.feeTargets.mutualCloseBlockTarget
        LNParams.feeRates.info.onChainFeeConf.feeEstimator.getFeeratePerKw(target)
      }
    }

    lazy val worker = new ThrottledWork[Satoshi, TxAndFee] {
      def work(amount: Satoshi): Observable[TxAndFee] = Rx fromFutureOnIo LNParams.chainWallet.wallet.sendPayment(amount, uri.address, feeView.rate)
      def process(amount: Satoshi, txAndFee: TxAndFee): Unit = feeView.update(feeOpt = Some(txAndFee.fee.toMilliSatoshi), showIssue = false)
      override def error(exc: Throwable): Unit = feeView.update(feeOpt = None, showIssue = manager.resultSat >= LNParams.minDustLimit)
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

    manager.hintDenom.setText(getString(dialog_can_send).format(canSend).html)
    manager.hintFiatDenom.setText(getString(dialog_can_send).format(canSendFiat).html)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount.foreach { asked =>
      manager.updateText(value = asked)
      manager.inputAmount.setEnabled(false)
      manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringReceivePopup: Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat, lnBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainDescription(split = None, label = None, invoiceText = manager.resultExtraInput getOrElse new String)
      override def processInvoice(prExt: PaymentRequestExt): Unit = runAnd(InputParser.value = prExt)(me goTo ClassNames.qrInvoiceActivityClass)
      override def getTitleText: String = getString(dialog_receive_ln)
    }
  }

  def bringWithdrawPopup(data: WithdrawRequest): Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = data.maxWithdrawable.msat, initMinReceivable = data.minCanReceive, lnBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_ln_memo).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainMetaDescription(split = None, label = manager.resultExtraInput, invoiceText = new String, meta = data.descriptionOrEmpty)
      override def getTitleText: String = getString(dialog_lnurl_withdraw).format(data.callbackUri.getHost, data.brDescription)
      override def processInvoice(prExt: PaymentRequestExt): Unit = data.requestWithdraw(prExt).foreach(none, onFail)
    }
  }

  def bringPayPopup(data: PayRequest): Unit =
    new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values) min data.maxSendable.msat, minSendable = LNParams.minPayment max data.minSendable.msat) {
      override def isNeutralEnabled: Boolean = manager.resultMsat >= LNParams.minPayment && manager.resultMsat <= minSendable - LNParams.minPayment
      override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable

      override val manager: RateManager = {
        val commentStringOpt = if (data.commentAllowed.isDefined) getString(dialog_add_comment).asSome else None
        new RateManager(body, commentStringOpt, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      }

      override def neutral(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(minSendable)
            InputParser.value = SplitParams(pf.prExt, pf.successAction, PlainMetaDescription(cmd.split.asSome, label = None, invoiceText = new String, meta = data.metaDataTextPlain), cmd, typicalChainTxFee)
            me goTo ClassNames.qrSplitActivityClass
            alert.dismiss
          }
        }

        val obs = getFinal(minSendable).doOnTerminate(removeCurrentSnack.run)
        val msg = getString(dialog_lnurl_splitting).format(data.callbackUri.getHost).html
        cancellingSnack(contentWindow, obs.subscribe(pf => proceed(pf).run, onFail), msg)
      }

      override def send(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(manager.resultMsat)
            replaceOutgoingPayment(pf.prExt, PlainMetaDescription(split = None, label = None, invoiceText = new String, meta = data.metaDataTextPlain), pf.successAction, sentAmount = cmd.split.myPart)
            LNParams.cm.localSend(cmd)
            alert.dismiss
          }
        }

        val obs = getFinal(manager.resultMsat).doOnTerminate(removeCurrentSnack.run)
        val amountHuman = LNParams.denomination.parsedWithSign(manager.resultMsat, cardIn, cardZero).html
        val msg = getString(dialog_lnurl_sending).format(amountHuman, data.callbackUri.getHost).html
        cancellingSnack(contentWindow, obs.subscribe(pf => proceed(pf).run, onFail), msg)
      }

      override val alert: AlertDialog = {
        val text = getString(dialog_lnurl_pay).format(data.callbackUri.getHost, s"<br><br>${data.metaDataTextPlain}")
        val title = titleBodyAsViewBuilder(text.asColoredView(R.color.cardLightning), manager.content)
        mkCheckFormNeutral(send, none, neutral, title, dialog_pay, dialog_cancel, dialog_split)
      }

      private def getFinal(amount: MilliSatoshi) =
        data.requestFinal(manager.resultExtraInput, amount).map { rawResponse =>
          val payRequestFinal: PayRequestFinal = to[PayRequestFinal](rawResponse)
          val descriptionHashOpt: Option[ByteVector32] = payRequestFinal.prExt.pr.description.right.toOption
          require(descriptionHashOpt.contains(data.metaDataHash), s"Metadata hash mismatch, original=${data.metaDataHash}, provided=$descriptionHashOpt")
          require(payRequestFinal.prExt.pr.amount.contains(amount), s"Payment amount mismatch, requested=$amount, provided=${payRequestFinal.prExt.pr.amount}")
          for (additionalEdge <- payRequestFinal.additionalRoutes) LNParams.cm.pf process additionalEdge
          payRequestFinal.modify(_.successAction.each.domain).setTo(data.callbackUri.getHost.asSome)
        }

      // Prefill with min possible
      manager.updateText(minSendable)
    }

  def updatePaymentList: Unit = {
    setVis(allInfos.nonEmpty, walletCards.listCaption)
    paymentsAdapter.notifyDataSetChanged
  }

  def updateToggleMenu: Unit = {
    val paymentHistory = getString(payments)
    val localPayments = lnPaymentsAdded.size + chainTxsAdded.size
    walletCards.paymentHistory setText { if (localPayments > 0) s"$paymentHistory +$localPayments" else paymentHistory }
    walletCards.paymentLinks setText { if (payLinksAdded.nonEmpty) s"+${payLinksAdded.size}" else new String }
    walletCards.routedPayments setText { if (relaysAdded.nonEmpty) s"+${relaysAdded.size}" else new String }
    setVis(relayedPreimageInfos.nonEmpty, walletCards.routedPayments)
    setVis(payLinkInfos.nonEmpty, walletCards.paymentLinks)
  }

  // Payment actions

  def resolveAction(fulfill: RemoteFulfill): Unit =
    paymentInfos.find(_.paymentHash == fulfill.ourAdd.paymentHash).flatMap(_.action).foreach { action =>
      def executeAction: Unit = showPaymentAction(action, fulfill.theirPreimage)
      UITask(executeAction).run
    }

  def showPaymentAction(action: PaymentAction, preimage: ByteVector32): Unit = action match {
    case data: MessageAction => mkCheckFormNeutral(_.dismiss, none, _ => share(data.message), actionPopup(data.finalMessage.html, data), dialog_ok, dialog_cancel, dialog_share)
    case data: UrlAction => mkCheckFormNeutral(_ => browse(data.url), none, _ => share(data.url), actionPopup(data.finalMessage.html, data), dialog_open, dialog_cancel, dialog_share)
    case data: AESAction =>
      decodeAesAction(preimage, data) match {
        case Success(secret ~ msg) => mkCheckFormNeutral(_.dismiss, none, _ => share(secret), actionPopup(msg, data), dialog_ok, dialog_cancel, dialog_share)
        case _ => mkCheckForm(_.dismiss, none, actionPopup(getString(dialog_lnurl_decrypt_fail), data), dialog_ok, -1)
      }
  }

  def actionPopup(msg: CharSequence, action: PaymentAction): AlertDialog.Builder = {
    val fromVendor = action.domain.map(site => s"<br><br><b>$site</b>").getOrElse(new String)
    val title = getString(dialog_lnurl_from_vendor).format(fromVendor).asDefView
    new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg)
  }

  private def decodeAesAction(preimage: ByteVector32, aes: AESAction) = Try {
    val secret = new String(AES.decode(data = aes.ciphertextBytes, key = preimage.toArray, initVector = aes.ivBytes).toArray, "UTF-8")
    val msg = if (secret.length > 36) s"${aes.finalMessage}<br><br><tt>$secret</tt><br>" else s"${aes.finalMessage}<br><br><tt><big>$secret</big></tt><br>"
    (secret, msg.html)
  }
}
