package com.btcontract.wallettest

import immortan._
import immortan.utils._
import android.widget._
import fr.acinq.eclair._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import com.softwaremill.quicklens._
import scala.collection.JavaConverters._
import com.btcontract.wallettest.Colors._
import com.btcontract.wallettest.R.string._
import immortan.utils.ImplicitJsonFormats._
import com.btcontract.wallettest.HubActivity._

import java.lang.{Integer => JInt}
import immortan.sqlite.{SQLiteData, Table}
import android.view.{MenuItem, View, ViewGroup}
import rx.lang.scala.{Observable, Subscription}
import android.graphics.{Bitmap, BitmapFactory}
import com.androidstudy.networkmanager.{Monitor, Tovuti}
import fr.acinq.eclair.wire.{FullPaymentTag, PaymentTagTlv}
import com.google.common.cache.{CacheBuilder, LoadingCache}
import immortan.ChannelMaster.{OutgoingAdds, RevealedLocalFulfills}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, FeeratePerVByte}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import com.google.android.material.button.{MaterialButton, MaterialButtonToggleGroup}
import com.google.android.material.button.MaterialButtonToggleGroup.OnButtonCheckedListener
import com.google.android.material.textfield.TextInputLayout
import com.chauthai.swipereveallayout.SwipeRevealLayout
import com.btcontract.wallettest.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import com.btcontract.wallettest.utils.LocalBackup
import fr.acinq.eclair.transactions.RemoteFulfill
import com.github.mmin18.widget.RealtimeBlurView
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.slider.Slider
import androidx.transition.TransitionManager
import immortan.ChannelListener.Malfunction
import fr.acinq.eclair.blockchain.TxAndFee
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import android.content.pm.PackageManager
import com.ornach.nobobutton.NoboButton
import java.util.concurrent.TimeUnit
import android.content.Intent
import org.ndeftools.Message
import java.util.TimerTask
import android.os.Bundle
import android.net.Uri
import scala.util.Try


object HubActivity {
  var txInfos: Iterable[TxInfo] = Iterable.empty
  var paymentInfos: Iterable[PaymentInfo] = Iterable.empty
  var payMarketInfos: Iterable[PayLinkInfo] = Iterable.empty
  var relayedPreimageInfos: Iterable[RelayedPreimageInfo] = Iterable.empty
  var allInfos: Seq[TransactionDetails] = Nil

  var lastInChannelOutgoing: Map[FullPaymentTag, OutgoingAdds] = Map.empty
  var lastHashToReveals: Map[ByteVector32, RevealedLocalFulfills] = Map.empty
  // Run clear up method once on app start, do not re-run it every time this activity gets restarted
  lazy val markAsFailedOnce: Unit = LNParams.cm.markAsFailed(paymentInfos, LNParams.cm.allInChannelOutgoing)

  def updateLnCaches: Unit = {
    lastInChannelOutgoing = LNParams.cm.allInChannelOutgoing
    lastHashToReveals = LNParams.cm.allIncomingRevealed
  }
}

class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver with ChannelListener { me =>
  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] lazy val walletCards = new WalletCardsViewHolder
  private val CHOICE_RECEIVE_TAG = "choiceReceiveTag"

  private[this] lazy val paymentTypeIconIds =
    List(R.id.btcIncoming, R.id.btcOutgoing, R.id.lnIncoming, R.id.lnOutgoing, R.id.lnRouted, R.id.btcLn,
      R.id.lnBtc, R.id.lnOutgoingBasic, R.id.lnOutgoingAction, R.id.btcOutgoingNormal, R.id.btcOutgoingToSelf)

  private[this] lazy val partsInFlight = getResources.getStringArray(R.array.parts_in_flight)
  private[this] lazy val pctCollected = getResources.getStringArray(R.array.pct_collected)
  private[this] lazy val lnSplitNotice = getString(tx_ln_notice_split)
  private[this] lazy val lnDefTitle = getString(tx_ln)

  // PAYMENT LIST

  def reloadTxInfos: Unit = txInfos = WalletApp.txDataBag.listRecentTxs(Table.DEFAULT_LIMIT.get).map(WalletApp.txDataBag.toTxInfo)
  def reloadPaymentInfos: Unit = paymentInfos = LNParams.cm.payBag.listRecentPayments(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toPaymentInfo)
  def reloadRelayedPreimageInfos: Unit = relayedPreimageInfos = LNParams.cm.payBag.listRecentRelays(Table.DEFAULT_LIMIT.get).map(LNParams.cm.payBag.toRelayedPreimageInfo)
  def reloadPayMarketInfos: Unit = payMarketInfos = WalletApp.payMarketBag.listRecentLinks(Table.DEFAULT_LIMIT.get).map(WalletApp.payMarketBag.toLinkInfo)

  def updAllInfos: Unit = {
    val itemsToDisplayMap = Map(
      R.id.bitcoinPayments -> txInfos,
      R.id.lightningPayments -> paymentInfos,
      R.id.relayedPayments -> relayedPreimageInfos,
      R.id.payMarketLinks -> payMarketInfos
    )

    val checkedIds = walletCards.toggleGroup.getCheckedButtonIds.asScala.map(_.toInt)
    allInfos = DelayedRefunds(LNParams.cm.allDelayedRefundsLeft.map(_.txOut.head.amount).sum.toMilliSatoshi) match {
      case _ if isSearchOn => (txInfos ++ paymentInfos ++ payMarketInfos).toList.sortBy(_.seenAt)(Ordering[Long].reverse)
      case dr if dr.totalAmount > 0L.msat => (checkedIds.flatMap(itemsToDisplayMap) :+ dr).sortBy(_.seenAt)(Ordering[Long].reverse)
      case _ => checkedIds.flatMap(itemsToDisplayMap).sortBy(_.seenAt)(Ordering[Long].reverse)
    }
  }

  def loadRecent: Unit =
    WalletApp.txDataBag.db.txWrap {
      reloadRelayedPreimageInfos
      reloadPayMarketInfos
      reloadPaymentInfos
      reloadTxInfos
      updAllInfos
    }

  def loadSearch(query: String): Unit = WalletApp.txDataBag.db.txWrap {
    txInfos = WalletApp.txDataBag.searchTransactions(query).map(WalletApp.txDataBag.toTxInfo)
    paymentInfos = LNParams.cm.payBag.searchPayments(query).map(LNParams.cm.payBag.toPaymentInfo)
    payMarketInfos = WalletApp.payMarketBag.searchLinks(query).map(WalletApp.payMarketBag.toLinkInfo)
    updAllInfos
  }

  val searchWorker: ThrottledWork[String, Unit] = new ThrottledWork[String, Unit] {
    def work(query: String): Observable[Unit] = Rx.ioQueue.map(_ => if (query.nonEmpty) loadSearch(query) else loadRecent)
    def process(userTypedQuery: String, searchLoadResultEffect: Unit): Unit = paymentAdapterDataChanged.run
  }

  val payLinkImageMemo: LoadingCache[Bytes, Bitmap] = memoize {
    bytes => BitmapFactory.decodeByteArray(bytes, 0, bytes.length)
  }

  val paymentsAdapter: BaseAdapter = new BaseAdapter {
    override def getItem(pos: Int): TransactionDetails = allInfos(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = allInfos.size

    override def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val view = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_payment_line, null) else savedView
      val holder = if (null == view.getTag) new PaymentLineViewHolder(view) else view.getTag.asInstanceOf[PaymentLineViewHolder]
      // Prevent reused layout to appear in open state as user scrolls down
      if (holder.swipeWrap.isOpened) holder.swipeWrap.close(false)
      holder.currentDetails = getItem(position)
      holder.updDetails
      view
    }
  }

  class PaymentLineViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val swipeWrap: SwipeRevealLayout = itemView.asInstanceOf[SwipeRevealLayout]

    val setItemLabel: NoboButton = swipeWrap.findViewById(R.id.setItemLabel).asInstanceOf[NoboButton]
    val removeItem: NoboButton = swipeWrap.findViewById(R.id.removeItem).asInstanceOf[NoboButton]

    val nonLinkContainer: LinearLayout = swipeWrap.findViewById(R.id.nonLinkContainer).asInstanceOf[LinearLayout]
    val detailsAndStatus: RelativeLayout = swipeWrap.findViewById(R.id.detailsAndStatus).asInstanceOf[RelativeLayout]
    val description: TextView = swipeWrap.findViewById(R.id.description).asInstanceOf[TextView]
    val statusIcon: ImageView = swipeWrap.findViewById(R.id.statusIcon).asInstanceOf[ImageView]
    val labelIcon: ImageView = swipeWrap.findViewById(R.id.labelIcon).asInstanceOf[ImageView]
    val amount: TextView = swipeWrap.findViewById(R.id.amount).asInstanceOf[TextView]
    val meta: TextView = swipeWrap.findViewById(R.id.meta).asInstanceOf[TextView]

    val linkContainer: RelativeLayout = swipeWrap.findViewById(R.id.linkContainer).asInstanceOf[RelativeLayout]
    val textMetadata: TextView = swipeWrap.findViewById(R.id.textMetadata).asInstanceOf[TextView]
    val lastAttempt: TextView = swipeWrap.findViewById(R.id.lastAttempt).asInstanceOf[TextView]
    val domainName: TextView = swipeWrap.findViewById(R.id.domainName).asInstanceOf[TextView]
    val linkImage: ImageView = swipeWrap.findViewById(R.id.linkImage).asInstanceOf[ImageView]
    swipeWrap.setTag(this)

    val paymentTypeIconViews: List[View] = paymentTypeIconIds.map(swipeWrap.findViewById)
    val iconMap: Map[Int, View] = paymentTypeIconIds.zip(paymentTypeIconViews).toMap
    var currentDetails: TransactionDetails = _
    var lastVisibleIconId: Int = -1

    setItemLabel setOnClickListener onButtonTap {
      val container = getLayoutInflater.inflate(R.layout.frag_hint_input, null, false)
      val extraInput: EditText = container.findViewById(R.id.extraInput).asInstanceOf[EditText]
      val extraInputLayout: TextInputLayout = container.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]

      def doSetItemLabel(alert: AlertDialog): Unit = {
        val rawLabel = Option(extraInput.getText.toString)
        val trimmedLabel = rawLabel.map(trimmed).filter(_.nonEmpty)
        alert.dismiss

        currentDetails match {
          case info: PayLinkInfo => WalletApp.payMarketBag.updateLabel(trimmedLabel.getOrElse(new String), info.lnurl)
          case info: PaymentInfo => LNParams.cm.payBag.updDescription(info.description.modify(_.label).setTo(trimmedLabel), info.paymentHash)
          case info: TxInfo => WalletApp.txDataBag.updDescription(info.description.modify(_.label).setTo(trimmedLabel), info.txid)
          case _ =>
        }
      }

      extraInputLayout.setHint(dialog_set_record_label)
      val builder = new AlertDialog.Builder(me).setView(container)
      mkCheckForm(doSetItemLabel, none, builder, dialog_ok, dialog_cancel)
      swipeWrap.close(true)
    }

    removeItem setOnClickListener onButtonTap {
      val builder = new AlertDialog.Builder(me).setMessage(confirm_remove_item)
      mkCheckForm(alert => runAnd(alert.dismiss)(doRemoveItem), none, builder, dialog_ok, dialog_cancel)
      swipeWrap.close(true)
    }

    def doRemoveItem: Unit = currentDetails match {
      case info: PayLinkInfo => WalletApp.payMarketBag.remove(info.lnurl)
      case info: PaymentInfo => LNParams.cm.payBag.removePaymentInfo(info.paymentHash)
      case _ =>
    }

    def setVisibleIcon(id: Int): Unit = if (lastVisibleIconId != id) {
      iconMap.get(lastVisibleIconId).foreach(_ setVisibility View.GONE)
      iconMap.get(id).foreach(_ setVisibility View.VISIBLE)
      lastVisibleIconId = id
    }

    def updDetails: Unit =
      currentDetails match {
        case info: RelayedPreimageInfo =>
          setVis(isVisible = false, labelIcon)
          setVis(isVisible = false, detailsAndStatus)
          meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
          amount setText LNParams.denomination.directedWithSign(info.earned, 0L.msat, cardOut, cardIn, cardZero, isPlus = true).html
          nonLinkContainer setBackgroundResource paymentBackground(info.fullTag)
          setVis(isVisible = true, nonLinkContainer)
          setVis(isVisible = false, linkContainer)
          setVisibleIcon(id = R.id.lnRouted)
          swipeWrap.setLockDrag(true)

        case info: TxInfo =>
          setVis(isVisible = true, detailsAndStatus)
          setVis(info.description.label.isDefined, labelIcon)
          amount setText LNParams.denomination.directedWithSign(info.receivedSat, info.sentSat, cardOut, cardIn, cardZero, info.isIncoming).html
          nonLinkContainer setBackgroundResource chainTxBackground(info)
          statusIcon setImageResource txStatusIcon(info)
          description setText txDescription(info).html
          setVis(isVisible = true, nonLinkContainer)
          setVis(isVisible = false, linkContainer)
          setVis(isVisible = false, removeItem)
          swipeWrap.setLockDrag(false)
          setTxTypeIcon(info)
          setTxMeta(info)

        case info: PaymentInfo =>
          setVis(isVisible = true, detailsAndStatus)
          setVis(info.description.label.isDefined, labelIcon)
          amount setText LNParams.denomination.directedWithSign(info.received, info.sent, cardOut, cardIn, cardZero, info.isIncoming).html
          if (info.isIncoming) setIncomingPaymentMeta(info) else setOutgoingPaymentMeta(info)
          nonLinkContainer setBackgroundResource paymentBackground(info.fullTag)
          statusIcon setImageResource paymentStatusIcon(info)
          description setText paymentDescription(info).html
          setVis(isVisible = true, nonLinkContainer)
          setVis(isVisible = false, linkContainer)
          setVis(isVisible = true, removeItem)
          swipeWrap.setLockDrag(false)
          setPaymentTypeIcon(info)

        case info: DelayedRefunds =>
          setVis(isVisible = false, labelIcon)
          setVis(isVisible = true, detailsAndStatus)
          amount setText LNParams.denomination.directedWithSign(info.totalAmount, 0L.msat, cardIn, cardIn, cardZero, isPlus = true).html
          nonLinkContainer setBackgroundResource R.drawable.border_gray
          statusIcon setImageResource R.drawable.baseline_feedback_24
          meta setText getString(delayed_pending).html
          setVis(isVisible = true, nonLinkContainer)
          setVis(isVisible = false, linkContainer)
          description setText delayed_refund
          setVisibleIcon(id = R.id.lnBtc)
          swipeWrap.setLockDrag(true)

        case info: PayLinkInfo =>
          setVis(isVisible = true, linkContainer)
          setVis(isVisible = false, nonLinkContainer)
          val amount = LNParams.denomination.parsedWithSign(info.lastMsat, cardIn, lnCardZero)
          lastAttempt setText getString(lnurl_pay_last_paid).format(WalletApp.app.when(info.date, WalletApp.app.dateFormat), amount).html
          info.imageBytesTry.map(payLinkImageMemo.get).foreach(linkImage.setImageBitmap)
          domainName setText info.lnurl.uri.getHost
          textMetadata setText info.meta.textPlain
          setVis(isVisible = true, removeItem)
          swipeWrap.setLockDrag(false)
      }

    // TX helpers

    private def txDescription(transactionInfo: TxInfo): String = transactionInfo.description match {
      case plain: PlainTxDescription => plain.label orElse plain.addresses.headOption.map(_.shortAddress) getOrElse getString(tx_btc)
      case _: ChanRefundingTxDescription => transactionInfo.description.label getOrElse getString(tx_description_refunding)
      case _: HtlcClaimTxDescription => transactionInfo.description.label getOrElse getString(tx_description_htlc_claiming)
      case _: ChanFundingTxDescription => transactionInfo.description.label getOrElse getString(tx_description_funding)
      case _: OpReturnTxDescription => transactionInfo.description.label getOrElse getString(tx_description_op_return)
      case _: PenaltyTxDescription => transactionInfo.description.label getOrElse getString(tx_description_penalty)
    }

    private def chainTxBackground(info: TxInfo): Int = info.description match {
      case _: HtlcClaimTxDescription if info.depth <= 0L => R.drawable.border_yellow
      case _: PenaltyTxDescription if info.depth <= 0L => R.drawable.border_yellow
      case _ => R.drawable.border_gray
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

    private def setTxMeta(info: TxInfo): Unit = {
      if (info.isDoubleSpent) meta setText getString(tx_state_double_spent).html
      else if (info.depth >= LNParams.minDepthBlocks) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html
      else if (info.depth > 0) meta setText getString(tx_state_confs).format(info.depth, LNParams.minDepthBlocks).html
      else meta setText pctCollected.head
    }

    private def txStatusIcon(info: TxInfo): Int = {
      if (info.depth >= LNParams.minDepthBlocks) R.drawable.baseline_done_24
      else if (info.isDoubleSpent) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    // LN helpers

    private def paymentDescription(info: PaymentInfo): String = info.description.split match {
      case Some(split) => lnSplitNotice.format(split.sentRatio) + info.description.finalDescription.getOrElse(lnDefTitle)
      case None => info.description.finalDescription.getOrElse(lnDefTitle)
    }

    private def setIncomingPaymentMeta(info: PaymentInfo): Unit = {
      val valueHuman = LNParams.cm.inProcessors.get(info.fullTag).map(info.receivedRatio).map(WalletApp.app plurOrZero pctCollected)
      if (PaymentStatus.SUCCEEDED == info.status && valueHuman.isDefined) meta setText pctCollected.last.html // Notify user that we are not exactly done yet
      else if (PaymentStatus.SUCCEEDED == info.status) meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html // Payment has been cleared in channels
      else meta setText valueHuman.getOrElse(pctCollected.head).html // Show either value collected so far or that we are still waiting
    }

    private def setOutgoingPaymentMeta(info: PaymentInfo): Unit = lastInChannelOutgoing.getOrElse(info.fullTag, Nil).size match {
      case partsInChans if PaymentStatus.PENDING == info.status || partsInChans > 0 => meta setText WalletApp.app.plurOrZero(partsInFlight)(partsInChans).html
      case _ => meta setText WalletApp.app.when(info.date, WalletApp.app.dateFormat).html // Payment has either succeeded or failed AND no leftovers are present
    }

    private def setPaymentTypeIcon(info: PaymentInfo): Unit = {
      if (info.isIncoming) setVisibleIcon(R.id.lnIncoming)
      else setOutgoingPaymentIcons(info)
    }

    private def setOutgoingPaymentIcons(info: PaymentInfo): Unit = {
      setVis(view = iconMap(R.id.lnOutgoingAction), isVisible = info.actionString != PaymentInfo.NO_ACTION)
      setVis(view = iconMap(R.id.lnOutgoingBasic), isVisible = info.actionString == PaymentInfo.NO_ACTION)
      setVisibleIcon(id = R.id.lnOutgoing)
    }

    private def paymentStatusIcon(info: PaymentInfo): Int = {
      if (PaymentStatus.SUCCEEDED == info.status) R.drawable.baseline_done_24
      else if (PaymentStatus.ABORTED == info.status) R.drawable.baseline_block_24
      else R.drawable.baseline_hourglass_empty_24
    }

    private def paymentBackground(fullTag: FullPaymentTag): Int = {
      if (ChannelMaster.dangerousHCRevealed(lastHashToReveals, LNParams.blockCount.get, fullTag.paymentHash).nonEmpty) R.drawable.border_red
      else if (LNParams.cm.opm.data.payments contains fullTag) R.drawable.border_blue // An active outgoing FSM is present for this tag
      else if (LNParams.cm.inProcessors contains fullTag) R.drawable.border_blue // An active incoming FSM exists for this tag
      else if (lastInChannelOutgoing contains fullTag) R.drawable.border_blue // Payments in channel are present for this tag
      else R.drawable.border_gray
    }
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

    val totalLightningBalance: TextView = view.findViewById(R.id.totalLightningBalance).asInstanceOf[TextView]
    val channelStateIndicators: LinearLayout = view.findViewById(R.id.channelStateIndicators).asInstanceOf[LinearLayout]
    val channelIndicator: ChannelIndicatorLine = view.findViewById(R.id.channelIndicator).asInstanceOf[ChannelIndicatorLine]

    val inFlightIncoming: TextView = view.findViewById(R.id.inFlightIncoming).asInstanceOf[TextView]
    val inFlightOutgoing: TextView = view.findViewById(R.id.inFlightOutgoing).asInstanceOf[TextView]
    val inFlightRelayed: TextView = view.findViewById(R.id.inFlightRelayed).asInstanceOf[TextView]
    val addChannelTip: ImageView = view.findViewById(R.id.addChannelTip).asInstanceOf[ImageView]

    val listCaption: RelativeLayout = view.findViewById(R.id.listCaption).asInstanceOf[RelativeLayout]
    val toggleGroup: MaterialButtonToggleGroup = view.findViewById(R.id.toggleGroup).asInstanceOf[MaterialButtonToggleGroup]
    val lightningPayments: MaterialButton = view.findViewById(R.id.lightningPayments).asInstanceOf[MaterialButton]
    val bitcoinPayments: MaterialButton = view.findViewById(R.id.bitcoinPayments).asInstanceOf[MaterialButton]
    val relayedPayments: MaterialButton = view.findViewById(R.id.relayedPayments).asInstanceOf[MaterialButton]
    val payMarketLinks: MaterialButton = view.findViewById(R.id.payMarketLinks).asInstanceOf[MaterialButton]
    val searchField: EditText = view.findViewById(R.id.searchField).asInstanceOf[EditText]

    def updateFiatRates: Unit = {
      val change = LNParams.fiatRates.info.pctDifference(WalletApp.fiatCode).map(_ + "<br>").getOrElse(new String)
      val unitPriceAndChange = s"<small>$change</small>${WalletApp currentMsatInFiatHuman 100000000000L.msat}"
      fiatUnitPriceAndChange.setText(unitPriceAndChange.html)
    }

    def updateView: Unit = LNParams.cm.totalBalance match { case currentLnBalance =>
      val walletBalance = currentLnBalance + LNParams.chainWallets.lnWallet.info.lastBalance
      val allChannels = LNParams.cm.all.values.take(8)

      totalFiatBalance.setText(WalletApp.currentMsatInFiatHuman(walletBalance).html)
      totalBalance.setText(LNParams.denomination.parsedWithSign(walletBalance, cardIn, totalZero).html)
      totalLightningBalance.setText(LNParams.denomination.parsedWithSign(currentLnBalance, cardIn, lnCardZero).html)
      totalBitcoinBalance.setText(LNParams.denomination.parsedWithSign(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi, cardIn, btcCardZero).html)
      channelIndicator.createIndicators(allChannels.toArray)

      setVis(LNParams.chainWallets.lnWallet.info.lastBalance != 0L.sat, totalBitcoinBalance)
      setVis(LNParams.chainWallets.lnWallet.info.lastBalance == 0L.sat, receiveBitcoinTip)
      setVis(allChannels.nonEmpty, channelStateIndicators)
      setVis(allChannels.nonEmpty, totalLightningBalance)
      setVis(allChannels.isEmpty, addChannelTip)

      val localInCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.FINAL_INCOMING }
      val localOutCount = LNParams.cm.opm.data.payments.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.LOCALLY_SENT }
      val trampolineCount = LNParams.cm.inProcessors.count { case (fullTag, _) => fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED }
      val hideAll = localInCount + localOutCount + trampolineCount == 0

      inFlightIncoming setAlpha { if (hideAll) 0F else if (localInCount > 0) 1F else 0.3F }
      inFlightOutgoing setAlpha { if (hideAll) 0F else if (localOutCount > 0) 1F else 0.3F }
      inFlightRelayed setAlpha { if (hideAll) 0F else if (trampolineCount > 0) 1F else 0.3F }

      inFlightIncoming.setText(localInCount.toString)
      inFlightOutgoing.setText(localOutCount.toString)
      inFlightRelayed.setText(trampolineCount.toString)
    }
  }

  // LISTENERS

  private var stateSubscription = Option.empty[Subscription]
  private var statusSubscription = Option.empty[Subscription]
  private var paymentSubscription = Option.empty[Subscription]
  private var preimageSubscription = Option.empty[Subscription]
  private var unknownReestablishSubscription = Option.empty[Subscription]

  private val netListener = new Monitor.ConnectivityListener {
    override def onConnectivityChanged(ct: Int, isConnected: Boolean, isFast: Boolean): Unit = UITask {
      // This will make channels SLEEPING right away instead of a bit later when we receive no Pong
      if (!isConnected) CommsTower.workers.values.foreach(_.disconnect)
      setVis(!isConnected, walletCards.offlineIndicator)
    }.run
  }

  private val chainListener = new WalletEventsListener {
    override def onChainSynchronized(event: WalletReady): Unit = {
      // First, update payments to highlight nearly expired revealed incoming now that chain tip it known
      // Second, check if any of unconfirmed chain transactions became confirmed or double-spent
      UITask(walletCards.updateView).run

      for {
        transactionInfo <- txInfos if transactionInfo.depth < LNParams.minDepthBlocks && !transactionInfo.isDoubleSpent
        (newDepth, newDoubleSpent) <- LNParams.chainWallets.lnWallet.doubleSpent(transactionInfo.tx)
        if newDepth != transactionInfo.depth || newDoubleSpent != transactionInfo.isDoubleSpent
      } WalletApp.txDataBag.updStatus(transactionInfo.txid, newDepth, newDoubleSpent)
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
      _ => readEmptyNdefMessage)(_ => me checkExternalData noneRunnable)

  // Channel errors

  private val MAX_ERROR_COUNT_WITHIN_WINDOW = 4
  private val channelErrors = CacheBuilder.newBuilder.expireAfterAccess(30, TimeUnit.SECONDS).maximumSize(500).build[ByteVector32, JInt]

  def chanUnknown(unknown: UnknownReestablish): Unit = UITask {
    def closeAndBreak(alert: AlertDialog): Unit = runAnd(alert.dismiss)(unknown.sendFailExpectClose)
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
    case (CMDException(_, _: CMD_CLOSE), _, _: HasNormalCommitments) => // Swallow this specific error here, it will be displayed on StatActivity
    case (CMDException(_, _: CMD_HOSTED_STATE_OVERRIDE), _, _: HostedCommits) => // Swallow this specific error here, it will be displayed on StatActivity
    case (error: ChannelTransitionFail, _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_closed).format(error.stackTraceAsString), data.commitments.remoteInfo)
    case (error: ChannelTransitionFail, _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_suspended).format(error.stackTraceAsString), hc.remoteInfo)
    case (RemoteErrorException(details), _, data: HasNormalCommitments) => chanError(data.channelId, getString(error_channel_remote).format(details), data.commitments.remoteInfo)
    case (RemoteErrorException(details), _, hc: HostedCommits) if hc.error.isEmpty => chanError(hc.channelId, getString(error_channel_remote).format(details), hc.remoteInfo)
    case (error, _, data: HasNormalCommitments) => chanError(data.channelId, error.stackTraceAsString, data.commitments.remoteInfo)
    case (error, _, hc: HostedCommits) => chanError(hc.channelId, error.stackTraceAsString, hc.remoteInfo)
  }

  // Lifecycle methods

  override def onResume: Unit = {
    val backupAllowed = LocalBackup.isAllowed(me)
    if (!backupAllowed) LocalBackup.askPermission(me)
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def onDestroy: Unit = {
    stateSubscription.foreach(_.unsubscribe)
    statusSubscription.foreach(_.unsubscribe)
    paymentSubscription.foreach(_.unsubscribe)
    preimageSubscription.foreach(_.unsubscribe)
    unknownReestablishSubscription.foreach(_.unsubscribe)

    LNParams.chainWallets.catcher ! WalletEventsCatcher.Remove(chainListener)
    for (channel <- LNParams.cm.all.values) channel.listeners -= me
    LNParams.fiatRates.listeners -= fiatRatesListener
    Tovuti.from(me).stop
    super.onDestroy
  }

  override def onBackPressed: Unit = {
    if (currentSnackbar.isDefined) removeCurrentSnack.run
    else if (isSearchOn) cancelSearch(null)
    else super.onBackPressed
  }

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], grantResults: GrantResults): Unit =
    if (reqCode == scannerRequestCode && grantResults.nonEmpty && grantResults.head == PackageManager.PERMISSION_GRANTED)
      bringScanner(null)

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
              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              title.addChipText(getString(dialog_ln_requested) format LNParams.denomination.parsedWithSign(origAmount, cardIn, cardZero), R.drawable.border_blue)
              title.addChipText(getString(dialog_ln_left) format LNParams.denomination.parsedWithSign(prExt.splitLeftover, cardIn, cardZero), R.drawable.border_blue)
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
              val builder = titleBodyAsViewBuilder(title.asColoredView(R.color.cardLightning), manager.content)
              title.addChipText(getString(dialog_ln_requested).format(totalHuman), R.drawable.border_blue)
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
      case pay: PayRequest => bringPayPopup(pay, lnUrl).run
      case withdraw: WithdrawRequest => UITask(me bringWithdrawPopup withdraw).run
      case nc: NormalChannelRequest => runAnd(InputParser.value = nc)(me goTo ClassNames.remotePeerActivityClass)
      case hc: HostedChannelRequest => runAnd(InputParser.value = hc)(me goTo ClassNames.remotePeerActivityClass)
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

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (CHOICE_RECEIVE_TAG, 0) => me goTo ClassNames.qrChainActivityClass
    case (CHOICE_RECEIVE_TAG, 1) => bringReceivePopup
    case _ =>
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.btcontract.wallettest.R.layout.activity_hub)
      for (channel <- LNParams.cm.all.values) channel.listeners += me
      LNParams.fiatRates.listeners += fiatRatesListener
      LNParams.chainWallets.catcher ! chainListener
      Tovuti.from(me).monitor(netListener)

      bottomActionBar post UITask {
        bottomBlurringArea.setHeightTo(bottomActionBar)
        itemsList.setPadding(0, 0, 0, bottomActionBar.getHeight)
      }

      // Set selections before list items and listener
      walletCards.toggleGroup.check(R.id.bitcoinPayments)
      walletCards.toggleGroup.check(R.id.lightningPayments)

      walletCards.toggleGroup addOnButtonCheckedListener new OnButtonCheckedListener {
        def onButtonChecked(g: MaterialButtonToggleGroup, checkId: Int, isChecked: Boolean): Unit =
          runAnd(updAllInfos)(paymentAdapterDataChanged.run)
      }

      itemsList.addHeaderView(walletCards.view)
      itemsList.setAdapter(paymentsAdapter)
      itemsList.setDividerHeight(0)
      itemsList.setDivider(null)

      walletCards.updateFiatRates
      walletCards.updateView

      runInFutureProcessOnUI(loadRecent, none) { _ =>
        // We suggest user to rate us if: no rate attempt has been made before, LN payments were successful, user has been using an app for certain period
        setVis(WalletApp.showRateUs && paymentInfos.forall(_.status == PaymentStatus.SUCCEEDED) && allInfos.size > 4 && allInfos.size < 8, walletCards.rateTeaser)
        walletCards.searchField.addTextChangedListener(me onTextChange searchWorker.addWork)
        runAnd(updateLnCaches)(paymentAdapterDataChanged.run)
      }

      val window = 500.millis
      // Throttle all types of burst updates, but make sure the last one is always called
      val txEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.txDbStream, window).doOnNext(_ => reloadTxInfos)
      val relayEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.relayDbStream, window).doOnNext(_ => reloadRelayedPreimageInfos)
      val marketEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.payMarketDbStream, window).doOnNext(_ => reloadPayMarketInfos)
      val paymentEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.paymentDbStream, window).doOnNext(_ => reloadPaymentInfos)
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window).doOnNext(_ => updateLnCaches)

      stateSubscription = txEvents.merge(paymentEvents).merge(relayEvents).merge(marketEvents).merge(stateEvents).doOnNext(_ => updAllInfos).subscribe(_ => paymentAdapterDataChanged.run).asSome
      statusSubscription = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window).merge(stateEvents).subscribe(_ => UITask(walletCards.updateView).run).asSome
      paymentSubscription = ChannelMaster.hashRevealStream.merge(ChannelMaster.remoteFulfillStream).throttleFirst(window).subscribe(_ => Vibrator.vibrate).asSome
      preimageSubscription = ChannelMaster.remoteFulfillStream.subscribe(fulfill => resolveAction(fulfill).run, none).asSome
      unknownReestablishSubscription = ChannelMaster.unknownReestablishStream.subscribe(chanUnknown, none).asSome
      timer.scheduleAtFixedRate(paymentAdapterDataChanged, 30000, 30000)
      markAsFailedOnce
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringRateDialog(view: View): Unit = {
    val marketUri = Uri.parse(s"market://details?id=$getPackageName")
    WalletApp.app.prefs.edit.putBoolean(WalletApp.SHOW_RATE_US, false).commit
    me startActivity new Intent(Intent.ACTION_VIEW, marketUri)
    view.setVisibility(View.GONE)
  }

  def bringMenu(view: View): Unit = {
    val popupMenu = new PopupMenu(me, view)
    popupMenu setOnMenuItemClickListener new PopupMenu.OnMenuItemClickListener {
      override def onMenuItemClick(selectedPopupMenuSection: MenuItem): Boolean = {
        if (selectedPopupMenuSection.getItemId == 1) me goTo ClassNames.statActivityClass
        false
      }
    }

    popupMenu.getMenu.add(0, 0, 0, menu_settings)
    popupMenu.getMenu.add(0, 1, 1, menu_chans_stats)
    popupMenu.show
  }

  def isSearchOn: Boolean = walletCards.searchField.getVisibility == View.VISIBLE
  def keyBoardOn: Unit = WalletApp.app.showKeys(walletCards.searchField)

  def bringSearch(view: View): Unit = {
    TransitionManager.beginDelayedTransition(walletCards.view)
    walletCards.searchField.setVisibility(View.VISIBLE)
    walletCards.defaultHeader.setVisibility(View.GONE)
    walletCards.searchField.requestFocus
    keyBoardOn
  }

  def cancelSearch(view: View): Unit = {
    walletCards.searchField.setText(new String)
    WalletApp.app.hideKeys(walletCards.searchField)
    TransitionManager.beginDelayedTransition(walletCards.view)
    walletCards.defaultHeader.setVisibility(View.VISIBLE)
    walletCards.searchField.setVisibility(View.GONE)
  }

  def bringScanner(view: View): Unit = {
    def explainClipboardFailure = UITask {
      val message = getString(error_nothing_in_clipboard)
      snack(contentWindow, message.html, dialog_ok, _.dismiss)
    }

    val onScan = UITask(me checkExternalData noneRunnable)
    val onPaste = UITask(me checkExternalData explainClipboardFailure)
    val sheet = new sheets.ScannerBottomSheet(me, None, onScan, onPaste)
    callScanner(sheet)
  }

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(getString).map(_.html)
    val list = makeChoiceList(options, itemId = android.R.layout.simple_expandable_list_item_1)
    new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me).show(getSupportFragmentManager, "unused-tag")
  }

  def goToReceiveBitcoinPage(view: View): Unit = onChoiceMade(CHOICE_RECEIVE_TAG, 0)
  def bringLnReceivePopup(view: View): Unit = onChoiceMade(CHOICE_RECEIVE_TAG, 1)

  def bringSendBitcoinPopup(uri: BitcoinUri): Unit = {
    val body = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
    val manager = new RateManager(body, getString(dialog_add_btc_memo).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
    val canSend = LNParams.denomination.parsedWithSign(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi, cardIn, cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi)

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
        txAndFee <- LNParams.chainWallets.lnWallet.sendPayment(manager.resultSat, uri.address, feeView.rate)
        // Record this description before attempting to send, we won't be able to know a memo otherwise
        knownDescription = PlainTxDescription(uri.address :: Nil, manager.resultExtraInput)
        _ = WalletApp.txDescriptions += Tuple2(txAndFee.tx.txid, knownDescription)
        definitelyCommitted <- LNParams.chainWallets.lnWallet.commit(txAndFee.tx)
        if !definitelyCommitted
      } warnSendingFailed.run
    }

    lazy val alert = {
      val neutralRes = if (uri.amount.isDefined) -1 else dialog_max
      val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
      val message = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
      val builder = titleBodyAsViewBuilder(getString(dialog_send_btc).format(uri.address.shortAddress, label + message).asColoredView(R.color.cardBitcoin), manager.content)
      if (uri.prExt.isEmpty) mkCheckFormNeutral(attempt, none, _ => manager.updateText(LNParams.chainWallets.lnWallet.info.lastBalance.toMilliSatoshi), builder, dialog_pay, dialog_cancel, neutralRes)
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
      def work(amount: Satoshi): Observable[TxAndFee] = Rx fromFutureOnIo LNParams.chainWallets.lnWallet.sendPayment(amount, uri.address, feeView.rate)
      def process(amount: Satoshi, txAndFee: TxAndFee): Unit = feeView.update(feeOpt = txAndFee.fee.toMilliSatoshi.asSome, showIssue = false)
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

    manager.hintDenom.setText(getString(dialog_up_to).format(canSend).html)
    manager.hintFiatDenom.setText(getString(dialog_up_to).format(canSendFiat).html)
    feeView.update(feeOpt = None, showIssue = false)

    uri.amount.foreach { asked =>
      manager.updateText(value = asked)
      manager.inputAmount.setEnabled(false)
      manager.fiatInputAmount.setEnabled(false)
    }
  }

  def bringReceivePopup: Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = Long.MaxValue.msat, initMinReceivable = 0L.msat, LNParams.cm.totalBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_description).asSome, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainDescription(split = None, label = None, invoiceText = manager.resultExtraInput getOrElse new String)
      override def processInvoice(prExt: PaymentRequestExt): Unit = runAnd(InputParser.value = prExt)(me goTo ClassNames.qrInvoiceActivityClass)
      override def getTitleText: String = getString(dialog_receive_ln)
    }
  }

  def bringWithdrawPopup(data: WithdrawRequest): Unit = lnReceiveGuard(contentWindow) {
    new OffChainReceiver(initMaxReceivable = data.maxWithdrawable.msat, initMinReceivable = data.minCanReceive, LNParams.cm.totalBalance) {
      override def getManager: RateManager = new RateManager(body, getString(dialog_add_ln_memo).asSome, dialog_visibility_private, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def getDescription: PaymentDescription = PlainMetaDescription(split = None, label = manager.resultExtraInput, invoiceText = new String, meta = data.descriptionOrEmpty)
      override def getTitleText: String = getString(dialog_lnurl_withdraw).format(data.callbackUri.getHost, data.brDescription)
      override def processInvoice(prExt: PaymentRequestExt): Unit = data.requestWithdraw(prExt).foreach(none, onFail)
    }
  }

  def bringPayPopup(data: PayRequest, lnUrl: LNUrl): TimerTask = UITask {
    new OffChainSender(maxSendable = LNParams.cm.maxSendable(LNParams.cm.all.values).min(data.maxSendable.msat), minSendable = LNParams.minPayment max data.minSendable.msat) {
      override lazy val manager: RateManager = new RateManager(body, getString(dialog_add_comment).asSome, dialog_visibility_public, LNParams.fiatRates.info.rates, WalletApp.fiatCode)
      override def isNeutralEnabled: Boolean = manager.resultMsat >= LNParams.minPayment && manager.resultMsat <= minSendable - LNParams.minPayment
      override def isPayEnabled: Boolean = manager.resultMsat >= minSendable && manager.resultMsat <= maxSendable

      override def neutral(alert: AlertDialog): Unit = {
        def proceed(pf: PayRequestFinal): TimerTask = UITask {
          lnSendGuard(pf.prExt, container = contentWindow) { _ =>
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(minSendable)
            InputParser.value = SplitParams(pf.prExt, pf.successAction, PlainMetaDescription(cmd.split.asSome, label = None, invoiceText = new String, meta = data.meta.textPlain), cmd, typicalChainTxFee)
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
            if (!pf.isThrowAway) WalletApp.payMarketBag.saveLink(lnUrl, data, manager.resultMsat, pf.prExt.pr.paymentHash.toHex)
            val cmd = LNParams.cm.makeSendCmd(pf.prExt, manager.resultMsat, LNParams.cm.all.values.toList, typicalChainTxFee, WalletApp.capLNFeeToChain).modify(_.split.totalSum).setTo(manager.resultMsat)
            replaceOutgoingPayment(pf.prExt, PlainMetaDescription(split = None, label = None, invoiceText = new String, meta = data.meta.textPlain), pf.successAction, sentAmount = cmd.split.myPart)
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
        val text = getString(dialog_lnurl_pay).format(data.callbackUri.getHost, s"<br><br>${data.meta.textPlain}")
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
  }

  def paymentAdapterDataChanged: TimerTask = UITask {
    setVis(txInfos.nonEmpty || paymentInfos.nonEmpty || relayedPreimageInfos.nonEmpty || payMarketInfos.nonEmpty, walletCards.listCaption)
    setVis(relayedPreimageInfos.nonEmpty, walletCards.relayedPayments)
    setVis(payMarketInfos.nonEmpty, walletCards.payMarketLinks)
    paymentsAdapter.notifyDataSetChanged
  }

  // Payment actions

  def resolveAction(fulfill: RemoteFulfill): TimerTask = UITask {
    paymentInfos.find(_.paymentHash == fulfill.ourAdd.paymentHash).flatMap(_.action).foreach {
      case data: MessageAction => mkCheckFormNeutral(_.dismiss, none, _ => share(data.message), actionPopup(data.finalMessage.html, data), dialog_ok, dialog_cancel, dialog_share)
      case data: UrlAction => mkCheckFormNeutral(_ => browse(data.url), none, _ => share(data.url), actionPopup(data.finalMessage.html, data), dialog_open, dialog_cancel, dialog_share)
      case data: AESAction => showAesAction(fulfill.theirPreimage, data) getOrElse mkCheckForm(_.dismiss, none, actionPopup(getString(dialog_lnurl_decrypt_fail), data), dialog_ok, noRes = -1)
    }
  }

  private def showAesAction(preimage: ByteVector32, aes: AESAction) = Try {
    val secret = SQLiteData byteVecToString AES.decode(data = aes.ciphertextBytes, key = preimage.toArray, initVector = aes.ivBytes)
    val msg = if (secret.length > 36) s"${aes.finalMessage}<br><br><tt>$secret</tt><br>" else s"${aes.finalMessage}<br><br><tt><big>$secret</big></tt><br>"
    mkCheckFormNeutral(_.dismiss, none, _ => share(secret), actionPopup(msg.html, aes), dialog_ok, dialog_cancel, dialog_share)
  }

  private def actionPopup(msg: CharSequence, action: PaymentAction) = {
    val fromVendor = action.domain.map(site => s"<br><br><b>$site</b>").getOrElse(new String)
    val title = getString(dialog_lnurl_from_vendor).format(fromVendor).asDefView
    new AlertDialog.Builder(me).setCustomTitle(title).setMessage(msg)
  }
}
