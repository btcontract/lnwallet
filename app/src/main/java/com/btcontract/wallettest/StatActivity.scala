package com.btcontract.wallettest

import immortan._
import android.widget._
import fr.acinq.eclair._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.btcontract.wallettest.Colors._
import com.btcontract.wallettest.R.string._

import java.util.{Date, TimerTask}
import android.view.{View, ViewGroup}
import android.graphics.{Bitmap, BitmapFactory}
import immortan.utils.{BitcoinUri, InputParser, PaymentRequestExt, Rx}
import com.chauthai.swipereveallayout.{SwipeRevealLayout, ViewBinderHelper}
import fr.acinq.eclair.channel.{CMD_CLOSE, Commitments, DATA_CLOSING, NormalCommits}
import com.btcontract.wallettest.BaseActivity.StringOps
import fr.acinq.eclair.wire.HostedChannelBranding
import androidx.recyclerview.widget.RecyclerView
import com.google.common.cache.LoadingCache
import com.indicator.ChannelIndicatorLine
import androidx.appcompat.app.AlertDialog
import androidx.cardview.widget.CardView
import com.ornach.nobobutton.NoboButton
import rx.lang.scala.Subscription
import immortan.wire.HostedState
import android.os.Bundle


class StatActivity extends BaseActivity with ChoiceReceiver with HasTypicalChainFee { me =>
  private[this] lazy val chanContainer = findViewById(R.id.chanContainer).asInstanceOf[LinearLayout]
  private[this] lazy val chanList = findViewById(R.id.chanList).asInstanceOf[ListView]

  private[this] lazy val brandingInfos = WalletApp.txDataBag.db.txWrap(getBrandingInfos.toMap)
  private[this] lazy val normalChanActions = getResources.getStringArray(R.array.ln_normal_chan_actions)
  private[this] lazy val hostedChanActions = getResources.getStringArray(R.array.ln_hosted_chan_actions)
  private[this] var updateSubscription = Option.empty[Subscription]
  private[this] var csToDisplay = Seq.empty[ChanAndCommits]

  val hcImageMemo: LoadingCache[Bytes, Bitmap] = memoize {
    bytes => BitmapFactory.decodeByteArray(bytes, 0, bytes.length)
  }

  val chanAdapter: BaseAdapter = new BaseAdapter {
    private[this] val viewBinderHelper = new ViewBinderHelper
    override def getItem(pos: Int): ChanAndCommits = csToDisplay(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = csToDisplay.size

    def getView(position: Int, savedView: View, parent: ViewGroup): View = {
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.frag_chan_card, null) else savedView

      val cardView = (getItem(position), card.getTag) match {
        case (ChanAndCommits(chan: ChannelHosted, hc: HostedCommits), view: HostedViewHolder) => view.fill(chan, hc)
        case (ChanAndCommits(chan: ChannelHosted, hc: HostedCommits), _) => new HostedViewHolder(card).fill(chan, hc)
        case (ChanAndCommits(chan: ChannelNormal, commits: NormalCommits), view: NormalViewHolder) => view.fill(chan, commits)
        case (ChanAndCommits(chan: ChannelNormal, commits: NormalCommits), _) => new NormalViewHolder(card).fill(chan, commits)
        case _ => throw new RuntimeException
      }

      viewBinderHelper.bind(cardView.swipeWrap, position.toString)
      card.setTag(cardView)
      card
    }
  }

  abstract class ChanCardViewHolder(view: View) extends RecyclerView.ViewHolder(view) {
    val swipeWrap: SwipeRevealLayout = itemView.asInstanceOf[SwipeRevealLayout]

    val removeItem: NoboButton = swipeWrap.findViewById(R.id.removeItem).asInstanceOf[NoboButton]
    val channelCard: CardView = swipeWrap.findViewById(R.id.channelCard).asInstanceOf[CardView]

    val hcBranding: RelativeLayout = swipeWrap.findViewById(R.id.hcBranding).asInstanceOf[RelativeLayout]
    val hcSupportInfo: TextView = swipeWrap.findViewById(R.id.hcSupportInfo).asInstanceOf[TextView]
    val hcImage: ImageView = swipeWrap.findViewById(R.id.hcImage).asInstanceOf[ImageView]

    val baseBar: ProgressBar = swipeWrap.findViewById(R.id.baseBar).asInstanceOf[ProgressBar]
    val overBar: ProgressBar = swipeWrap.findViewById(R.id.overBar).asInstanceOf[ProgressBar]
    val peerAddress: TextView = swipeWrap.findViewById(R.id.peerAddress).asInstanceOf[TextView]
    val chanState: View = swipeWrap.findViewById(R.id.chanState).asInstanceOf[View]

    val canSendText: TextView = swipeWrap.findViewById(R.id.canSendText).asInstanceOf[TextView]
    val canReceiveText: TextView = swipeWrap.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val refundableAmountText: TextView = swipeWrap.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText: TextView = swipeWrap.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val totalCapacityText: TextView = swipeWrap.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val extraInfoText: TextView = swipeWrap.findViewById(R.id.extraInfoText).asInstanceOf[TextView]

    val wrappers: Seq[View] =
      swipeWrap.findViewById(R.id.progressBars).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.refundableAmount).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.paymentsInFlight).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.canReceive).asInstanceOf[View] ::
        swipeWrap.findViewById(R.id.canSend).asInstanceOf[View] ::
        Nil

    def visibleExcept(goneRes: Int*): Unit = for (wrap <- wrappers) {
      val hideView = goneRes.contains(wrap.getId)
      setVis(!hideView, wrap)
    }

    baseBar.setMax(1000)
    overBar.setMax(1000)
  }

  class NormalViewHolder(view: View) extends ChanCardViewHolder(view) {
    def fill(chan: ChannelNormal, cs: NormalCommits): NormalViewHolder = {

      val capacity: Satoshi = cs.commitInput.txOut.amount
      val barCanReceive = (cs.availableForReceive.toLong / capacity.toLong).toInt
      val barCanSend = (cs.latestReducedRemoteSpec.toRemote.toLong / capacity.toLong).toInt
      val barLocalReserve = (cs.latestReducedRemoteSpec.toRemote - cs.availableForSend).toLong / capacity.toLong
      val inFlight: MilliSatoshi = cs.latestReducedRemoteSpec.htlcs.foldLeft(0L.msat)(_ + _.add.amountMsat)
      val refundable: MilliSatoshi = cs.latestReducedRemoteSpec.toRemote + inFlight

      if (Channel isWaiting chan) {
        setVis(isVisible = false, extraInfoText)
        channelCard setOnClickListener bringChanOptions(normalChanActions.take(2), cs)
        visibleExcept(R.id.progressBars, R.id.paymentsInFlight, R.id.canReceive, R.id.canSend)
      } else if (Channel isOperational chan) {
        channelCard setOnClickListener bringChanOptions(normalChanActions, cs)
        setVis(isVisible = cs.updateOpt.isEmpty, extraInfoText)
        extraInfoText.setText(ln_info_no_update)
        visibleExcept(goneRes = -1)
      } else {
        val closeInfoRes = chan.data match { case c: DATA_CLOSING => closedBy(c) case _ => ln_info_shutdown }
        channelCard setOnClickListener bringChanOptions(normalChanActions.take(2), cs)
        visibleExcept(R.id.progressBars, R.id.canReceive, R.id.canSend)
        extraInfoText.setText(getString(closeInfoRes).html)
        setVis(isVisible = true, extraInfoText)
      }

      removeItem setOnClickListener onButtonTap {
        def proceed: Unit = chan process CMD_CLOSE(None, force = true)
        val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_force_close).html)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
        swipeWrap.close(true)
      }

      setVis(isVisible = false, hcBranding)

      ChannelIndicatorLine.setView(chanState, chan)
      peerAddress.setText(cs.remoteInfo.address.toString)
      overBar.setProgress(barCanSend min barLocalReserve.toInt)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      totalCapacityText.setText(sumOrNothing(capacity, cardIn).html)
      canReceiveText.setText(sumOrNothing(cs.availableForReceive.truncateToSatoshi, cardOut).html)
      canSendText.setText(sumOrNothing(cs.availableForSend.truncateToSatoshi, cardIn).html)

      refundableAmountText.setText(sumOrNothing(refundable.truncateToSatoshi, cardIn).html)
      paymentsInFlightText.setText(sumOrNothing(inFlight.truncateToSatoshi, cardIn).html)
      this
    }
  }

  class HostedViewHolder(view: View) extends ChanCardViewHolder(view) {
    def fill(chan: ChannelHosted, hc: HostedCommits): HostedViewHolder = {
      val capacity: Satoshi = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat.truncateToSatoshi
      val inFlight: MilliSatoshi = hc.nextLocalSpec.htlcs.foldLeft(0L.msat)(_ + _.add.amountMsat)
      val barCanReceive = (hc.availableForReceive.toLong / capacity.toLong).toInt
      val barCanSend = (hc.availableForSend.toLong / capacity.toLong).toInt

      val errorText = (hc.localError, hc.remoteError) match {
        case Some(error) ~ _ => s"LOCAL: ${ErrorExt extractDescription error}"
        case _ ~ Some(error) => s"REMOTE: ${ErrorExt extractDescription error}"
        case _ => new String
      }

      val brandOpt = brandingInfos.get(hc.remoteInfo.nodeId)
      setVis(isVisible = brandOpt.isDefined, hcBranding)

      for (HostedChannelBranding(_, pngIcon, contactInfo) <- brandOpt) {
        pngIcon.map(_.toArray).map(hcImageMemo.get).foreach(hcImage.setImageBitmap)
        hcSupportInfo.setText(contactInfo)
      }

      removeItem setOnClickListener onButtonTap {
        if (hc.localSpec.htlcs.nonEmpty) snack(chanContainer, getString(ln_hosted_chan_remove_impossible).html, R.string.dialog_ok, _.dismiss)
        else mkCheckForm(alert => runAnd(alert.dismiss)(me removeHc hc), none, confirmationBuilder(hc, getString(confirm_ln_hosted_chan_remove).html), dialog_ok, dialog_cancel)
        swipeWrap.close(true)
      }

      channelCard setOnClickListener bringChanOptions(hostedChanActions, hc)

      visibleExcept(R.id.refundableAmount)
      ChannelIndicatorLine.setView(chanState, chan)
      peerAddress.setText(hc.remoteInfo.address.toString)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      totalCapacityText.setText(sumOrNothing(capacity, cardIn).html)
      canReceiveText.setText(sumOrNothing(hc.availableForReceive.truncateToSatoshi, cardOut).html)
      canSendText.setText(sumOrNothing(hc.availableForSend.truncateToSatoshi, cardIn).html)

      paymentsInFlightText.setText(sumOrNothing(inFlight.truncateToSatoshi, cardIn).html)
      setVis(isVisible = hc.error.isDefined || hc.updateOpt.isEmpty, extraInfoText)
      // Order messages by degree of importance
      extraInfoText.setText(ln_info_no_update)
      extraInfoText.setText(errorText)
      this
    }
  }

  override def onDestroy: Unit = {
    updateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = (tag, pos) match {
    case (cs: NormalCommits, 1) => browseTxid(cs.commitInput.outPoint.txid)
    case (hc: HostedCommits, 1) => share(me getHcState hc)

    case (cs: NormalCommits, 2) =>
      val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_close_wallet).html)
      def proceed: Unit = for (chan <- me getChan cs) chan process CMD_CLOSE(None, force = false)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)

    case (hc: HostedCommits, 2) =>
      val builder = confirmationBuilder(hc, getString(confirm_ln_hosted_chan_drain).html)
      mkCheckForm(alert => runAnd(alert.dismiss)(me drainHc hc), none, builder, dialog_ok, dialog_cancel)

    case (cs: NormalCommits, 3) => closeNcToAddress(cs)
    case (c: Commitments, 0) => share(me getDetails c)
    case _ =>
  }

  def closeNcToAddress(cs: NormalCommits): Unit = {
    def confirmResolve(bitcoinUri: BitcoinUri): Unit = {
      def proceed: Unit = for (chan <- me getChan cs) chan process CMD_CLOSE(scriptPubKey = Script.write(bitcoinUri.pubKeyScript).asSome, force = false)
      val builder = confirmationBuilder(cs, getString(confirm_ln_normal_chan_close_address).format(bitcoinUri.address.humanFour).html)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
    }

    def resolveClosingAddress: Unit = InputParser.checkAndMaybeErase {
      case ext: PaymentRequestExt if ext.pr.fallbackAddress.isDefined => ext.pr.fallbackAddress.map(BitcoinUri.fromRaw).foreach(confirmResolve)
      case closingBitcoinUri: BitcoinUri if closingBitcoinUri.isValid => confirmResolve(closingBitcoinUri)
      case _ => UITask(WalletApp.app quickToast error_nothing_useful).run
    }

    def onData: Runnable = UITask(resolveClosingAddress)
    val instruction = getString(ln_normal_chan_close_scan_address).asSome
    val sheet = new sheets.ScannerBottomSheet(me, instruction, onData, onData)
    callScanner(sheet)
  }

  def drainHc(hc: HostedCommits): Unit = maxNormalReceivable match {
    case None => snack(chanContainer, getString(ln_hosted_chan_drain_impossible).html, R.string.dialog_ok, _.dismiss)
    case Some(csAndMax) if csAndMax.maxReceivable < LNParams.minPayment => snack(chanContainer, getString(ln_hosted_chan_drain_impossible).html, R.string.dialog_ok, _.dismiss)
    case Some(csAndMax) => LNParams.cm.localSendToSelf(getChan(hc).toList, csAndMax, randomBytes32, typicalChainTxFee, WalletApp.capLNFeeToChain)
  }

  def removeHc(hc: HostedCommits): Unit = {
    LNParams.cm.chanBag.delete(hc.channelId)
    LNParams.cm.all -= hc.channelId

    // Update hub activity balance and chan list here
    ChannelMaster.next(ChannelMaster.stateUpdateStream)
    CommsTower.disconnectNative(hc.remoteInfo)
    updateChanData.run
  }

  def getHcState(hc: HostedCommits): String = {
    val state = HostedState(hc.remoteInfo.nodeId, hc.remoteInfo.nodeSpecificPubKey, hc.lastCrossSignedState)
    val data = immortan.wire.ExtCodecs.hostedStateCodec.encode(state).require.toHex
    val preimages = hc.revealedFulfills.map(_.ourPreimage.toHex).mkString("\n")
    getString(ln_hosted_chan_state).format(getDetails(hc), data, preimages)
  }

  def getDetails(cs: Commitments): String = {
    val remoteId = cs.remoteInfo.nodeId.toString
    val localId = cs.remoteInfo.nodeSpecificPubKey.toString
    val shortId = cs.updateOpt.map(_.shortChannelId.toString).getOrElse("unknown")
    val stamp = WalletApp.app.when(new Date(cs.startedAt), WalletApp.app.dateFormat)
    getString(ln_chan_details).format(remoteId, localId, shortId, stamp)
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_stat)
      updateChanData.run

      val title = new TitleView(me getString menu_chans_stats)
      title.view.setOnClickListener(me onButtonTap finish)
      title.backArrow.setVisibility(View.VISIBLE)
      chanList.addHeaderView(title.view)
      chanList.setAdapter(chanAdapter)
      chanList.setDividerHeight(0)
      chanList.setDivider(null)

      WalletApp.txDataBag.db.txWrap {
        val txSummary = WalletApp.txDataBag.txSummary.filter(_.count > 0)
        val relaySummary = LNParams.cm.payBag.relaySummary.filter(_.count > 0)
        val paymentSummary = LNParams.cm.payBag.paymentSummary.filter(_.count > 0)
        val channelTxFeesSummary = LNParams.cm.chanBag.channelTxFeesSummary.filter(_.count > 0)

        if (txSummary.isSuccess || relaySummary.isSuccess || paymentSummary.isSuccess || channelTxFeesSummary.isSuccess) {
          val statList = getLayoutInflater.inflate(R.layout.frag_stat_list, null).asInstanceOf[LinearLayout]
          chanList.addFooterView(statList)

          for (summary <- txSummary) {
            val slotTitle = new TitleView(me getString stats_title_chain)
            slotTitle.addChipText(getString(stats_item_transactions).format(summary.count), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_received) format LNParams.denomination.directedWithSign(summary.received, 0L.sat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_sent) format LNParams.denomination.directedWithSign(0L.sat, summary.sent, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.sat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- paymentSummary) {
            val slotTitle = new TitleView(me getString stats_title_ln)
            slotTitle.addChipText(getString(stats_item_payments).format(summary.count), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_received) format LNParams.denomination.directedWithSign(summary.received, 0L.msat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_sent) format LNParams.denomination.directedWithSign(0L.msat, summary.sent, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.msat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees_saved) format LNParams.denomination.parsedWithSign(summary.chainFees - summary.fees, cardIn, cardZero), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- relaySummary) {
            val slotTitle = new TitleView(me getString stats_title_relays)
            slotTitle.addChipText(getString(stats_item_relays).format(summary.count), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_relayed) format LNParams.denomination.parsedWithSign(summary.relayed, cardIn, cardZero), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_earned) format LNParams.denomination.directedWithSign(summary.earned, 0L.msat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- channelTxFeesSummary) {
            val slotTitle = new TitleView(me getString stats_title_chan_loss)
            slotTitle.addChipText(getString(stats_item_transactions).format(summary.count), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.sat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }
        }
      }

      val window = 500.millis
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window)
      val statusEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window)
      updateSubscription = stateEvents.merge(statusEvents).subscribe(_ => updateChanData.run).asSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  private def getBrandingInfos = for {
    ChanAndCommits(_: ChannelHosted, commits) <- csToDisplay
    brand <- WalletApp.extDataBag.tryGetBranding(commits.remoteInfo.nodeId).toOption
  } yield commits.remoteInfo.nodeId -> brand

  private def sumOrNothing(amt: Satoshi, mainColor: String): String = {
    if (0L.sat != amt) LNParams.denomination.parsedWithSign(amt.toMilliSatoshi, mainColor, cardZero)
    else getString(chan_nothing)
  }

  private def closedBy(cd: DATA_CLOSING): Int = {
    if (cd.remoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.nextRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.futureRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.mutualClosePublished.nonEmpty) ln_info_close_coop
    else ln_info_close_local
  }

  private def confirmationBuilder(commits: Commitments, msg: CharSequence) = new AlertDialog.Builder(me).setTitle(commits.remoteInfo.address.toString).setMessage(msg)

  private def getChan(commits: Commitments) = csToDisplay.collectFirst { case cnc if cnc.commits.channelId == commits.channelId => cnc.chan }

  private def maxNormalReceivable = LNParams.cm.maxReceivable(LNParams.cm sortedReceivable LNParams.cm.allNormal)

  private def updateChanData: TimerTask = UITask {
    csToDisplay = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).toList
    chanAdapter.notifyDataSetChanged
  }

  def bringChanOptions(options: Array[String], cs: Commitments): View.OnClickListener = onButtonTap {
    val allOptions = makeChoiceList(options.map(_.html), android.R.layout.simple_expandable_list_item_1)
    new sheets.ChoiceBottomSheet(allOptions, cs, me).show(getSupportFragmentManager, "unused-tag")
  }
}
