package com.btcontract.wallettest

import immortan._
import fr.acinq.eclair._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.btcontract.wallettest.Colors._
import com.btcontract.wallettest.R.string._

import android.view.{View, ViewGroup}
import fr.acinq.eclair.channel.{DATA_CLOSING, NormalCommits}
import android.widget.{BaseAdapter, LinearLayout, ListView, ProgressBar, TextView}
import com.btcontract.wallettest.BaseActivity.StringOps
import androidx.recyclerview.widget.RecyclerView
import com.indicator.ChannelIndicatorLine
import rx.lang.scala.Subscription
import java.util.TimerTask
import android.os.Bundle
import immortan.utils.Rx


class StatActivity extends BaseActivity { me =>
  private[this] var csToDisplay = Seq.empty[ChanAndCommits]
  private[this] var updateSubscription = Option.empty[Subscription]
  private[this] lazy val chanList = findViewById(R.id.chanList).asInstanceOf[ListView]

  val chanAdapter: BaseAdapter = new BaseAdapter {
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

      card.setTag(cardView)
      card
    }
  }

  abstract class ChanCardViewHolder(view: View) extends RecyclerView.ViewHolder(view) {
    val baseBar: ProgressBar = view.findViewById(R.id.baseBar).asInstanceOf[ProgressBar]
    val overBar: ProgressBar = view.findViewById(R.id.overBar).asInstanceOf[ProgressBar]
    val peerAddress: TextView = view.findViewById(R.id.peerAddress).asInstanceOf[TextView]
    val chanState: View = view.findViewById(R.id.chanState).asInstanceOf[View]

    val canSendText: TextView = view.findViewById(R.id.canSendText).asInstanceOf[TextView]
    val canReceiveText: TextView = view.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val refundableAmountText: TextView = view.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText: TextView = view.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val totalCapacityText: TextView = view.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val extraInfoText: TextView = view.findViewById(R.id.extraInfoText).asInstanceOf[TextView]

    val wrappers: Seq[View] =
      view.findViewById(R.id.progressBars).asInstanceOf[View] ::
        view.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        view.findViewById(R.id.balancesDivider).asInstanceOf[View] ::
        view.findViewById(R.id.refundableAmount).asInstanceOf[View] ::
        view.findViewById(R.id.paymentsInFlight).asInstanceOf[View] ::
        view.findViewById(R.id.canReceive).asInstanceOf[View] ::
        view.findViewById(R.id.canSend).asInstanceOf[View] ::
        Nil

    def visibleExcept(goneRes: Int*): Unit = for (wrap <- wrappers) setVis(!goneRes.contains(wrap.getId), wrap)

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
        visibleExcept(R.id.progressBars, R.id.paymentsInFlight, R.id.canReceive, R.id.canSend)
      } else if (Channel isOperational chan) {
        setVis(isVisible = false, extraInfoText)
        visibleExcept(goneRes = -1)
      } else {
        visibleExcept(R.id.progressBars, R.id.paymentsInFlight, R.id.canReceive, R.id.canSend)
        val closeInfoRes = chan.data match { case c: DATA_CLOSING => closedBy(c) case _ => ln_info_shutdown }
        extraInfoText.setText(getString(closeInfoRes).html)
        setVis(isVisible = true, extraInfoText)
      }

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

      visibleExcept(R.id.refundableAmount)
      ChannelIndicatorLine.setView(chanState, chan)
      peerAddress.setText(hc.remoteInfo.address.toString)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      totalCapacityText.setText(sumOrNothing(capacity, cardIn).html)
      canReceiveText.setText(sumOrNothing(hc.availableForReceive.truncateToSatoshi, cardOut).html)
      canSendText.setText(sumOrNothing(hc.availableForSend.truncateToSatoshi, cardIn).html)

      paymentsInFlightText.setText(sumOrNothing(inFlight.truncateToSatoshi, cardIn).html)
      setVis(isVisible = hc.error.isDefined, extraInfoText)
      extraInfoText.setText(errorText)
      this
    }
  }

  override def onDestroy: Unit = {
    updateSubscription.foreach(_.unsubscribe)
    super.onDestroy
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

  private def sumOrNothing(amt: Satoshi, mainColor: String): String =
    if (0L.sat != amt) LNParams.denomination.parsedWithSign(amt.toMilliSatoshi, mainColor, cardZero)
    else getString(chan_nothing)

  private def closedBy(cd: DATA_CLOSING): Int = {
    if (cd.remoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.nextRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.futureRemoteCommitPublished.nonEmpty) ln_info_close_remote
    else if (cd.mutualClosePublished.nonEmpty) ln_info_close_coop
    else ln_info_close_local
  }

  private def updateChanData: TimerTask = UITask {
    csToDisplay = LNParams.cm.all.values.flatMap(Channel.chanAndCommitsOpt).toList
    chanAdapter.notifyDataSetChanged
  }
}
