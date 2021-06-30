package com.btcontract.wallettest

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import immortan.crypto.Tools._
import scala.concurrent.duration._
import com.btcontract.wallettest.Colors._
import com.btcontract.wallettest.R.string._
import android.widget.{BaseAdapter, LinearLayout, ListView}
import immortan.{Channel, ChannelMaster, LNParams}
import android.view.{View, ViewGroup}
import rx.lang.scala.Subscription
import android.os.Bundle
import immortan.utils.Rx


class StatActivity extends BaseActivity { me =>
  private[this] var updateSubscription = Option.empty[Subscription]
  private[this] lazy val chanList = findViewById(R.id.chanList).asInstanceOf[ListView]

  val chanAdapter: BaseAdapter = new BaseAdapter {
    override def getView(position: Int, savedView: View, parent: ViewGroup): View = savedView
    override def getItem(pos: Int): Channel = LNParams.cm.all.values.toList(pos)
    override def getItemId(position: Int): Long = position
    override def getCount: Int = 0
  }

  override def onDestroy: Unit = {
    updateSubscription.foreach(_.unsubscribe)
    super.onDestroy
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_stat)

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
            slotTitle.addChipText(getString(stats_item_transactions).format(summary.count), R.drawable.border_yellow)
            slotTitle.addChipText(getString(stats_item_received) format LNParams.denomination.directedWithSign(summary.received, 0L.sat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_sent) format LNParams.denomination.directedWithSign(0L.sat, summary.sent, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.sat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- paymentSummary) {
            val slotTitle = new TitleView(me getString stats_title_ln)
            slotTitle.addChipText(getString(stats_item_payments).format(summary.count), R.drawable.border_blue)
            slotTitle.addChipText(getString(stats_item_received) format LNParams.denomination.directedWithSign(summary.received, 0L.msat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_sent) format LNParams.denomination.directedWithSign(0L.msat, summary.sent, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.msat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_fees_saved) format LNParams.denomination.parsedWithSign(summary.chainFees - summary.fees, cardIn, cardZero), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- relaySummary) {
            val slotTitle = new TitleView(me getString stats_title_relays)
            slotTitle.addChipText(getString(stats_item_relays).format(summary.count), R.drawable.border_blue)
            slotTitle.addChipText(getString(stats_item_relayed) format LNParams.denomination.parsedWithSign(summary.relayed, cardIn, cardZero), R.drawable.border_gray)
            slotTitle.addChipText(getString(stats_item_earned) format LNParams.denomination.directedWithSign(summary.earned, 0L.msat, cardOut, cardIn, cardZero, isPlus = true), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }

          for (summary <- channelTxFeesSummary) {
            val slotTitle = new TitleView(me getString stats_title_chan_loss)
            slotTitle.addChipText(getString(stats_item_transactions).format(summary.count), R.drawable.border_yellow)
            slotTitle.addChipText(getString(stats_item_fees) format LNParams.denomination.directedWithSign(0L.sat, summary.fees, cardOut, cardIn, cardZero, isPlus = false), R.drawable.border_gray)
            statList.addView(slotTitle.view)
          }
        }
      }

      val window = 500.millis
      val stateEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.stateUpdateStream, window)
      val statusEvents = Rx.uniqueFirstAndLastWithinWindow(ChannelMaster.statusUpdateStream, window)
      updateSubscription = stateEvents.merge(statusEvents).subscribe(_ => UITask(chanAdapter.notifyDataSetChanged).run).asSome
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
}
