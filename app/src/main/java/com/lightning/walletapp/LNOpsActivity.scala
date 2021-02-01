package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import java.io.{BufferedWriter, File, FileWriter}
import android.view.{Menu, MenuItem, View, ViewGroup}
import com.lightning.walletapp.ln.Tools.{none, random, wrap}
import org.bitcoinj.core.{Address, Block, FilteredBlock, Peer}
import fr.acinq.bitcoin.{MilliSatoshiLong, Satoshi, SatoshiLong}

import com.lightning.walletapp.lnutils.{ChannelTable, PaymentInfoWrap, PaymentTable}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.hostedStateCodec
import com.lightning.walletapp.lnutils.IconGetter.scrWidth
import com.lightning.walletapp.ln.PaymentInfo.REBALANCING
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.ln.RefundingData
import android.support.v4.content.FileProvider
import android.support.v7.widget.Toolbar
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.uri.BitcoinURI
import android.content.Intent
import scodec.bits.ByteVector
import android.os.Bundle
import java.util.Date
import scala.util.Try


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  def goHostedInfo(top: View) = host.browse("https://lightning-wallet.com/hosted-channels#hosted-channels")
  lazy val hostedChanActions = getResources.getStringArray(R.array.ln_hosted_chan_actions).map(_.html)
  lazy val normalChanActions = getResources.getStringArray(R.array.ln_normal_chan_actions).map(_.html)
  lazy val barStatus = app.getResources.getStringArray(R.array.ln_chan_ops_status)
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val displayedChans = ChannelManager.all.filter(canDisplayData)
  lazy val host = me

  val getTotalPayments = { chanId: ByteVector =>
    val cursor = LNParams.db.select(PaymentTable.selectPaymentNumSql, chanId)
    RichCursor(cursor) headTry { case RichCursor(c1) => c1 getLong 0 } getOrElse 0L
  }

  val adapter = new BaseAdapter {
    def getCount = displayedChans.size
    def getItemId(chanPosition: Int) = chanPosition
    def getItem(position: Int) = displayedChans(position)
    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      // Use View as well as ViewHolder caching to display chan cards as fast as possible
      // it may happen that HostedView gets on NormalChannel and vice versa: account for that
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.card_chan, null) else savedView

      val cardView = Tuple3(getItem(position), getItem(position).data, card.getTag) match {
        case (chan: HostedChannel, commits: HostedCommits, view: HostedViewHolder) => view.fill(chan, commits)
        case (chan: HostedChannel, commits: HostedCommits, _) => new HostedViewHolder(card).fill(chan, commits)
        case (chan: NormalChannel, commits: HasNormalCommits, view: NormalViewHolder) => view.fill(chan, commits.commitments)
        case (chan: NormalChannel, commits: HasNormalCommits, _) => new NormalViewHolder(card).fill(chan, commits.commitments)
        case _ => throw new RuntimeException
      }

      // Remember generated view
      card setTag cardView
      card
    }
  }

  val eventsListener = new ChannelListener with BlocksListener {
    override def onBecome = { case _ => UITask(adapter.notifyDataSetChanged).run }
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = if (left < 1) UITask(adapter.notifyDataSetChanged).run
  }

  abstract class ChanViewHolder(view: View) {
    val baseBar = view.findViewById(R.id.baseBar).asInstanceOf[ProgressBar]
    val overBar = view.findViewById(R.id.overBar).asInstanceOf[ProgressBar]
    val extraInfoText = view.findViewById(R.id.extraInfoText).asInstanceOf[TextView]
    val addressAndKey = view.findViewById(R.id.addressAndKey).asInstanceOf[TextView]
    val stateAndConnectivity = view.findViewById(R.id.stateAndConnectivity).asInstanceOf[TextView]
    def setExtraInfo(text: CharSequence) = wrap(extraInfoText setVisibility View.VISIBLE)(extraInfoText setText text)
    def setExtraInfo(resource: Int) = wrap(extraInfoText setVisibility View.VISIBLE)(extraInfoText setText resource)

    val wrappers =
      view.findViewById(R.id.refundableAmount).asInstanceOf[View] ::
        view.findViewById(R.id.hostedWarningHeader).asInstanceOf[View] ::
        view.findViewById(R.id.paymentsInFlight).asInstanceOf[View] ::
        view.findViewById(R.id.balancesDivider).asInstanceOf[View] ::
        view.findViewById(R.id.totalPayments).asInstanceOf[View] ::
        view.findViewById(R.id.totalCapacity).asInstanceOf[View] ::
        view.findViewById(R.id.fundingDepth).asInstanceOf[View] ::
        view.findViewById(R.id.canReceive).asInstanceOf[View] ::
        view.findViewById(R.id.startedAt).asInstanceOf[View] ::
        view.findViewById(R.id.refundFee).asInstanceOf[View] ::
        view.findViewById(R.id.closedAt).asInstanceOf[View] ::
        view.findViewById(R.id.canSend).asInstanceOf[View] ::
        baseBar :: overBar :: Nil

    val totalPaymentsText = view.findViewById(R.id.totalPaymentsText).asInstanceOf[TextView]
    val refundableAmountText = view.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText = view.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val totalCapacityText = view.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val fundingDepthText = view.findViewById(R.id.fundingDepthText).asInstanceOf[TextView]
    val canReceiveText = view.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val startedAtText = view.findViewById(R.id.startedAtText).asInstanceOf[TextView]
    val refundFeeText = view.findViewById(R.id.refundFeeText).asInstanceOf[TextView]
    val closedAtText = view.findViewById(R.id.closedAtText).asInstanceOf[TextView]
    val canSendText = view.findViewById(R.id.canSendText).asInstanceOf[TextView]
    baseBar setMax 1000
    overBar setMax 1000

    def visibleExcept(gone: Int*) =
      for (textWrapper <- wrappers) {
        val isGone = gone contains textWrapper.getId
        textWrapper setVisibility viewMap(!isGone)
      }

    def doNormalOperationalStateChecks(channel: Channel) = {
      val extraRoute = channelAndHop(channel) map { case _ \ route => route } getOrElse Vector.empty
      val isIncomingFeeTooHigh = extraRoute.nonEmpty && LNParams.isFeeBreach(extraRoute, 1000000000L, percent = 100L)
      if (isIncomingFeeTooHigh) setExtraInfo(text = me getString ln_info_high_fee format extraRoute.head.feeBreakdown)
      val hasStuckHtlcs = channel.inFlightHtlcs.exists(_.add.expiry <= LNParams.broadcaster.currentHeight)
      if (hasStuckHtlcs) setExtraInfo(resource = ln_info_stuck_htlcs)
    }
  }

  class NormalViewHolder(view: View) extends ChanViewHolder(view) {
    def fill(chan: NormalChannel, cs: NormalCommits): NormalViewHolder = {
      val fundingDepth \ lostFunding = LNParams.broadcaster.getStatus(chan.fundTxId)
      val threshold = math.max(cs.remoteParams.minimumDepth, LNParams.minDepth)
      val forceCloseFeeSat = Satoshi(cs.reducedRemoteState.myFeeSat)
      val startedAt = me time new Date(cs.startedAt)

      val capacitySat = cs.commitInput.txOut.amount
      // Bar reserve for incoming channels is only reserve since fee is zero, multipled by 1000 to match bar scaling
      val barLocalReserve = (forceCloseFeeSat.toLong + cs.remoteParams.channelReserveSatoshis) * 1000L / capacitySat.toLong
      val barCanSend = (cs.remoteCommit.spec.toRemoteMsat / capacitySat.toLong).toInt
      val barCanReceive = (chan.estCanReceiveMsat / capacitySat.toLong).toInt
      overBar.setProgress(barCanSend min barLocalReserve.toInt)
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      startedAtText setText startedAt.html
      fundingDepthText setText s"$fundingDepth / $threshold"
      addressAndKey setText chan.data.announce.asString.html
      totalPaymentsText setText getTotalPayments(cs.channelId).toString
      // All amounts are in MilliSatoshi, but we divide them by 1000 to erase trailing msat remainders
      stateAndConnectivity setText s"<strong>${me stateStatusColor chan}</strong><br>${me connectivityStatusColor chan}".html
      paymentsInFlightText setText sumOrNothing(chan.inFlightHtlcs.toVector.map(_.add.amountMsat).sum.fromMsatToSat).html
      canReceiveText setText denom.parsedWithSign(chan.estCanReceiveMsat.toTruncatedMsat).html
      canSendText setText denom.parsedWithSign(chan.estCanSendMsat.toTruncatedMsat).html
      refundableAmountText setText sumOrNothing(chan.refundableMsat.fromMsatToSat).html
      totalCapacityText setText denom.parsedWithSign(capacitySat).html
      refundFeeText setText sumOrNothing(forceCloseFeeSat).html
      // Always reset extra info to GONE at first
      extraInfoText setVisibility View.GONE

      chan.data match {
        case norm: NormalData if isOperational(chan) =>
          doNormalOperationalStateChecks(channel = chan)
          if (lostFunding) setExtraInfo(resource = ln_info_funding_lost)
          visibleExcept(gone = R.id.fundingDepth, R.id.closedAt, R.id.hostedWarningHeader)

        case norm: NormalData =>
          setExtraInfo(resource = ln_info_coop_attempt)
          // If unknown spend is present then channel is not operational
          if (norm.unknownSpend.isDefined) setExtraInfo(resource = ln_info_unknown_spend)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.refundFee, R.id.fundingDepth, R.id.closedAt, R.id.hostedWarningHeader)

        case _: NegotiationsData =>
          setExtraInfo(resource = ln_info_coop_attempt)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.refundFee, R.id.fundingDepth, R.id.closedAt, R.id.hostedWarningHeader)

        case wait: WaitBroadcastRemoteData =>
          if (lostFunding) setExtraInfo(resource = ln_info_funding_lost)
          if (wait.fundingError.isDefined) setExtraInfo(text = wait.fundingError.get)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.closedAt, R.id.paymentsInFlight, R.id.totalPayments, R.id.hostedWarningHeader)

        case _: WaitFundingDoneData =>
          if (lostFunding) setExtraInfo(resource = ln_info_funding_lost)
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.closedAt, R.id.paymentsInFlight, R.id.totalPayments, R.id.hostedWarningHeader)

        case cd: ClosingData =>
          setExtraInfo(text = me closedBy cd)
          val closeDate = new Date(cd.closedAt)
          closedAtText setText time(closeDate).html

          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.fundingDepth, R.id.paymentsInFlight, R.id.refundableAmount, R.id.refundFee,
            R.id.balancesDivider, R.id.totalPayments, R.id.hostedWarningHeader)

        case _ =>
          visibleExcept(gone = R.id.baseBar, R.id.overBar, R.id.canSend, R.id.canReceive,
            R.id.fundingDepth, R.id.paymentsInFlight, R.id.refundableAmount, R.id.refundFee,
            R.id.balancesDivider, R.id.totalPayments, R.id.closedAt, R.id.hostedWarningHeader)
      }

      // MENU PART

      view setOnClickListener onButtonTap {
        def closeBitcoinAddress: Option[Address] =
          Try(app.TransData parse app.getBufferUnsafe).toOption
            .collectFirst { case uri: BitcoinURI => uri.getAddress }

        val addressHint = getString(ln_chan_close_address) -> closeBitcoinAddress match {
          case base \ Some(bitcoinAddress) => base format humanSix(bitcoinAddress.toString)
          case base \ _ => base format getString(ln_chan_close_address_hint)
        }

        val lst \ alert = makeChoiceList(chan.data match {
          // Unknown spend may be our own future commit, don't allow force-closing here
          case norm: NormalData if norm.unknownSpend.isDefined => normalChanActions take 1
          // Remote funding may not be visible yet, channel will be removed automatically later
          case _: WaitBroadcastRemoteData => normalChanActions take 1
          // Spending current commit here would be a channel breach
          case _: RefundingData => normalChanActions take 1
          // No reason to close an already closed channel
          case _: ClosingData => normalChanActions take 1
          case _ => normalChanActions :+ addressHint.html
        }, chan.data.announce.asString.html)

        lst setOnItemClickListener onTap { pos =>
          def warnAndMaybeClose(channelClosureWarning: String) = {
            val bld = baseTextBuilder(channelClosureWarning.html).setCustomTitle(chan.data.announce.asString.html)
            mkCheckForm(alert => rm(alert)(chan process ChannelManager.CMDLocalShutdown), none, bld, dialog_ok, dialog_cancel)
          }

          def warnAndMaybeCloseToAddress = closeBitcoinAddress map { bitcoinAddress =>
            val text = getString(ln_chan_close_confirm_address) format humanSix(bitcoinAddress.toString)
            val bld = baseTextBuilder(text.html).setCustomTitle(chan.data.announce.asString.html)
            val program = ScriptBuilder.createOutputScript(bitcoinAddress).getProgram
            val customShutdown = CMDShutdown apply Some(ByteVector view program)
            mkCheckForm(alert => rm(alert)(chan process customShutdown),
              none, bld, dialog_ok, dialog_cancel)
          }

          rm(alert) {
            val noHtlcBlock = chan.inFlightHtlcs.isEmpty
            val canCoopClose = isOpeningOrOperational(chan)
            if (0 == pos) host.browse(s"https://mempool.space/tx/${chan.fundTxId.toHex}")
            else if (1 == pos && canCoopClose && noHtlcBlock) warnAndMaybeClose(me getString ln_chan_close_confirm_wallet)
            else if (1 == pos && canCoopClose) warnAndMaybeClose(me getString ln_chan_close_inflight_details)
            else if (1 == pos) warnAndMaybeClose(channelClosureWarning = me getString ln_chan_force_details)
            else if (2 == pos && closeBitcoinAddress.isEmpty) host toast ln_chan_close_address_hint
            else if (2 == pos && canCoopClose && noHtlcBlock) warnAndMaybeCloseToAddress
            else if (2 == pos) host toast ln_chan_close_address_no
          }
        }
      }

      this
    }
  }

  class HostedViewHolder(view: View) extends ChanViewHolder(view) {
    def fill(chan: HostedChannel, hc: HostedCommits): HostedViewHolder = {
      val capacitySat = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat.fromMsatToSat
      val startedAt = me time new Date(hc.startedAt)

      val barCanSend = (chan.estCanSendMsat / capacitySat.toLong).toInt
      val barCanReceive = (chan.estCanReceiveMsat / capacitySat.toLong).toInt
      baseBar.setSecondaryProgress(barCanSend + barCanReceive)
      baseBar.setProgress(barCanSend)

      startedAtText setText startedAt.html
      addressAndKey setText chan.data.announce.asString.html
      totalPaymentsText setText getTotalPayments(hc.channelId).toString
      // All amounts are in MilliSatoshi, but we divide them by 1000 to erase trailing msat remainders
      stateAndConnectivity setText s"<strong>${me stateStatusColor chan}</strong><br>${me connectivityStatusColor chan}".html
      paymentsInFlightText setText sumOrNothing(chan.inFlightHtlcs.toVector.map(_.add.amountMsat).sum.fromMsatToSat).html
      canReceiveText setText denom.parsedWithSign(chan.estCanReceiveMsat.toTruncatedMsat).html
      canSendText setText denom.parsedWithSign(chan.estCanSendMsat.toTruncatedMsat).html
      totalCapacityText setText denom.parsedWithSign(capacitySat).html
      // Always reset extra info to GONE at first
      extraInfoText setVisibility View.GONE

      hc.localError -> hc.remoteError match {
        case _ \ Some(err) => setExtraInfo(s"Remote: ${ChanErrorCodes translateTag err}")
        case Some(err) \ _ => setExtraInfo(s"Local: ${ChanErrorCodes translateTag err}")
        case _ => doNormalOperationalStateChecks(chan)
      }

      visibleExcept(gone = R.id.fundingDepth, R.id.closedAt,
        R.id.balancesDivider, R.id.refundableAmount, R.id.refundFee,
        R.id.overBar)

      view setOnClickListener onButtonTap {
        val title = chan.data.announce.asString.html
        val lst \ alert = makeChoiceList(hostedChanActions, title)

        lst setOnItemClickListener onTap { pos =>
          def warnAndMaybeRemove(removalWarning: String) = {
            val bld = baseTextBuilder(removalWarning.html).setCustomTitle(title)
            mkCheckForm(alert => rm(alert)(removeChan), none, bld, dialog_ok, dialog_cancel)
          }

          def removeChan = {
            // This activity only works fine if channels are intact so exit on removal
            // We also need to disconnect from peer because they still have NORMAL state
            ConnectionManager.workers.get(hc.announce.nodeId).foreach(_.disconnect)
            ChannelManager.all = ChannelManager.all diff Vector(chan)
            LNParams.db.change(ChannelTable.killSql, hc.channelId)
            PaymentInfoWrap unknownHostedHtlcsDetected hc
            finish
          }

          def shareStateAsFile = {
            val snapshotFilePath = new File(getCacheDir, "images")
            if (!snapshotFilePath.isFile) snapshotFilePath.mkdirs

            val preimages = hc.sentPreimages.toMap.keys.mkString("\n")
            val data = hostedStateCodec.encode(hc.hostedState).require.toHex
            val text = getString(ln_hosted_state_wrap).format(hc.channelId.toHex, data, preimages)
            val savedFile = new File(snapshotFilePath, s"Hosted channel state ${new Date}.txt")
            val writer = new FileWriter(savedFile)
            val bw = new BufferedWriter(writer)
            bw.write(text)
            bw.close

            val fileURI = FileProvider.getUriForFile(me, "com.lightning.walletapp", savedFile)
            val share = new Intent setAction Intent.ACTION_SEND addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
            share.putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
            me startActivity Intent.createChooser(share, "Choose an app")
          }

          rm(alert) {
            if (1 == pos) shareStateAsFile
            else if (0 == pos) goHostedInfo(null)
            else if (2 == pos && chan.inFlightHtlcs.nonEmpty) warnAndMaybeRemove(me getString ln_hosted_remove_inflight_details)
            else if (2 == pos && chan.estCanSendMsat.fromMsatToSat > LNParams.dust) warnAndMaybeRemove(me getString ln_hosted_remove_non_empty_details)
            else if (2 == pos) removeChan
          }
        }
      }

      this
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.lnops, menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    if (m.getItemId == R.id.actionAddNodeId) share(LNParams.keys.extendedNodeKey.publicKey.toString)
    else if (m.getItemId == R.id.actionDrainHosted) drainHostedChan
    true
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.peerGroup removeBlocksDownloadedEventListener eventsListener
    for (chan <- displayedChans) chan.listeners -= eventsListener
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_ln_ops
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setSubtitle app.plur1OrZero(barStatus, displayedChans.size)
    getSupportActionBar setTitle action_ln_details

    gridView setAdapter adapter
    gridView setNumColumns math.round(scrWidth / 2.4).toInt
    app.kit.peerGroup addBlocksDownloadedEventListener eventsListener
    for (chan <- displayedChans) chan.listeners += eventsListener
  } else me exitTo classOf[MainActivity]

  def drainHostedChan = {
    val hosted \ normal = ChannelManager.all.partition(_.isHosted)
    val hosted1 = hosted.filter(hostedChan => isOperational(hostedChan) && hostedChan.estCanSendMsat.fromMsatToSat > LNParams.dust)
    val normal1 = normal.filter(chan => isOperational(chan) && chan.estCanReceiveMsat.fromMsatToSat > LNParams.dust && channelAndHop(chan).nonEmpty)

    if (hosted1.isEmpty) me toast err_ln_drain_no_hosted
    else if (normal1.isEmpty) me toast err_ln_drain_no_normal
    else {
      val preimage = ByteVector(random getBytes 32)
      val largestCanSendMsat = hosted1.map(_.estCanSendMsat).sorted.last
      val maxFee = LNParams.maxAcceptableFee(largestCanSendMsat, hops = 5)
      val normalExtraRoutes = normal1 flatMap channelAndHop collect { case _ \ normalExtraHop => normalExtraHop }
      val transferSum = math.min((normal1.map(_.estCanReceiveMsat).sorted.head * 0.99).toLong, largestCanSendMsat - maxFee).millisatoshi
      val rd = PaymentInfoWrap.recordRoutingDataWithPr(normalExtraRoutes, transferSum, preimage, REBALANCING).copy(isRebalancing = true)
      PaymentInfoWrap addPendingPayment rd.copy(fromHostedOnly = true)
      me toast dialog_hosted_draining
    }
  }

  // UTILS

  def stateStatusColor(c: Channel) = (c.data, c.state) match {
    case (_: HostedCommits, OPEN) => me getString ln_info_status_open
    case (_: HostedCommits, SUSPENDED) => me getString ln_info_status_suspended
    case (_: NormalData, OPEN) if isOperational(c) => me getString ln_info_status_open
    case (_: NormalData, _) if !isOperational(c) => me getString ln_info_status_shutdown
    case (_: HasNormalCommits, WAIT_FUNDING_DONE) => me getString ln_info_status_opening
    case (_: HasNormalCommits, NEGOTIATIONS) => me getString ln_info_status_negotiations
    case _ => me getString ln_info_status_other format c.state
  }

  def canDisplayData(c: Channel): Boolean = c.data match {
    case refund: RefundingData => refund.remoteLatestPoint.isDefined
    case cd: ClosingData => cd.bestClosing.getDepth <= LNParams.minDepth
    case _: HasNormalCommits => true
    case _: HostedCommits => true
    case _ => false
  }

  def sumOrNothing(amt: Satoshi) = amt match {
    case Satoshi(0L) => getString(ln_info_nothing)
    case _ => denom parsedWithSign amt
  }

  def connectivityStatusColor(chan: Channel) =
    ConnectionManager.workers get chan.data.announce.nodeId match {
      case Some(w) if w.sock.isConnected => me getString ln_info_state_online
      case _ => me getString ln_info_state_offline
    }

  def closedBy(cd: ClosingData) =
    if (cd.remoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.nextRemoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.refundRemoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.mutualClose.nonEmpty) me getString ln_info_close_coop
    else me getString ln_info_close_local
}