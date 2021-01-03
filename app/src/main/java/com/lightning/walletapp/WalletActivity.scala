package com.lightning.walletapp

import spray.json._
import android.view._
import com.lightning.walletapp.ln._
import android.text.format.DateUtils._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.LNUrl._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import scala.util.{Success, Try}
import org.bitcoinj.core.{Block, FilteredBlock, Peer}
import com.lightning.walletapp.lnutils.IconGetter.{bigFont, scrWidth}
import com.lightning.walletapp.lnutils.{LocalBackup, PaymentInfoWrap}
import com.lightning.walletapp.ln.crypto.Sphinx.DecryptedFailurePacket
import io.github.douglasjunior.androidSimpleTooltip.SimpleTooltip
import com.lightning.walletapp.lnutils.JsonHttpUtils.queue
import android.support.v4.app.FragmentStatePagerAdapter
import org.ndeftools.util.activity.NfcReaderActivity
import com.lightning.walletapp.helper.AwaitService
import android.support.v4.content.ContextCompat
import me.relex.circleindicator.CircleIndicator
import com.github.clans.fab.FloatingActionMenu
import android.support.v7.widget.SearchView
import org.bitcoinj.script.ScriptBuilder
import fr.acinq.bitcoin.Crypto.PublicKey
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import android.app.AlertDialog
import scodec.bits.ByteVector
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date


trait SearchBar { me =>
  var isSearching = false
  var lastQuery = new String
  var searchView: SearchView = _

  def setupSearch(m: Menu) = {
    searchView = m.findItem(R.id.action_search).getActionView.asInstanceOf[SearchView]
    searchView addOnAttachStateChangeListener new View.OnAttachStateChangeListener {
      def onViewDetachedFromWindow(lens: View) = runAnd(isSearching = false)(react)
      def onViewAttachedToWindow(lens: View) = runAnd(isSearching = true)(react)
    }

    searchView setOnQueryTextListener new SearchView.OnQueryTextListener {
      def onQueryTextChange(txt: String) = runAnd(true)(me search txt)
      def onQueryTextSubmit(txt: String) = true
    }
  }

  def react: Unit
  def search(txt: String) = {
    // Update and do the search
    lastQuery = txt
    react
  }
}

trait HumanTimeDisplay {
  lazy val timeString = DateFormat is24HourFormat host match {
    case false if scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"
    case false => "MMM dd, yyyy' <small>'h:mma'</small>'"

    case true if scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true => "d MMM yyyy' <small>'HH:mm'</small>'"
  }

  val host: TimerActivity
  val time: Date => String = new SimpleDateFormat(timeString) format _
  def when(now: Long, thenDate: Date) = thenDate.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(thenDate)
  }

  def initToolbar(toolbar: android.support.v7.widget.Toolbar) = {
    // Show back arrow button to allow users to get back to wallet
    // just kill current activity once a back button is tapped

    host.setSupportActionBar(toolbar)
    host.getSupportActionBar.setDisplayHomeAsUpEnabled(true)
    host.getSupportActionBar.setDisplayShowHomeEnabled(true)
    toolbar.setNavigationOnClickListener(host onButtonTap host.finish)
  }
}

class WalletActivity extends NfcReaderActivity with ScanActivity { me =>
  lazy val floatingActionMenu = findViewById(R.id.fam).asInstanceOf[FloatingActionMenu]
  lazy val positionIndicator = findViewById(R.id.positionIndicator).asInstanceOf[CircleIndicator]

  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(pos: Int) = if (0 == pos) new FragPayMarket else if (1 == pos) new FragWallet else new FragScan
    def getCount = 3
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onResume = wrap(super.onResume)(walletPager setCurrentItem 1)
  override def onOptionsItemSelected(menu: MenuItem): Boolean = runAnd(true) {
    if (menu.getItemId == R.id.actionSettings) me goTo classOf[SettingsActivity]
  }

  override def onBackPressed = {
    val isExpanded = null != FragWallet.worker && FragWallet.worker.currentCut > FragWallet.worker.minLinesNum
    val isPayLinkSerching = null != FragPayMarket.worker && FragPayMarket.worker.paySearch.getQuery.nonEmpty
    if (0 == walletPager.getCurrentItem && isPayLinkSerching) FragPayMarket.worker.paySearch.clearQuery
    else if (1 != walletPager.getCurrentItem) walletPager.setCurrentItem(1, true)
    else if (floatingActionMenu.isOpened) floatingActionMenu close true
    else if (isExpanded) FragWallet.worker.toggler.performClick
    else super.onBackPressed
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after worker sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.wallet, menu)

    // Worker is definitely not null
    FragWallet.worker.setupSearch(menu)
    FragWallet.worker.searchView.setQueryHint(app getString search_hint_payments)
    val openAutoHostedChan = false // app.prefs.getBoolean(AbstractKit.AUTO_HOSTED_CHAN, false)
    val showTooltip = app.prefs.getBoolean(AbstractKit.SHOW_TOOLTIP, true)

    if (openAutoHostedChan) {
      var hasDefaultHosted = ChannelManager.hasHostedChanWith(FragLNStart.defaultHostedNode.ann.nodeId)
      if (hasDefaultHosted) app.prefs.edit.putBoolean(AbstractKit.AUTO_HOSTED_CHAN, false).commit else {
        val refundScriptPubKey: ByteVector = ByteVector(ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram)
        val waitData = WaitRemoteHostedReply(FragLNStart.defaultHostedNode.ann, refundScriptPubKey, FragLNStart.defaultHostedNode.secret)
        val freshChannel = ChannelManager.createHostedChannel(Set.empty, waitData)

        lazy val hostedChanOpenListener = new ConnectionListener with ChannelListener {
          override def onDisconnect(nodeId: PublicKey) = if (nodeId == FragLNStart.defaultHostedNode.ann.nodeId) detachAll(retryOnRestart = true)
          override def onOperational(nodeId: PublicKey, isCompat: Boolean) = if (nodeId == FragLNStart.defaultHostedNode.ann.nodeId && isCompat) freshChannel.startUp
          override def onHostedMessage(ann: NodeAnnouncement, message: HostedChannelMessage) = if (ann.nodeId == FragLNStart.defaultHostedNode.ann.nodeId) freshChannel process message

          override def onMessage(nodeId: PublicKey, message: LightningMessage) = message match {
            case upd: ChannelUpdate if nodeId == FragLNStart.defaultHostedNode.ann.nodeId && upd.isHosted => freshChannel process upd
            case error: Error if nodeId == FragLNStart.defaultHostedNode.ann.nodeId => freshChannel process error
            case _ => super.onMessage(nodeId, message)
          }

          override def onBecome = {
            case (_: HostedChannel, _, WAIT_FOR_ACCEPT, OPEN | SUSPENDED) =>
              // Hosted channel is now established and stored, may contain error
              FragWallet.worker.reg(freshChannel)
              detachAll(retryOnRestart = false)
          }

          override def onException = {
            case (_: HostedChannel, openingError) =>
              UITask(app quickToast openingError.getMessage).run
              detachAll(retryOnRestart = false)
          }
        }

        lazy val chainListener = new BlocksListener {
          def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = if (!hasDefaultHosted) {
            // First attach listeners and then connect to remote node because connection may be there already
            ConnectionManager.listeners += hostedChanOpenListener
            freshChannel.listeners += hostedChanOpenListener

            ConnectionManager.connectTo(FragLNStart.defaultHostedNode.ann, notify = true)
            // Method may be called many times even if removed so guard with boolean condition
            hasDefaultHosted = true
          }
        }

        def detachAll(retryOnRestart: Boolean): Unit = {
          app.prefs.edit.putBoolean(AbstractKit.AUTO_HOSTED_CHAN, retryOnRestart).commit
          app.kit.peerGroup removeBlocksDownloadedEventListener chainListener
          ConnectionManager.listeners -= hostedChanOpenListener
          freshChannel.listeners -= hostedChanOpenListener
        }

        // First obtain current chain height, then try to get a channel
        app.kit.peerGroup addBlocksDownloadedEventListener chainListener
      }
    }

    if (showTooltip) try {
      app.prefs.edit.putBoolean(AbstractKit.SHOW_TOOLTIP, false).commit
      val tip = new SimpleTooltip.Builder(me).anchorView(floatingActionMenu.getMenuIconView)
      tip.text("Menu").gravity(Gravity.START).transparentOverlay(false).animated(true).build.show
    } catch none
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    wrap(me setDetecting true)(me initNfc state)
    me setContentView R.layout.activity_pager

    walletPager.setAdapter(slidingFragmentAdapter)
    walletPager.setOffscreenPageLimit(2)
    walletPager.setCurrentItem(1, false)

    positionIndicator setViewPager walletPager
    positionIndicator setOnClickListener onButtonTap {
      // Send to scanner if tapped out of curiosity
      walletPager.setCurrentItem(2, true)
    }

    PaymentInfoWrap.newRoutesOrGiveUp = rd =>
      if (rd.callsLeft > 0 && ChannelManager.checkIfSendable(rd).isRight) {
        // We do not care about options such as AIR or AMP here, this HTLC may be one of them
        PaymentInfoWrap fetchAndSend rd.copy(callsLeft = rd.callsLeft - 1, useCache = false)
      } else {
        // Our direct peers likely have no liquidity at the moment
        val allChanFailsFromPeers = PaymentInfo.errors(rd.pr.paymentHash) forall {
          // Ensure failed route is not like A -> B [peer] -> C because peer may send temp failure for these but it could be C's fault
          case DecryptedFailurePacket(origin, _: TemporaryChannelFailure) \ route => route.size > 1 && origin == rd.nextNodeId(route)
          case DecryptedFailurePacket(origin, PermanentChannelFailure) \ route => route.size > 1 && origin == rd.nextNodeId(route)
          case _ => false
        }

        if (allChanFailsFromPeers) UITask(me toast err_ln_peer_can_not_route).run
        // Too many attempts and still no luck so we give up on payment for now
        PaymentInfoWrap.updStatus(PaymentInfo.FAILURE, rd.pr.paymentHash)
      }

    PaymentInfoWrap.failOnUI = rd => {
      PaymentInfoWrap.unsentPayments -= rd.pr.paymentHash
      PaymentInfoWrap.updStatus(PaymentInfo.FAILURE, rd.pr.paymentHash)
      if (rd.expensiveScids.nonEmpty) UITask(me toast ln_fee_expensive_omitted).run
      PaymentInfoWrap.uiNotify
    }

    val backupAllowed = LocalBackup.isAllowed(activity = me)
    if (!backupAllowed) LocalBackup.askPermission(activity = me)
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app quickToast err_nothing_useful
  def readNonNdefMessage = app quickToast err_nothing_useful
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(nfcMessage: Message) =
    <(app.TransData recordValue ndefMessageString(nfcMessage),
      error => app quickToast err_nothing_useful)(ok => checkTransData)

  // EXTERNAL DATA CHECK

  def checkTransData: Unit = {
    walletPager.setCurrentItem(1, false)

    app.TransData checkAndMaybeErase {
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case FragWallet.OPEN_RECEIVE_MENU => goReceivePayment(null): Unit
      case FragWallet.REDIRECT => goOps(null): Unit

      case btcURI: BitcoinURI =>
        val canSendOffChain = Try(btcURI.getAmount).map(coin2MSat).filter(msat => ChannelManager.estimateAIRCanSend >= msat.amount).isSuccess
        if (canSendOffChain && btcURI.getLightningRequest != null) <(app.TransData recordValue btcURI.getLightningRequest, onFail)(_ => checkTransData)
        else FragWallet.worker.sendBtcPopup(btcURI)

      case lnUrl: LNUrl =>
        if (lnUrl.isAuth) showAuthForm(lnUrl)
        else if (lnUrl.withdrawAttempt.isSuccess) me doReceivePayment Some(lnUrl -> lnUrl.withdrawAttempt.get)
        else lnUrl.lnUrlAndDataObs.doOnSubscribe(app quickToast ln_url_resolving).foreach(data => UITask(me resolveLNUrl data).run, onFail)

      case pr: PaymentRequest =>
        val ourNetPrefix = PaymentRequest.prefixes(LNParams.chainHash)
        if (ourNetPrefix != pr.prefix) app quickToast err_nothing_useful
        else if (!pr.isFresh) app quickToast dialog_pr_expired
        else FragWallet.worker.standardOffChainSend(pr)

      case _ =>
    }
  }

  // LNURL

  val resolveLNUrl: PartialFunction[LNUrlAndData, Unit] = {
    case (lnUrl, response: PayRequest) => FragWallet.worker.lnurlPayOffChainSend(lnUrl, response)
    case (lnUrl, response: WithdrawRequest) => me doReceivePayment Some(lnUrl -> response)
    case (_, response: IncomingChannelRequest) => me initIncoming response
    case (_, response: HostedChannelRequest) => me goLNStartFund response
    case _ => app quickToast err_nothing_useful
  }

  def goLNStartFund(data: Any) = {
    me goTo classOf[LNStartFundActivity]
    app.TransData.value = data
  }

  def initIncoming(incoming: IncomingChannelRequest) = {
    val initialListener = new ConnectionListener { self =>
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == incoming.pubKey) ConnectionManager.listeners -= self
      override def onOperational(nodeId: PublicKey, isCompatible: Boolean) = if (nodeId == incoming.pubKey && isCompatible) {
        queue.map(_ => incoming.requestChannel.body).map(LNUrl.guardResponse).foreach(none, onFail)
      }

      override def onMessage(nodeId: PublicKey, message: LightningMessage) = message match {
        case open: OpenChannel if !open.channelFlags.isPublic => onOpenOffer(nodeId, open)
        case _ => // Ignore anything else including public channel offers
      }

      override def onOpenOffer(nodeId: PublicKey, open: OpenChannel) = {
        val incomingTip = app getString ln_ops_start_fund_incoming_channel
        val hnv = HardcodedNodeView(incoming.ann, incomingTip)
        me goLNStartFund IncomingChannelParams(hnv, open)
        ConnectionManager.listeners -= self
      }
    }

    if (ChannelManager hasNormalChanWith incoming.ann.nodeId) {
      // TODO: remove this limitation once random shortId is merged
      me toast err_ln_chan_exists_already
    } else {
      ConnectionManager.listeners += initialListener
      ConnectionManager.connectTo(incoming.ann, notify = true)
    }
  }

  def showAuthForm(lnUrl: LNUrl) = lnUrl.k1 foreach { k1 =>
    val linkingPrivKey = LNParams.keys.makeLinkingKey(lnUrl.uri.getHost)
    val linkingPubKey = linkingPrivKey.publicKey.toString
    val dataToSign = ByteVector.fromValidHex(k1)

    def wut(alert: AlertDialog): Unit = {
      val explanation = getString(ln_url_info_login).format(lnUrl.uri.getHost, Utils humanSix linkingPubKey).html
      mkCheckFormNeutral(_.dismiss, none, _ => share(linkingPubKey), baseTextBuilder(explanation), dialog_ok, -1, dialog_share)
    }

    def doAuth(alert: AlertDialog) = rm(alert) {
      val secondLevelRequestUri = lnUrl.uri.buildUpon.appendQueryParameter("sig", Tools.sign(dataToSign, linkingPrivKey).toHex).appendQueryParameter("key", linkingPubKey)
      queue.map(_ => get(secondLevelRequestUri.build.toString, false).connectTimeout(15000).body).map(LNUrl.guardResponse).foreach(raw => onAuthSuccess(raw).run, onFail)
      app quickToast ln_url_resolving
    }

    def onAuthSuccess(raw: String) = UITask {
      val msg = raw.parseJson.asJsObject.fields get "event" collect {
        case JsString("REGISTERED") => ln_url_registration_ok
        case JsString("AUTHED") => ln_url_authorization_ok
        case JsString("LOGGEDIN") => ln_url_login_ok
        case JsString("LINKED") => ln_url_linking_ok
      }

      val finalMessage = getString(msg getOrElse ln_url_authorization_ok)
      val bld = baseBuilder(title = lnUrl.uri.getHost, body = finalMessage)
      mkCheckForm(alert => rm(alert)(none), finish, bld, dialog_ok, dialog_close)
    }

    if (32 == dataToSign.length) {
      val title = updateView2Blue(oldView = str2View(new String), s"<big>${lnUrl.uri.getHost}</big>")
      mkCheckFormNeutral(doAuth, none, wut, baseBuilder(title, null), dialog_login, dialog_cancel, dialog_info)
    } else onFail(s"Challenge=$k1 is not a valid 32-byte hex-encoded string")
  }

  // BUTTONS REACTIONS

  def doReceivePayment(extra: Option[LNUrlAndWithdraw] = None) = {
    val viableChannels = ChannelManager.all.filter(isOpeningOrOperational)
    val withRoutes = viableChannels.filter(isOperational).flatMap(channelAndHop).toMap

    // For now we are bounded to single largest channel
    val receivables = withRoutes.keys.map(_.estCanReceiveMsat)
    val largestOne = if (receivables.isEmpty) 0L else receivables.max
    val maxCanReceive = MilliSatoshi(largestOne)

    // maxCanReceive may be negative, show a warning to user in this case
    val humanShouldSpend = s"<strong>${denom parsedWithSign -maxCanReceive}</strong>"
    val reserveUnspentWarning = getString(ln_receive_reserve) format humanShouldSpend

    extra match {
      case Some(lnUrl \ wr) =>
        val title = updateView2Blue(str2View(new String), app getString ln_receive_title)
        val finalMaxCanReceiveCapped = MilliSatoshi(wr.maxWithdrawable min maxCanReceive.amount)

        if (viableChannels.isEmpty) showForm(negTextBuilder(dialog_ok, getString(ln_receive_howto).html).create)
        else if (withRoutes.isEmpty) showForm(negTextBuilder(dialog_ok, getString(ln_receive_6conf).html).create)
        else if (maxCanReceive.amount < 0L) showForm(negTextBuilder(dialog_ok, reserveUnspentWarning.html).create)
        else FragWallet.worker.receive(withRoutes, finalMaxCanReceiveCapped, MilliSatoshi(wr.minCanReceive), title, wr.defaultDescription) { rd =>
          app.foregroundServiceIntent.putExtra(AwaitService.SHOW_AMOUNT, denom asString rd.pr.amount.get).setAction(AwaitService.SHOW_AMOUNT)
          queue.map(_ => wr.requestWithdraw(lnUrl, rd.pr).body).map(LNUrl.guardResponse).foreach(none, onFail)
          ContextCompat.startForegroundService(app, app.foregroundServiceIntent)
        }

      case None =>
        val alertLNHint =
          if (viableChannels.isEmpty) getString(ln_receive_suggestion)
          else if (withRoutes.isEmpty) getString(ln_receive_6conf)
          else if (maxCanReceive.amount < 0L) reserveUnspentWarning
          else getString(ln_receive_ok)

        val actions = Array(me getString ln_receive_option format alertLNHint, me getString btc_receive_option)
        val lst \ alert = makeChoiceList(actions.map(_.html), me getString action_coins_receive)
        lst setOnItemClickListener onTap { case 0 => offChain case 1 => onChain }

        def offChain = rm(alert) {
          if (viableChannels.isEmpty) showForm(negTextBuilder(dialog_ok, app.getString(ln_receive_howto).html).create)
          else FragWallet.worker.receive(withRoutes, maxCanReceive, MilliSatoshi(LNParams.minPaymentMsat), app.getString(ln_receive_title).html, new String) { rd =>
            app.foregroundServiceIntent.putExtra(AwaitService.SHOW_AMOUNT, denom asString rd.pr.amount.get).setAction(AwaitService.SHOW_AMOUNT)
            ContextCompat.startForegroundService(app, app.foregroundServiceIntent)
            me PRQR rd.pr
          }
        }

        def onChain = rm(alert) {
          app.TransData.value = app.kit.currentAddress
          me goTo classOf[RequestActivity]
        }
    }
  }

  def PRQR(pr: PaymentRequest) = {
    me goTo classOf[RequestActivity]
    app.TransData.value = pr
  }

  def goSendPaymentForm(top: View) = {
    val actions = Array(send_paste_payment_request, send_hivemind_deposit)
    val lst \ alert = makeChoiceList(actions.map(getString).map(_.html), me getString action_coins_send)
    lst setOnItemClickListener onTap { case 0 => pastePaymentRequest case 1 => depositHivemind }

    def pastePaymentRequest = rm(alert) {
      def mayResolve(rawBufferString: String) = <(app.TransData recordValue rawBufferString, onFail)(_ => checkTransData)
      Try(app.getBufferUnsafe) match { case Success(rawData) => mayResolve(rawData) case _ => app quickToast err_nothing_useful }
    }

    def depositHivemind = rm(alert) {
      // Show a warning for now since hivemind sidechain is not enabled yet
      val alert = showForm(negTextBuilder(dialog_ok, getString(hivemind_details).html).create)
      try Utils clickableTextField alert.findViewById(android.R.id.message) catch none
    }
  }

  def goOps(top: View) = me goTo classOf[LNOpsActivity]
  def goAddChannel(top: View) = me goTo classOf[LNStartActivity]
  def goReceivePayment(top: View) = doReceivePayment(extra = None)
}