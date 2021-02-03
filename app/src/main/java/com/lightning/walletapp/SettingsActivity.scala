package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.wire.LightningMessageCodecs._

import android.view.{Menu, MenuItem, View}
import android.content.{DialogInterface, Intent}
import android.os.Build.{VERSION, VERSION_CODES}
import com.lightning.walletapp.helper.{AES, FingerPrint}
import com.lightning.walletapp.ln.wire.{Domain, NodeAddress}
import android.content.DialogInterface.OnDismissListener
import com.lightning.walletapp.lnutils.LocalBackup
import android.support.v7.widget.Toolbar
import android.content.pm.PackageManager
import org.bitcoinj.store.SPVBlockStore
import co.infinum.goldfinger.Goldfinger
import android.provider.Settings
import android.app.AlertDialog
import scodec.bits.ByteVector
import android.os.Bundle


class SettingsActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val useTrustedNode = findViewById(R.id.useTrustedNode).asInstanceOf[CheckBox]
  lazy val saveLocalBackups = findViewById(R.id.saveLocalBackups).asInstanceOf[CheckBox]
  lazy val fpAuthentication = findViewById(R.id.fpAuthentication).asInstanceOf[CheckBox]
  lazy val ensureTorRunning = findViewById(R.id.ensureTorRunning).asInstanceOf[CheckBox]
  lazy val constrainLNFees = findViewById(R.id.constrainLNFees).asInstanceOf[CheckBox]

  lazy val useTrustedNodeState = findViewById(R.id.useTrustedNodeState).asInstanceOf[TextView]
  lazy val constrainLNFeesState = findViewById(R.id.constrainLNFeesState).asInstanceOf[TextView]
  lazy val saveLocalBackupsPath = findViewById(R.id.saveLocalBackupsPath).asInstanceOf[TextView]

  lazy val chooseBitcoinUnit = findViewById(R.id.chooseBitcoinUnit).asInstanceOf[Button]
  lazy val recoverFunds = findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
  lazy val setFiatCurrency = findViewById(R.id.setFiatCurrency).asInstanceOf[Button]
  lazy val manageOlympus = findViewById(R.id.manageOlympus).asInstanceOf[Button]
  lazy val rescanWallet = findViewById(R.id.rescanWallet).asInstanceOf[Button]
  lazy val viewMnemonic = findViewById(R.id.viewMnemonic).asInstanceOf[Button]
  lazy val gf = new Goldfinger.Builder(me).build
  lazy val host = me

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.status, menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    browse("http://lightning-wallet.com")
    true
  }

  type GrantResults = Array[Int]
  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], grantResults: GrantResults): Unit = {
    val isAllowed = reqCode == LocalBackup.LOCAL_BACKUP_REQUEST_NUMBER && grantResults.nonEmpty && grantResults.head == PackageManager.PERMISSION_GRANTED
    if (isAllowed) ChannelManager.backupSaveWorker.replaceWork("SETTINGS-INIT-SAVE-BACKUP")
    updateBackupView
  }

  def onBackupTap(cb: View) = {
    val isAllowed = LocalBackup.isAllowed(activity = me)
    if (!isAllowed) LocalBackup.askPermission(activity = me) else {
      val intent = (new Intent).setAction(Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
      val intent1 = intent setData android.net.Uri.fromParts("package", getPackageName, null)
      startActivity(intent1)
    }
  }

  def onFpTap(cb: View) = fpAuthentication.isChecked match {
    case true if VERSION.SDK_INT < VERSION_CODES.M => runAnd(fpAuthentication setChecked false)(app quickToast fp_no_support)
    case true if !gf.hasFingerprintHardware => runAnd(fpAuthentication setChecked false)(app quickToast fp_no_support)
    case true if !gf.hasEnrolledFingerprint => runAnd(fpAuthentication setChecked false)(app quickToast fp_add_print)
    case mode => FingerPrint switch mode
  }

  def onTorTap(cb: View) = app.prefs.edit.putBoolean(AbstractKit.ENSURE_TOR, ensureTorRunning.isChecked).commit

  def onTrustedTap(cb: View) = {
    val title = getString(sets_trusted_title).html
    val content = getLayoutInflater.inflate(R.layout.frag_olympus_details, null, false)
    val serverHostPort = content.findViewById(R.id.serverHostPort).asInstanceOf[EditText]
    val formatInputHint = content.findViewById(R.id.formatInputHint).asInstanceOf[TextView]
    lazy val alert = mkCheckForm(addAttempt, none, baseBuilder(title, content), dialog_ok, dialog_cancel)
    def addAttempt(alert: AlertDialog) = <(process(serverHostPort.getText.toString), onFail)(_ => alert.dismiss)
    alert setOnDismissListener new OnDismissListener { def onDismiss(some: DialogInterface) = updateTrustedView }
    content.findViewById(R.id.serverBackup).asInstanceOf[CheckBox] setVisibility View.GONE
    app.kit.trustedNodeTry.foreach(tnt => serverHostPort setText tnt.toString)
    formatInputHint setText trusted_hint

    def process(rawText: String) = if (rawText.nonEmpty) {
      val hostOrIP \ port = rawText.splitAt(rawText lastIndexOf ':')
      val nodeAddress = NodeAddress.fromParts(hostOrIP, port.tail.toInt, orElse = Domain)
      app.kit.wallet.setDescription(nodeaddress.encode(nodeAddress).require.toHex)
      app.kit.wallet.saveToFile(app.walletFile)
    } else {
      app.kit.wallet.setDescription(new String)
      app.kit.wallet.saveToFile(app.walletFile)
    }
  }

  def onConstrainLNFeesTap(cb: View) = wrap(updateConstrainLNFeesView) {
    app.prefs.edit.putBoolean(AbstractKit.CAP_LN_FEES, constrainLNFees.isChecked).commit
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_settings
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setSubtitle "App version 0.4.7-156"
    getSupportActionBar setTitle wallet_settings

    updateConstrainLNFeesView
    updateTrustedView
    updateBackupView
    updateTorView
    updateFpView

    setFiatCurrency setOnClickListener onButtonTap {
      val fiatCodes \ fiatHumanNames = fiatNames.toSeq.reverse.unzip
      val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
      val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

      def updateFiatType(pos: Int) = {
        fiatCode = fiatCodes.toList(pos)
        // Update fiatCode so UI update can react to changes right away
        app.prefs.edit.putString(AbstractKit.FIAT_TYPE, fiatCode).commit
        Option(FragWallet.worker).foreach(_.updTitleTask.run)
      }

      lst setOnItemClickListener onTap(updateFiatType)
      lst setAdapter new ArrayAdapter(me, android.R.layout.select_dialog_singlechoice, fiatHumanNames.toArray)
      showForm(alertDialog = negBuilder(dialog_ok, me getString sets_set_fiat, form).create)
      lst.setItemChecked(fiatCodes.toList indexOf fiatCode, true)
    }

    chooseBitcoinUnit setOnClickListener onButtonTap {
      val currentDenom = app.prefs.getInt(AbstractKit.DENOM_TYPE, 0)
      val allDenoms = getResources.getStringArray(R.array.denoms).map(_.html)
      val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
      val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

      def updateDenomination(pos: Int) = {
        // Update denom so UI update can react to changes
        // then persist user choice in local data storage

        denom = denoms(pos)
        app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
        Option(FragWallet.worker).foreach(_.adapter.notifyDataSetChanged)
        Option(FragWallet.worker).foreach(_.updTitleTask.run)
      }

      lst setOnItemClickListener onTap(updateDenomination)
      lst setAdapter new ArrayAdapter(me, android.R.layout.select_dialog_singlechoice, allDenoms)
      showForm(alertDialog = negBuilder(dialog_ok, me getString sets_choose_unit, form).create)
      lst.setItemChecked(currentDenom, true)
    }

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      me goTo classOf[OlympusActivity]
    }

    rescanWallet setOnClickListener onButtonTap {
      val bld = baseTextBuilder(me getString sets_rescan_ok).setTitle(me getString sets_rescan)
      mkCheckForm(alert => rm(alert)(go), none, bld, dialog_ok, dialog_cancel)

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
        finishAffinity
        System exit 0
      } catch none
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Can be accessed here and from page button
      me viewMnemonic null
    }

    recoverFunds setOnClickListener onButtonTap {
      def recover = LNParams.olympusWrap getBackup LNParams.keys.cloudId foreach { backups =>
        // Get all backups, then filter out those with already locally existing channel ids

        for {
          ciphertext <- backups
          cipherbytes = ByteVector.fromValidHex(ciphertext).toArray
          ref <- AES.decBytes(cipherbytes, LNParams.keys.cloudSecret.toArray) map bin2readable map to[RefundingData]
          if !ChannelManager.all.flatMap(_.getCommits).map(_.channelId).contains(ref.commitments.channelId)
        } ChannelManager.all +:= ChannelManager.createChannel(ChannelManager.operationalListeners, ref)

        // Disconnect in case if we are already connected to target node
        // without doing this a channel reestablish won't be happening
        ConnectionManager.workers.values.foreach(_.disconnect)

        for {
          chan <- ChannelManager.all if chan.state == REFUNDING
          // Try to connect right away and maybe use new address later
          _ = ConnectionManager.connectTo(chan.data.announce, notify = false)
          // Can call findNodes without `retry` wrapper because it gives `Obs.empty` on error
          Vector(ann1 \ _, _*) <- LNParams.olympusWrap findNodes chan.data.announce.nodeId.toString
        } chan process ann1
      }

      val bld = baseTextBuilder(me getString channel_recovery_info)
      def proceed = runAnd(recover)(me toast dialog_olympus_recovering)
      mkCheckForm(alert => rm(alert)(proceed), none, bld, dialog_next, dialog_cancel)
    }

    // Wallet may not notice incoming tx until synchronized
    recoverFunds.setEnabled(ChannelManager.blockDaysLeft <= 1)
  } else me exitTo classOf[MainActivity]

  def updateBackupView = {
    val dir = LocalBackup.getBackupDirectory(LNParams.chainHash)
    val canWrite = LocalBackup.isAllowed(me) && LocalBackup.isExternalStorageWritable
    if (canWrite) saveLocalBackupsPath.setText(LocalBackup.getBackupFileUnsafe(dir, LNParams.keys.cloudId).getPath)
    saveLocalBackupsPath setVisibility viewMap(canWrite)
    saveLocalBackups setChecked canWrite
  }

  def updateFpView = fpAuthentication setChecked FingerPrint.isOperational(gf)
  def updateTorView = ensureTorRunning setChecked app.prefs.getBoolean(AbstractKit.ENSURE_TOR, false)

  def updateTrustedView = {
    val nodeAddressTry = app.kit.trustedNodeTry
    if (nodeAddressTry.isFailure) useTrustedNodeState setText trusted_hint_none
    else useTrustedNodeState setText nodeAddressTry.get.toString
    useTrustedNode setChecked nodeAddressTry.isSuccess
  }

  def updateConstrainLNFeesView = {
    constrainLNFees setChecked app.prefs.getBoolean(AbstractKit.CAP_LN_FEES, false)
    val constrainedText = getString(ln_fee_cap_enabled).format("1%", denom parsedWithSign PaymentInfo.onChainThreshold)
    val message = if (constrainLNFees.isChecked) constrainedText else getString(ln_fee_cap_disabled).format("1%")
    constrainLNFeesState setText message.html
  }
}