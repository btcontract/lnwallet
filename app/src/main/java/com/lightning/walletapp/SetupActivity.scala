package com.lightning.walletapp

import fr.acinq.eclair._
import scala.util.{Failure, Success}
import android.widget.{ArrayAdapter, LinearLayout}
import immortan.{LightningNodeKeys, MnemonicExtStorageFormat}
import com.lightning.walletapp.BaseActivity.StringOps
import com.lightning.walletapp.utils.LocalBackup
import androidx.transition.TransitionManager
import androidx.appcompat.app.AlertDialog
import com.google.common.io.ByteStreams
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.MnemonicCode
import immortan.crypto.Tools.none
import android.content.Intent
import scodec.bits.ByteVector
import android.app.Activity
import android.os.Bundle
import android.view.View


class SetupActivity extends BaseActivity { me =>
  private[this] lazy val activitySetupMain = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]
  private[this] lazy val restoreOptionsButton = findViewById(R.id.restoreOptionsButton).asInstanceOf[NoboButton]
  private[this] lazy val restoreOptions = findViewById(R.id.restoreOptions).asInstanceOf[LinearLayout]

  private[this] final val FILE_REQUEST_CODE = 112
  private[this] final val SEPARATOR = " "

  def INIT(state: Bundle): Unit = if (WalletApp.isAlive) {
    setContentView(R.layout.activity_setup)
  } else {
    WalletApp.freePossiblyUsedResouces
    me exitTo ClassNames.mainActivityClass
  }

  def makeFromSeed(seed: ByteVector): Unit = {
    val keys = LightningNodeKeys.makeFromSeed(seed.toArray)
    val format = MnemonicExtStorageFormat(Set.empty, keys, seed)
    WalletApp.extDataBag.putFormat(format)
    WalletApp.makeOperational(format)
  }

  var proceedWithSeed: ByteVector => Unit = seed => {
    // Make sure this method can be run at most once by replacing it with a dummy right away
    runInFutureProcessOnUI(makeFromSeed(seed), onFail)(_ => me exitTo ClassNames.hubActivityClass)
    TransitionManager.beginDelayedTransition(activitySetupMain)
    activitySetupMain.setVisibility(View.GONE)
    proceedWithSeed = none
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent): Unit =
    if (requestCode == FILE_REQUEST_CODE && resultCode == Activity.RESULT_OK && resultData != null) {
      val cipherBytes = ByteStreams.toByteArray(getContentResolver openInputStream resultData.getData)

      showMnemonicPopup(R.string.action_backup_present_title) { seed =>
        LocalBackup.decryptBackup(ByteVector.view(cipherBytes), seed) match {

          case Success(plainBytes) =>
            // We were able to decrypt a file, implant it into db location and proceed
            LocalBackup.restoreFromPlainBackup(me, WalletApp.dbFileNameEssential, plainBytes)
            proceedWithSeed(plainBytes)

          case Failure(exception) =>
            val msg = getString(R.string.error_could_not_decrypt)
            onFail(msg.format(exception.getMessage).html)
        }
      }
    }

  def createNewWallet(view: View): Unit = proceedWithSeed(randomBytes32)

  def showRestoreOptions(view: View): Unit = {
    TransitionManager.beginDelayedTransition(activitySetupMain)
    restoreOptionsButton.setVisibility(View.GONE)
    restoreOptions.setVisibility(View.VISIBLE)
  }

  def useBackupFile(view: View): Unit = startActivityForResult(new Intent(Intent.ACTION_OPEN_DOCUMENT).setType("*/*"), FILE_REQUEST_CODE)

  def useRecoveryPhrase(view: View): Unit = showMnemonicPopup(R.string.action_recovery_phrase_title)(proceedWithSeed)

  def showMnemonicPopup(title: Int)(onSeed: ByteVector => Unit): Unit = {
    val mnemonicWrap = getLayoutInflater.inflate(R.layout.frag_mnemonic, null).asInstanceOf[LinearLayout]
    val recoveryPhrase = mnemonicWrap.findViewById(R.id.recoveryPhrase).asInstanceOf[com.hootsuite.nachos.NachoTextView]

    recoveryPhrase.addChipTerminator(' ', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator(',', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator('\n', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, WalletApp.app.englishWordList)
    recoveryPhrase setDropDownBackgroundResource R.color.button_material_dark

    def maybeProceed(alert: AlertDialog): Unit = {
      val mnemonic = recoveryPhrase.getText.toString.toLowerCase.trim
      val pureMnemonic = mnemonic.replaceAll("[^a-zA-Z0-9']+", SEPARATOR).split(SEPARATOR).distinct
      if (pureMnemonic.length != 12) WalletApp.app.quickToast(getString(R.string.error_wrong_phrase).html) else {
        val seed = MnemonicCode.toSeed(pureMnemonic, passphrase = new String)
        removeAndProceedWithTimeout(alert)(onSeed apply seed)
      }
    }

    val bld = titleBodyAsViewBuilder(str2View(getString(title).html), mnemonicWrap)
    mkCheckForm(maybeProceed, none, bld, R.string.dialog_ok, R.string.dialog_cancel)
  }
}
