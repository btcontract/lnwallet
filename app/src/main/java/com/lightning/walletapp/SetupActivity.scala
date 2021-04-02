package com.lightning.walletapp

import android.widget.{ArrayAdapter, LinearLayout}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import androidx.appcompat.app.AlertDialog
import com.google.common.io.ByteStreams
import com.ornach.nobobutton.NoboButton
import android.content.Intent
import immortan.crypto.Tools
import android.app.Activity
import android.os.Bundle
import android.view.View


class SetupActivity extends BaseActivity { me =>
  private[this] lazy val activitySetupMain = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]
  private[this] lazy val restoreOptionsButton = findViewById(R.id.restoreOptionsButton).asInstanceOf[NoboButton]
  private[this] lazy val restoreOptions = findViewById(R.id.restoreOptions).asInstanceOf[LinearLayout]

  private[this] final val FILE_REQUEST_CODE = 112
  private[this] final val SEPARATOR = " "

  def INIT(state: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
  }

  def openFilePicker: Unit = {
    val intent = new Intent(Intent.ACTION_OPEN_DOCUMENT).setType("*/*")
    startActivityForResult(intent, FILE_REQUEST_CODE)
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent): Unit = {
    if (requestCode == FILE_REQUEST_CODE && resultCode == Activity.RESULT_OK && resultData != null) {
      val inputStream = getContentResolver.openInputStream(resultData.getData)
      ByteStreams.toByteArray(inputStream)
    }
  }

  def createNewWallet(view: View): Unit = {

  }

  def showRestoreOptions(view: View): Unit = {
    TransitionManager.beginDelayedTransition(activitySetupMain)
    restoreOptionsButton.setVisibility(View.GONE)
    restoreOptions.setVisibility(View.VISIBLE)
  }

  def useBackupFile(view: View): Unit = {
    showMnemonicPopup(R.string.action_recovery_phrase_backup_title)
  }

  def useRecoveryPhrase(view: View): Unit = {
    showMnemonicPopup(R.string.action_recovery_phrase_title)
  }

  def showMnemonicPopup(title: Int): Unit = {
    val mnemonicWrap = getLayoutInflater.inflate(R.layout.frag_mnemonic, null).asInstanceOf[LinearLayout]
    val restoreCode = mnemonicWrap.findViewById(R.id.restoreCode).asInstanceOf[com.hootsuite.nachos.NachoTextView]

    restoreCode.addChipTerminator(' ', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, WalletApp.app.englishWordList)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark

    def maybeProceed(alert: AlertDialog): Unit = {
      val mnemonic = restoreCode.getText.toString.toLowerCase.trim
      val pureMnemonic = mnemonic.replaceAll("[^a-zA-Z0-9']+", SEPARATOR).split(SEPARATOR)
      if (pureMnemonic.length != 12) WalletApp.app.quickToast(R.string.error_short_phrase)
      else alert.dismiss
    }

    val bld = titleBodyAsViewBuilder(str2View(getString(title).html), mnemonicWrap)
    mkCheckForm(maybeProceed, Tools.none, bld, R.string.dialog_ok, R.string.dialog_cancel)
  }
}
