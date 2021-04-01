package com.lightning.walletapp

import com.google.common.io.ByteStreams
import android.content.Intent
import android.app.Activity
import android.os.Bundle


class SetupActivity extends BaseActivity {
  final val FILE_REQUEST_CODE: Int = 112

  def INIT(state: Bundle): Unit = {

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
}
