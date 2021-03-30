package com.lightning.walletapp

import org.ndeftools.util.activity.NfcReaderActivity
import com.ornach.nobobutton.NoboButton
import immortan.crypto.Tools.none
import immortan.utils.InputParser
import android.widget.TextView
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.view.View


object MainActivity {
  val mainActivityClass: Class[MainActivity] = classOf[MainActivity]
}

class MainActivity extends NfcReaderActivity with BaseActivity {
  lazy val skipOrbotCheck: NoboButton = findViewById(R.id.skipOrbotCheck).asInstanceOf[NoboButton]
  lazy val takeOrbotAction: NoboButton = findViewById(R.id.takeOrbotAction).asInstanceOf[NoboButton]
  lazy val mainOrbotMessage: TextView = findViewById(R.id.mainOrbotMessage).asInstanceOf[TextView]
  lazy val mainOrbotIssues: View = findViewById(R.id.mainOrbotIssues).asInstanceOf[View]
  lazy val mainOrbotCheck: View = findViewById(R.id.mainOrbotCheck).asInstanceOf[View]

  def INIT(state: Bundle): Unit = {
    setContentView(R.layout.activity_main)
    initNfc(state)
  }

  // NFC AND SHARE

  // This method is always run when `onResume` event is fired, should be a starting point for all subsequent checks
  def readNdefMessage(msg: Message): Unit = runInFutureProcessOnUI(InputParser recordValue ndefMessageString(msg), proceed)(proceed)

  override def onNoNfcIntentFound: Unit = {
    val processIntent = (getIntent.getFlags & Intent.FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY) == 0
    val dataOpt = Seq(getIntent.getDataString, getIntent getStringExtra Intent.EXTRA_TEXT).find(data => null != data)
    if (processIntent) runInFutureProcessOnUI(dataOpt foreach InputParser.recordValue, proceed)(proceed) else proceed(null)
  }

  def onNfcStateEnabled: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateChange(ok: Boolean): Unit = none
  def readEmptyNdefMessage: Unit = proceed(null)
  def readNonNdefMessage: Unit = proceed(null)

  def proceed(disregard: Any): Unit = {

  }
}
