package com.lightning.walletapp

import android.os.Bundle
import android.view.View
import scala.util.Try


class EmergencyActivity extends BaseActivity { me =>
  def INIT(state: Bundle): Unit = me setContentView R.layout.activity_emergency
  def shareErrorReport(view: View): Unit = Try(getIntent getStringExtra UncaughtHandler.ERROR_REPORT).foreach(share)
}
