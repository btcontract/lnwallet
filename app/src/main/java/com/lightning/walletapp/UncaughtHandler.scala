package com.lightning.walletapp

import java.lang.Thread.UncaughtExceptionHandler
import immortan.crypto.Tools.excToString
import android.content.Intent
import android.app.Activity


object UncaughtHandler {
  val ERROR_REPORT = "errorReport"
}

class UncaughtHandler(ctxt: Activity) extends UncaughtExceptionHandler {
  def uncaughtException(thread: Thread, exception: Throwable): Unit = {
    val emergencyActivity = classOf[EmergencyActivity]
    val intent = new Intent(ctxt, emergencyActivity)
    val content = excToString(exception)

    ctxt startActivity intent.putExtra(UncaughtHandler.ERROR_REPORT, content)
    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}