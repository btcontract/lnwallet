package com.lightning.walletapp

import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import android.content.Intent
import android.app.Activity


object UncaughtHandler {
  val ERROR_REPORT = "errorReport"
  def toText(exc: Throwable): String = {
    val stackTraceWriter = new StringWriter
    exc printStackTrace new PrintWriter(stackTraceWriter)
    stackTraceWriter.toString
  }
}

class UncaughtHandler(ctxt: Activity) extends UncaughtExceptionHandler {
  def uncaughtException(thread: Thread, exception: Throwable): Unit = {
    val emerge: Class[EmergencyActivity] = classOf[EmergencyActivity]
    val content = UncaughtHandler toText exception
    val intent = new Intent(ctxt, emerge)

    ctxt startActivity intent.putExtra(UncaughtHandler.ERROR_REPORT, content)
    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}