package com.lightning.walletapp

import scala.util.{Success, Try}
import android.content.{ClipboardManager, Context}
import immortan.utils.{BtcDenomination, Denomination}
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import android.app.Application
import android.widget.Toast
import immortan.LNParams


object WalletApp {
  var app: WalletApp = _
  var fiatCode: String = _

  // Fiat conversion

  def currentRate(rates: Fiat2Btc, code: String): Try[Double] = Try(rates apply code)
  def msatInFiat(rates: Fiat2Btc, code: String)(msat: MilliSatoshi): Try[Double] = currentRate(rates, code) map { ratePerOneBtc => msat.toLong * ratePerOneBtc / BtcDenomination.factor }
  def msatInFiatHuman(rates: Fiat2Btc, code: String, msat: MilliSatoshi): String = msatInFiat(rates, code)(msat) match { case Success(amt) => s"≈ ${Denomination.formatFiat format amt} $code" case _ => s"≈ ? $code" }
  val currentMsatInFiatHuman: MilliSatoshi => String = msat => msatInFiatHuman(LNParams.fiatRatesInfo.rates, fiatCode, msat)
}

class WalletApp extends Application {
  private[this] lazy val metrics = getResources.getDisplayMetrics
  lazy val scrWidth: Double = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val maxDialog: Double = metrics.densityDpi * 2.2
  lazy val isTablet: Boolean = scrWidth > 3.5

  lazy val plur: (Array[String], Long) => String = getString(R.string.lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], _: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  def quickToast(code: Int): Unit = quickToast(this getString code)
  def quickToast(msg: CharSequence): Unit = Toast.makeText(this, msg, Toast.LENGTH_SHORT).show
  def plur1OrZero(opts: Array[String], num: Long): String = if (num > 0) plur(opts, num).format(num) else opts(0)
  def clipboardManager: ClipboardManager = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBufferUnsafe: String = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString
}
