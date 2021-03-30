package immortan.utils

import java.text._
import fr.acinq.eclair._


object Denomination {
  val symbols = new DecimalFormatSymbols
  val formatFiat = new DecimalFormat("#,###,###.##")
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi =
    (btc * BtcDenomination.factor).toLong.msat
}

trait Denomination { me =>
  def asString(msat: MilliSatoshi): String = fmt.format(BigDecimal(msat.toLong) / factor)
  def parsedWithSign(msat: MilliSatoshi): String = parsed(msat) + "\u00A0" + sign
  protected def parsed(msat: MilliSatoshi): String

  def coloredOut(msat: MilliSatoshi, suffix: String) = s"<font color=#E35646><tt>-</tt>${me parsed msat}</font>$suffix"
  def coloredIn(msat: MilliSatoshi, suffix: String) = s"<font color=#6AAB38><tt>+</tt>${me parsed msat}</font>$suffix"

  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("###,###,###.###")
  val factor = 1000L
  val sign = "sat"

  fmt setDecimalFormatSymbols Denomination.symbols

  def parsed(msat: MilliSatoshi): String = {
    val basicFormattedMsatSum = asString(msat)
    val dotIndex = basicFormattedMsatSum.indexOf(".")
    val (whole, decimal) = basicFormattedMsatSum.splitAt(dotIndex)
    if (decimal == basicFormattedMsatSum) basicFormattedMsatSum
    else s"$whole<small>$decimal</small>"
  }
}

object BtcDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000###")
  val factor = 100000000000L
  val sign = "btc"

  fmt setDecimalFormatSymbols Denomination.symbols
  def parsed(msat: MilliSatoshi): String = asString(msat) take 10
}