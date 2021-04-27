package immortan.utils

import java.text._
import fr.acinq.eclair._
import fr.acinq.bitcoin.Satoshi


object Denomination {
  val symbols = new DecimalFormatSymbols

  val formatFiatPrecise = new DecimalFormat("#,###,###.##")
  val formatFiat = new DecimalFormat("#,###,###")

  formatFiatPrecise setDecimalFormatSymbols symbols
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi =
    (btc * BtcDenomination.factor).toLong.msat
}

trait Denomination { me =>
  protected def parsed(msat: MilliSatoshi, zeroColor: String): String
  def asString(msat: Satoshi): String = fmt.format(BigDecimal(msat.toLong) / factor)
  def asString(msat: MilliSatoshi): String = fmt.format(BigDecimal(msat.toLong) / factor)
  def parsedWithSign(msat: MilliSatoshi, zeroColor: String): String = parsed(msat, zeroColor) + "\u00A0" + sign
  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi, zeroColor: String, isPlus: Boolean): String

  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("###,###,###.###")
  val factor = 1000L
  val sign = "sat"

  fmt setDecimalFormatSymbols Denomination.symbols
  def parsed(msat: MilliSatoshi, zeroColor: String): String = {
    // Zero color is not used in SAT denomination

    val basicMsatSum = asString(msat)
    val dotIndex = basicMsatSum.indexOf(".")
    val (whole, decimal) = basicMsatSum.splitAt(dotIndex)
    if (decimal == basicMsatSum) s"<font color=#FFFFFF>$basicMsatSum</font>"
    else s"<font color=#FFFFFF>$whole</font><small>$decimal</small>"
  }

  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi, zeroColor: String, isPlus: Boolean): String =
    if (isPlus) "+" + parsedWithSign(incoming, zeroColor)
    else "-" + parsedWithSign(outgoing, zeroColor)
}

object BtcDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000000")
  val factor = 100000000000L
  val sign = "sat"

  fmt setDecimalFormatSymbols Denomination.symbols

  def parsed(msat: MilliSatoshi, zeroColor: String): String = {
    // Alpha channel does not work on Android when set as HTML attribute
    // hence zero color is supplied to match different backgrounds well
    if (0L == msat.toLong) return "0"

    val basicFormattedMsatSum = asString(msat)
    val dotIndex = basicFormattedMsatSum.indexOf(".")
    val (whole, decimal) = basicFormattedMsatSum.splitAt(dotIndex)
    val (decSat, decMsat) = decimal.splitAt(9)

    val bld = new StringBuilder(decSat).insert(3, ",").insert(7, ",").insert(0, whole)
    if ("000" != decMsat) bld.append("<small>.").append(decMsat).append("</small>")

    val splitIndex = bld.indexWhere(char => char != '0' && char != '.' && char != ',')
    val finalSplitIndex = if (".00000000" == decSat) splitIndex - 1 else splitIndex
    val (finalWhole, finalDecimal) = bld.splitAt(finalSplitIndex)

    new StringBuilder("<font color=").append(zeroColor).append('>').append(finalWhole).append("</font>")
      .append("<font color=#FFFFFF>").append(finalDecimal).append("</font>").toString
  }

  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi, zeroColor: String, isPlus: Boolean): String =
    if (isPlus) s"<font color=$zeroColor>+</font>" + parsedWithSign(incoming, zeroColor)
    else s"<font color=$zeroColor>-</font>" + parsedWithSign(outgoing, zeroColor)
}