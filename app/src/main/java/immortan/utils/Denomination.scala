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
  def asString(amount: MilliSatoshi): String = fmt.format(BigDecimal(amount.toLong) / factor)

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = parsed(msat, mainColor, zeroColor) + "\u00A0" + sign

  def directedWithSign(incoming: Satoshi, outgoing: Satoshi, inColor: String, outColor: String, zeroColor: String, isPlus: Boolean): String =
    directedWithSign(incoming.toMilliSatoshi, outgoing.toMilliSatoshi, inColor, outColor, zeroColor, isPlus)

  def directedWithSign(incoming: MilliSatoshi, out: MilliSatoshi, inColor: String, outColor: String, zeroColor: String, isPlus: Boolean): String =
    if (isPlus && incoming == 0L.msat) parsedWithSign(incoming, inColor, zeroColor)
    else if (isPlus) "+&#160;" + parsedWithSign(incoming, inColor, zeroColor)
    else if (out == 0L.msat) parsedWithSign(out, outColor, zeroColor)
    else "-&#160;" + parsedWithSign(out, outColor, zeroColor)

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String

  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("###,###,###.###")
  val factor = 1000L
  val sign = "sat"

  fmt.setDecimalFormatSymbols(Denomination.symbols)
  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    // Zero color is not used in SAT denomination since it has no decimal parts

    val basicMsatSum = asString(msat)
    val dotIndex = basicMsatSum.indexOf(".")
    val (whole, decimal) = basicMsatSum.splitAt(dotIndex)
    if (decimal == basicMsatSum) s"<font color=$mainColor>$basicMsatSum</font>"
    else s"<font color=$mainColor>$whole<small>$decimal</small></font>"
  }
}

object BtcDenomination extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000000")
  val factor = 100000000000L
  val sign = "sat"

  fmt.setDecimalFormatSymbols(Denomination.symbols)
  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
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
      .append("<font color=").append(mainColor).append('>').append(finalDecimal).append("</font>")
      .toString
  }
}