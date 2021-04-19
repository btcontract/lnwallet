package immortan.utils

import immortan.crypto.Tools._
import immortan.utils.ImplicitJsonFormats._
import rx.lang.scala.{Observable, Subscription}
import com.github.kevinsawicki.http.HttpRequest.get
import immortan.crypto.CanBeShutDown
import immortan.LNParams


object FiatRates extends CanBeShutDown {
  type BlockchainInfoItemMap = Map[String, BlockchainInfoItem]
  type CoinGeckoItemMap = Map[String, CoinGeckoItem]
  type BitpayItemList = List[BitpayItem]

  val customFiatSymbols: Map[String, String] =
    Map("rub" -> "\u20BD", "usd" -> "$", "inr" -> "₹", "gbp" -> "£",
      "cny" -> "CN¥", "jpy" -> "¥", "brl" -> "R$", "eur" -> "€", "krw" -> "₩")

  def reloadData: Fiat2Btc = fr.acinq.eclair.secureRandom nextInt 3 match {
    case 0 => to[CoinGecko](get("https://api.coingecko.com/api/v3/exchange_rates").body).rates.map { case (code, item) => code.toLowerCase -> item.value }
    case 1 => to[BlockchainInfoItemMap](get("https://blockchain.info/ticker").body).map { case (code, item) => code.toLowerCase -> item.last }
    case _ => to[Bitpay](get("https://bitpay.com/rates").body).data.map { case BitpayItem(code, rate) => code.toLowerCase -> rate }.toMap
  }

  private[this] val periodSecs = 60 * 30
  private[this] val retryRepeatDelayedCall: Observable[Fiat2Btc] = {
    val retry = Rx.retry(Rx.ioQueue.map(_ => reloadData), Rx.incSec, 3 to 18 by 3)
    val repeat = Rx.repeat(retry, Rx.incSec, periodSecs to Int.MaxValue by periodSecs)
    Rx.initDelay(repeat, LNParams.fiatRatesInfo.stamp, periodSecs * 1000L)
  }

  var listeners: Set[FiatRatesListener] = Set.empty

  val subscription: Subscription = retryRepeatDelayedCall.subscribe(newRates => {
    LNParams.fiatRatesInfo = FiatRatesInfo(newRates, LNParams.fiatRatesInfo.rates, System.currentTimeMillis)
    for (lst <- listeners) lst.onFiatRates(LNParams.fiatRatesInfo)
  }, none)

  override def becomeShutDown: Unit = {
    subscription.unsubscribe
    listeners = Set.empty
  }
}

trait FiatRatesListener {
  def onFiatRates(rates: FiatRatesInfo): Unit
}

case class CoinGeckoItem(value: Double)
case class BlockchainInfoItem(last: Double)
case class BitpayItem(code: String, rate: Double)

case class Bitpay(data: FiatRates.BitpayItemList)
case class CoinGecko(rates: FiatRates.CoinGeckoItemMap)

case class FiatRatesInfo(rates: Fiat2Btc, oldRates: Fiat2Btc, stamp: Long) {
  def pctChange(fresh: Double, old: Double): Double = (fresh - old) / old * 100

  def pctDifference(code: String): Option[String] = List(rates get code, oldRates get code) match {
    case Some(fresh) :: Some(old) :: Nil if fresh > old => Some(s"<font color=#8BD670><small>▲</small> ${Denomination.formatFiatPrecise format pctChange(fresh, old).abs}%</font>")
    case Some(fresh) :: Some(old) :: Nil if fresh < old => Some(s"<small>▼</small> ${Denomination.formatFiatPrecise format pctChange(fresh, old).abs}%")
    case _ => None
  }
}