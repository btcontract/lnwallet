package immortan.utils

import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.fee._
import immortan.utils.ImplicitJsonFormats._
import rx.lang.scala.{Observable, Subscription}
import com.github.kevinsawicki.http.HttpRequest.get
import fr.acinq.eclair.blockchain.CurrentFeerates
import immortan.crypto.Tools.none
import immortan.LNParams


object FeeRates {
  def reloadData: FeeratesPerKB = fr.acinq.eclair.secureRandom nextInt 4 match {
    case 0 => new EsploraFeeProvider("https://blockstream.info/api/fee-estimates").provide
    case 1 => new EsploraFeeProvider("https://mempool.space/api/fee-estimates").provide
    case 2 => EarnDotComFeeProvider.provide
    case _ => BitgoFeeProvider.provide
  }

  private[this] val periodHours = 12
  private[this] val retryRepeatDelayedCall: Observable[FeeratesPerKB] = {
    val retry = Rx.retry(Rx.ioQueue.map(_ => reloadData), Rx.incSec, 3 to 18 by 3)
    val repeat = Rx.repeat(retry, Rx.incHour, periodHours to Int.MaxValue by periodHours)
    Rx.initDelay(repeat, LNParams.fiatRatesInfo.stamp, periodHours * 60 * 60 * 1000L)
  }

  var listeners: Set[FeeRatesListener] = Set.empty
  val subscription: Subscription = retryRepeatDelayedCall.subscribe(newRates => {
    val newRatesInfo = FeeRatesInfo(newRates, System.currentTimeMillis)
    for (lst <- listeners) lst.onFeeRates(newRatesInfo)
    LNParams.feeRatesInfo = newRatesInfo
  }, none)

  val defaultFeerates: FeeratesPerKB =
    FeeratesPerKB(
      block_1 = FeeratePerKB(210000.sat),
      blocks_2 = FeeratePerKB(180000.sat),
      blocks_6 = FeeratePerKB(150000.sat),
      blocks_12 = FeeratePerKB(110000.sat),
      blocks_36 = FeeratePerKB(50000.sat),
      blocks_72 = FeeratePerKB(20000.sat),
      blocks_144 = FeeratePerKB(15000.sat),
      blocks_1008 = FeeratePerKB(5000.sat),
      mempoolMinFee = FeeratePerKB(5000.sat)
    )
}

case class FeeRatesInfo(perKb: FeeratesPerKB, stamp: Long) {
  val feeratesPerKw: FeeratesPerKw = FeeratesPerKw(feerates = perKb)
  val current: CurrentFeerates = CurrentFeerates(feeratesPerKw)

  val feeEstimator: FeeEstimator = new FeeEstimator {
    override def getFeeratePerKb(target: Int): FeeratePerKB = perKb.feePerBlock(target)
    override def getFeeratePerKw(target: Int): FeeratePerKw = feeratesPerKw.feePerBlock(target)
  }

  val onChainFeeConf: OnChainFeeConf =
    OnChainFeeConf(FeeTargets(fundingBlockTarget = 12, commitmentBlockTarget = 6, mutualCloseBlockTarget = 36, claimMainBlockTarget = 36),
      feeEstimator, closeOnOfflineMismatch = false, updateFeeMinDiffRatio = 0.1, FeerateTolerance(0.2, 20), perNodeFeerateTolerance = Map.empty)
}

trait FeeRatesListener {
  def onFeeRates(rates: FeeRatesInfo): Unit
}

trait FeeRatesProvider {
  def provide: FeeratesPerKB
  val url: String
}

// Esplora

class EsploraFeeProvider(val url: String) extends FeeRatesProvider {
  type EsploraFeeStructure = Map[String, Long]

  def provide: FeeratesPerKB = {
    val structure = to[EsploraFeeStructure](get(url).connectTimeout(15000).body)

    FeeratesPerKB(
      mempoolMinFee = extractFeerate(structure, 1008),
      block_1 = extractFeerate(structure, 1),
      blocks_2 = extractFeerate(structure, 2),
      blocks_6 = extractFeerate(structure, 6),
      blocks_12 = extractFeerate(structure, 12),
      blocks_36 = extractFeerate(structure, 36),
      blocks_72 = extractFeerate(structure, 72),
      blocks_144 = extractFeerate(structure, 144),
      blocks_1008 = extractFeerate(structure, 1008)
    )
  }

  // First we keep only fee ranges with a max block delay below the limit
  // out of all the remaining fee ranges, we select the one with the minimum higher bound
  def extractFeerate(feeRanges: EsploraFeeStructure, maxBlockDelay: Int): FeeratePerKB = {
    val belowLimit = FeeratePerVByte(feeRanges.filterKeys(_.toInt <= maxBlockDelay).values.min.sat)
    val convertedToPerKw = FeeratePerKw(belowLimit)
    FeeratePerKB(convertedToPerKw)
  }
}

// BitGo

case class BitGoFeeRateStructure(feeByBlockTarget: Map[String, Long], feePerKb: Long)

object BitgoFeeProvider extends FeeRatesProvider {
  val url = "https://www.bitgo.com/api/v2/btc/tx/fee"

  def provide: FeeratesPerKB = {
    val structure = to[BitGoFeeRateStructure](get(url).connectTimeout(15000).body)

    FeeratesPerKB(
      mempoolMinFee = extractFeerate(structure, 1008),
      block_1 = extractFeerate(structure, 1),
      blocks_2 = extractFeerate(structure, 2),
      blocks_6 = extractFeerate(structure, 6),
      blocks_12 = extractFeerate(structure, 12),
      blocks_36 = extractFeerate(structure, 36),
      blocks_72 = extractFeerate(structure, 72),
      blocks_144 = extractFeerate(structure, 144),
      blocks_1008 = extractFeerate(structure, 1008)
    )
  }

  // first we keep only fee ranges with a max block delay below the limit
  // out of all the remaining fee ranges, we select the one with the minimum higher bound
  def extractFeerate(structure: BitGoFeeRateStructure, maxBlockDelay: Int): FeeratePerKB = {
    val belowLimit = structure.feeByBlockTarget.filterKeys(_.toInt <= maxBlockDelay).values
    FeeratePerKB(belowLimit.min.sat)
  }
}

// EarnDotCom

case class EarnDotComFeeRateStructure(fees: List[EarnDotComFeeRateItem] = Nil) {
  val feesPerKilobyte: List[EarnDotComFeeRateItem] = fees.map(_.perKilobyte)
}

case class EarnDotComFeeRateItem(minFee: Long, maxFee: Long, memCount: Long, minDelay: Long, maxDelay: Long) {
  lazy val perKilobyte: EarnDotComFeeRateItem = copy(minFee = minFee * 1000L, maxFee = maxFee * 1000L)
}

object EarnDotComFeeProvider extends FeeRatesProvider {
  val url = "https://bitcoinfees.earn.com/api/v1/fees/list"

  def provide: FeeratesPerKB = {
    val structure = to[EarnDotComFeeRateStructure](get(url).connectTimeout(15000).body)

    FeeratesPerKB(
      mempoolMinFee = extractFeerate(structure, 1008),
      block_1 = extractFeerate(structure, 1),
      blocks_2 = extractFeerate(structure, 2),
      blocks_6 = extractFeerate(structure, 6),
      blocks_12 = extractFeerate(structure, 12),
      blocks_36 = extractFeerate(structure, 36),
      blocks_72 = extractFeerate(structure, 72),
      blocks_144 = extractFeerate(structure, 144),
      blocks_1008 = extractFeerate(structure, 1008)
    )
  }

  // First we keep only fee ranges with a max block delay below the limit
  // out of all the remaining fee ranges, select the one with the minimum higher bound and make sure it is > 0
  def extractFeerate(structure: EarnDotComFeeRateStructure, maxBlockDelay: Int): FeeratePerKB = {
    val belowLimit = structure.feesPerKilobyte.filter(_.maxDelay <= maxBlockDelay)
    FeeratePerKB(Math.max(belowLimit.minBy(_.maxFee).maxFee, 1).sat)
  }
}