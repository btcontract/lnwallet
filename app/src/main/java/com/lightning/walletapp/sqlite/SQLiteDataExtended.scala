package com.lightning.walletapp.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._
import com.lightning.walletapp.sqlite.SQLiteDataExtended._

import immortan.utils.{FeeRatesInfo, FiatRatesInfo}
import com.lightning.walletapp.utils.{AddonData, BasicAddon, UsedAddons}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.WalletReady
import immortan.sqlite.SQLiteData
import fr.acinq.bitcoin.Satoshi
import scala.util.Try


object SQLiteDataExtended {
  final val LABEL_ADDONS = "label-addons"
  final val LABEL_WALLET_READY = "label-wallet-ready"
  final val LABEL_FIAT_RATES = "label-fiat-rates"
  final val LABEL_FEE_RATES = "label-fee-rates"

  // Addons

  implicit object AddonDataFmt extends JsonFormat[AddonData] {
    def write(internal: AddonData): JsValue = internal match {
      case data: BasicAddon => data.toJson
      case _ => throw new Exception
    }

    def read(raw: JsValue): AddonData = raw.asJsObject fields TAG match {
      case JsString("BasicAddon") => raw.convertTo[BasicAddon]
      case tag => throw new Exception(s"Unknown addon=$tag")
    }
  }

  implicit val basicAddonFmt: JsonFormat[BasicAddon] = taggedJsonFmt(jsonFormat[Option[String], String, String, String,
    BasicAddon](BasicAddon.apply, "authToken", "supportEmail", "description", "domain"), tag = "BasicAddon")

  implicit val usedAddonsFmt: JsonFormat[UsedAddons] =
    jsonFormat[List[AddonData], UsedAddons](UsedAddons.apply, "addons")
}

class SQLiteDataExtended(override val db: DBInterfaceSQLiteAndroidMisc) extends SQLiteData(db) {
  def putAddons(addons: UsedAddons): Unit = put(LABEL_ADDONS, addons.toJson.compactPrint getBytes "UTF-8")

  def tryGetAddons: Try[UsedAddons] = tryGet(LABEL_ADDONS).map(SQLiteData.byteVecToString) map to[UsedAddons]

  def putLastBalance(wr: Satoshi): Unit = put(LABEL_WALLET_READY, wr.toJson.compactPrint getBytes "UTF-8")

  def tryGetLastBalance: Try[Satoshi] = tryGet(LABEL_WALLET_READY).map(SQLiteData.byteVecToString) map to[Satoshi]

  def putFiatRatesInfo(data: FiatRatesInfo): Unit = put(LABEL_FIAT_RATES, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFiatRatesInfo: Try[FiatRatesInfo] = tryGet(LABEL_FIAT_RATES).map(SQLiteData.byteVecToString) map to[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo): Unit = put(LABEL_FEE_RATES, data.toJson.compactPrint getBytes "UTF-8")

  def tryGetFeeRatesInfo: Try[FeeRatesInfo] = tryGet(LABEL_FEE_RATES).map(SQLiteData.byteVecToString) map to[FeeRatesInfo]
}
