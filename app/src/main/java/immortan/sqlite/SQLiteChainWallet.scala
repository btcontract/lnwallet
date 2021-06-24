package immortan.sqlite

import spray.json._
import immortan.utils.ImplicitJsonFormats._

import java.lang.{Long => JLong}
import fr.acinq.eclair.blockchain.electrum.db.{ChainWalletInfo, CompleteChainWalletInfo, WalletDb}
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.blockchain.electrum.PersistentData
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.Satoshi
import immortan.crypto.Tools


class SQLiteChainWallet(val db: DBInterface) extends WalletDb {
  def remove(pub: PublicKey): Unit = db.change(ChainWalletTable.killSql, pub.toString)

  def addChainWallet(info: CompleteChainWalletInfo): Unit =
    db.change(ChainWalletTable.newSql, info.core.toJson.compactPrint, info.pub.toString,
      info.data.toArray, info.lastBalance.toLong: JLong, info.label)

  def persist(data: PersistentData, lastBalance: Satoshi, pub: PublicKey): Unit =
    db.change(ChainWalletTable.updSql, persistentDataCodec.encode(data).require.toByteArray,
      lastBalance.toLong: JLong, pub.toString)

  def updateLabel(label: String, pub: PublicKey): Unit = db.change(ChainWalletTable.updLabelSql, label, pub.toString)

  def listWallets: Iterable[CompleteChainWalletInfo] = db.select(ChainWalletTable.selectSql).iterable { rc =>
    CompleteChainWalletInfo(to[ChainWalletInfo](rc string ChainWalletTable.info), Tools.stringToPubKey(rc string ChainWalletTable.xPub),
      rc byteVec ChainWalletTable.data, Satoshi(rc long ChainWalletTable.lastBalance), rc string ChainWalletTable.label)
  }
}
