package immortan.sqlite

import immortan.sqlite.SQLiteData._
import fr.acinq.eclair.wire.{HostedChannelBranding, SwapInState}
import immortan.{DataBag, StorageFormat, SwapInStateExt}
import fr.acinq.bitcoin.{BlockHeader, ByteVector32}
import java.lang.{Integer => JInt}

import fr.acinq.eclair.blockchain.electrum.db.WalletDb
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.PersistentData
import fr.acinq.eclair.blockchain.electrum.db.sqlite.SqliteWalletDb.persistentDataCodec
import fr.acinq.eclair.wire.LightningMessageCodecs.hostedChannelBrandingCodec
import fr.acinq.eclair.wire.LightningMessageCodecs.swapInStateCodec
import immortan.wire.ExtCodecs.storageFormatCodec
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector
import scala.util.Try


object SQLiteData {
  final val LABEL_FORMAT = "label-format"
  final val LABEL_ELECTRUM_DATA = "label-electrum-data"
  final val LABEL_BRANDING_PREFIX = "label-branding-node-"
  final val LABEL_SWAP_IN_STATE_PREFIX = "label-swap-in-node-"
  final val LABEL_PAYMENT_REPORT_PREFIX = "label-payment-report-"
}

class SQLiteData(db: DBInterface) extends WalletDb with DataBag {
  def delete(label: String): Unit = db.change(DataTable.killSql, label)

  def tryGet(label: String): Try[ByteVector] = db.select(DataTable.selectSql, label).headTry(_ byteVec DataTable.content)

  def put(label: String, content: Bytes): Unit = {
    // Insert and then update because of INSERT IGNORE
    db.change(DataTable.newSql, label, content)
    db.change(DataTable.updSql, content, label)
  }

  // StorageFormat

  def putFormat(format: StorageFormat): Unit = put(LABEL_FORMAT, storageFormatCodec.encode(format).require.toByteArray)

  def tryGetFormat: Try[StorageFormat] = tryGet(LABEL_FORMAT).map(raw => storageFormatCodec.decode(raw.toBitVector).require.value)

  // Payment reports

  def putReport(paymentHash: ByteVector32, report: String): Unit = put(LABEL_PAYMENT_REPORT_PREFIX + paymentHash.toHex, report getBytes "UTF-8")

  def tryGetReport(paymentHash: ByteVector32): Try[String] = tryGet(LABEL_PAYMENT_REPORT_PREFIX + paymentHash.toHex).map(raw => new String(raw.toArray, "UTF-8").trim)

  // HostedChannelBranding

  def putBranding(nodeId: PublicKey, branding: HostedChannelBranding): Unit = {
    val hostedChannelBranding = hostedChannelBrandingCodec.encode(branding).require.toByteArray
    put(LABEL_BRANDING_PREFIX + nodeId.toString, hostedChannelBranding)
  }

  def tryGetBranding(nodeId: PublicKey): Try[HostedChannelBranding] =
    tryGet(LABEL_BRANDING_PREFIX + nodeId.toString) map { rawHostedChannelBranding =>
      hostedChannelBrandingCodec.decode(rawHostedChannelBranding.toBitVector).require.value
    }

  // SwapInState

  def putSwapInState(nodeId: PublicKey, state: SwapInState): Unit = {
    val swapInState = swapInStateCodec.encode(state).require.toByteArray
    put(LABEL_SWAP_IN_STATE_PREFIX + nodeId.toString, swapInState)
  }

  def tryGetSwapInState(nodeId: PublicKey): Try[SwapInStateExt] =
    tryGet(LABEL_SWAP_IN_STATE_PREFIX + nodeId.toString) map { rawSwapInState =>
      SwapInStateExt(swapInStateCodec.decode(rawSwapInState.toBitVector).require.value, nodeId)
    }

  // WalletDb

  override def persist(data: PersistentData): Unit = put(LABEL_ELECTRUM_DATA, persistentDataCodec.encode(data).require.toByteArray)

  override def readPersistentData: Option[PersistentData] = tryGet(LABEL_ELECTRUM_DATA).map(raw => persistentDataCodec.decode(raw.toBitVector).require.value).toOption

  // HeadersDb

  override def addHeaders(startHeight: Int, headers: Seq[BlockHeader] = Nil): Unit = db txWrap {
    for (Tuple2(header, idx) <- headers.zipWithIndex) db.change(ElectrumHeadersTable.addHeaderSql,
      startHeight + idx: JInt, header.hash.toHex, BlockHeader.write(header).toArray)
  }

  override def getHeader(height: Int): Option[BlockHeader] =
    db.select(ElectrumHeadersTable.selectByHeightSql, height.toString).headTry { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toOption

  // Only used in testing currently
  override def getHeader(blockHash: ByteVector32): Option[HeightAndHeader] =
    db.select(ElectrumHeadersTable.selectByBlockHashSql, blockHash.toHex).headTry { rc =>
      val header = BlockHeader.read(rc bytes ElectrumHeadersTable.header)
      val height = rc int ElectrumHeadersTable.height
      (height, header)
    }.toOption

  override def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader] =
    db.select(ElectrumHeadersTable.selectHeadersSql, startHeight.toString, maxCount.toString).iterable { rc =>
      BlockHeader.read(rc bytes ElectrumHeadersTable.header)
    }.toList

  override def getTip: Option[HeightAndHeader] =
    db.select(ElectrumHeadersTable.selectTipSql).headTry { rc =>
      val header = BlockHeader.read(rc bytes ElectrumHeadersTable.header)
      val height = rc int ElectrumHeadersTable.height
      (height, header)
    }.toOption
}
