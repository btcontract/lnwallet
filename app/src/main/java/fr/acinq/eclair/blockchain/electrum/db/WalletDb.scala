package fr.acinq.eclair.blockchain.electrum.db

import fr.acinq.bitcoin.{BlockHeader, ByteVector32}
import fr.acinq.eclair.blockchain.electrum.PersistentData


trait HeaderDb {
  type HeightAndHeader = (Int, BlockHeader)
  def addHeaders(startHeight: Int, headers: Seq[BlockHeader] = Nil): Unit

  def getHeader(height: Int): Option[BlockHeader]
  def getHeader(blockHash: ByteVector32): Option[HeightAndHeader]
  def getHeaders(startHeight: Int, maxCount: Int): Seq[BlockHeader]
  def getTip: Option[HeightAndHeader]
}

trait WalletDb extends HeaderDb {
  def persist(data: PersistentData, tag: String): Unit
  def readPersistentData(tag: String): Option[PersistentData]
}