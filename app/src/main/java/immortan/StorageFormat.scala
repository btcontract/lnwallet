package immortan

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.wire.NodeAnnouncement
import fr.acinq.eclair.crypto.Mac32
import scodec.bits.ByteVector


sealed trait StorageFormat {
  def attachedChannelSecret(theirNodeId: PublicKey): ByteVector32
  def outstandingProviders: Set[NodeAnnouncement]
  def keys: LightningNodeKeys
}

case class MnemonicExtStorageFormat(outstandingProviders: Set[NodeAnnouncement], keys: LightningNodeKeys, seed: Option[ByteVector] = None) extends StorageFormat {
  override def attachedChannelSecret(theirNodeId: PublicKey): ByteVector32 = Mac32.hmac256(keys.ourFakeNodeIdKey(theirNodeId).secretkeybytes, theirNodeId.value)
}

case class PasswordStorageFormat(outstandingProviders: Set[NodeAnnouncement], keys: LightningNodeKeys, user: String, password: Option[String] = None) extends StorageFormat {
  override def attachedChannelSecret(theirNodeId: PublicKey): ByteVector32 = Mac32.hmac256(ByteVector.view(user getBytes "UTF-8"), theirNodeId.value)
}