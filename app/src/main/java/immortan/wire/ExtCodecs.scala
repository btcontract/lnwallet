package immortan.wire

import immortan._
import scodec.codecs._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.ChannelCodecs._
import fr.acinq.eclair.wire.LightningMessageCodecs._
import fr.acinq.eclair.wire.LastCrossSignedState
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import scodec.Codec


case class HostedState(nodeId1: PublicKey, nodeId2: PublicKey, lastCrossSignedState: LastCrossSignedState)

object ExtCodecs {
  val hostedStateCodec = {
    (publicKey withContext "nodeId1") ::
      (publicKey withContext "nodeId2") ::
      (lastCrossSignedStateCodec withContext "lastCrossSignedState")
  }.as[HostedState]

  val lightningNodeKeysCodec = {
    (extendedPrivateKeyCodec withContext "extendedNodeKey") ::
      (text withContext "xpub") ::
      (privateKey withContext "hashingKey")
  }.as[LightningNodeKeys]

  val mnemonicExtStorageFormatCodec = {
    (setCodec(nodeAnnouncementCodec) withContext "outstandingProviders") ::
      (lightningNodeKeysCodec withContext "keys") ::
      (optional(bool8, bytes) withContext "seed")
  }.as[MnemonicExtStorageFormat]

  val passwordStorageFormatCodec = {
    (setCodec(nodeAnnouncementCodec) withContext "outstandingProviders") ::
      (lightningNodeKeysCodec withContext "keys") ::
      (text withContext "user") ::
      (optionalText withContext "password")
  }.as[PasswordStorageFormat]

  val storageFormatCodec: Codec[StorageFormat] =
    discriminated[StorageFormat].by(uint16)
      .typecase(1, mnemonicExtStorageFormatCodec)
      .typecase(2, passwordStorageFormatCodec)
}
