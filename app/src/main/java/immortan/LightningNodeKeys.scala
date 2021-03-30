package immortan

import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{ByteVector32, Protocol, Script}
import fr.acinq.eclair.crypto.Mac32
import java.io.ByteArrayInputStream
import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector
import java.nio.ByteOrder


object LightningNodeKeys {
  def makeFromSeed(seed: Bytes): LightningNodeKeys = {
    val master: ExtendedPrivateKey = generate(ByteVector view seed)
    val extendedNodeKey: ExtendedPrivateKey = derivePrivateKey(master, hardened(46L) :: hardened(0L) :: Nil)
    val hashingKey: PrivateKey = derivePrivateKey(master, hardened(138L) :: 0L :: Nil).privateKey
    LightningNodeKeys(extendedNodeKey, xPub(master), hashingKey)
  }

  // Compatible with Electrum/Phoenix/BLW
  def xPub(parent: ExtendedPrivateKey): String = {
    val derivationPath: KeyPath = KeyPath("m/84'/0'/0'")
    val privateKey = derivePrivateKey(parent, derivationPath)
    encode(publicKey(privateKey), zpub)
  }
}

case class LightningNodeKeys(extendedNodeKey: ExtendedPrivateKey, xpub: String, hashingKey: PrivateKey) {
  lazy val ourNodePrivateKey: PrivateKey = extendedNodeKey.privateKey
  lazy val ourNodePubKey: PublicKey = extendedNodeKey.publicKey

  // Used for separate key per domain
  def makeLinkingKey(domain: String): PrivateKey = {
    val domainBytes = ByteVector.view(domain getBytes "UTF-8")
    val pathMaterial = Mac32.hmac256(hashingKey.value.bytes, domainBytes)
    val chain = hardened(138) :: makeKeyPath(pathMaterial.bytes)
    derivePrivateKey(extendedNodeKey, chain).privateKey
  }

  def fakeInvoiceKey(paymentHash: ByteVector32): PrivateKey = {
    val chain = hardened(184) :: makeKeyPath(paymentHash.bytes)
    derivePrivateKey(extendedNodeKey, chain).privateKey
  }

  def ourFakeNodeIdKey(theirNodeId: PublicKey): ExtendedPrivateKey = {
    val chain = hardened(230) :: makeKeyPath(theirNodeId.value)
    derivePrivateKey(extendedNodeKey, chain)
  }

  def refundPubKey(theirNodeId: PublicKey): ByteVector = {
    val derivationChain = hardened(276) :: makeKeyPath(theirNodeId.value)
    val p2wpkh = Script.pay2wpkh(derivePrivateKey(extendedNodeKey, derivationChain).publicKey)
    Script.write(p2wpkh)
  }

  def makeKeyPath(material: ByteVector): List[Long] = {
    require(material.size > 15, "Material size must be at least 16")
    val stream = new ByteArrayInputStream(material.slice(0, 16).toArray)
    def getChunk = Protocol.uint32(stream, ByteOrder.BIG_ENDIAN)
    List.fill(4)(getChunk)
  }
}