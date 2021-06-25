package fr.acinq.eclair.blockchain.electrum

import fr.acinq.bitcoin._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.bitcoin.Crypto.PublicKey
import immortan.crypto.Tools.Any2Some
import scodec.bits.ByteVector
import scala.util.Try


object ElectrumWalletType {
  def makeSigningType(tag: String, master: ExtendedPrivateKey, chainHash: ByteVector32): ElectrumWalletType = tag match {
    case EclairWallet.BIP32 => makeSigningType(EclairWallet.BIP32, xPriv32(master, chainHash), chainHash)
    case EclairWallet.BIP44 => makeSigningType(EclairWallet.BIP44, xPriv44(master, chainHash), chainHash)
    case EclairWallet.BIP49 => makeSigningType(EclairWallet.BIP49, xPriv49(master, chainHash), chainHash)
    case EclairWallet.BIP84 => makeSigningType(EclairWallet.BIP84, xPriv84(master, chainHash), chainHash)
    case _ => throw new RuntimeException
  }

  def makeSigningType(tag: String, secrets: AccountAndXPrivKey, chainHash: ByteVector32): ElectrumWalletType = tag match {
    case EclairWallet.BIP32 => ElectrumWallet32(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP44 => ElectrumWallet32(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP49 => ElectrumWallet49(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case EclairWallet.BIP84 => ElectrumWallet84(secrets.asSome, publicKey(secrets.xPriv), chainHash)
    case _ => throw new RuntimeException
  }

  def makeWatchingType(tag: String, xPub: ExtendedPublicKey, chainHash: ByteVector32): ElectrumWalletType = tag match {
    case EclairWallet.BIP32 => ElectrumWallet32(secrets = None, xPub, chainHash)
    case EclairWallet.BIP44 => ElectrumWallet32(secrets = None, xPub, chainHash)
    case EclairWallet.BIP49 => ElectrumWallet49(secrets = None, xPub, chainHash)
    case EclairWallet.BIP84 => ElectrumWallet84(secrets = None, xPub, chainHash)
    case _ => throw new RuntimeException
  }

  def xPriv32(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(1L) :: 0L :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(1L) :: 0L :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(0L) :: 0L :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv44(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(44L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv49(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(49L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
  }

  def xPriv84(master: ExtendedPrivateKey, chainHash: ByteVector32): AccountAndXPrivKey = chainHash match {
    case Block.RegtestGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.TestnetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(1L) :: hardened(0L) :: Nil), master)
    case Block.LivenetGenesisBlock.hash => AccountAndXPrivKey(derivePrivateKey(master, hardened(84L) :: hardened(0L) :: hardened(0L) :: Nil), master)
    case _ => throw new RuntimeException
  }
}

abstract class ElectrumWalletType(val tag: String) {

  val secrets: Option[AccountAndXPrivKey]

  val xPub: ExtendedPublicKey

  val chainHash: ByteVector32

  val accountMaster: ExtendedPublicKey = derivePublicKey(xPub, 0L :: Nil)

  val changeMaster: ExtendedPublicKey = derivePublicKey(xPub, 1L :: Nil)

  def textAddress(key: ExtendedPublicKey): String

  def computePublicKeyScript(key: PublicKey): Seq[ScriptElt]

  def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey]

  def computeScriptHashFromPublicKey(key: PublicKey): ByteVector32 = {
    val serializedPubKeyScript: Seq[ScriptElt] = computePublicKeyScript(key)
    Crypto.sha256(Script write serializedPubKeyScript).reverse
  }

  def addUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction

  def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction
}

case class ElectrumWallet32(secrets: Option[AccountAndXPrivKey], xPub: ExtendedPublicKey, chainHash: ByteVector32) extends ElectrumWalletType(EclairWallet.BIP32) {

  override def textAddress(key: ExtendedPublicKey): String = computeP2PkhAddress(key.publicKey, chainHash)

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2pkh(key)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try {
    val _ :: OP_PUSHDATA(data, _) :: Nil = Script.parse(txIn.signatureScript)
    PublicKey(data)
  }.toOption

  override def addUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      dummySig = ByteVector.fill(71)(1)
      sigScript = Script.write(OP_PUSHDATA(dummySig) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil)
    } yield TxIn(utxo.item.outPoint, sigScript, sequenceFlag)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      previousOutputScript = Script.pay2pkh(pubKey = utxo.key.publicKey)
      privateKey = derivePrivateKey(secrets.get.master, utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, previousOutputScript, SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_BASE, privateKey)
      sigScript = Script.write(OP_PUSHDATA(sig) :: OP_PUSHDATA(utxo.key.publicKey) :: Nil)
    } yield txIn.copy(signatureScript = sigScript)
    tx.copy(txIn = txIn1)
  }
}

case class ElectrumWallet49(secrets: Option[AccountAndXPrivKey], xPub: ExtendedPublicKey, chainHash: ByteVector32) extends ElectrumWalletType(EclairWallet.BIP49) {

  override def textAddress(key: ExtendedPublicKey): String = computeBIP49Address(key.publicKey, chainHash)

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2sh(Script pay2wpkh key)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = {
    Try {
      require(txIn.witness.stack.size == 2)
      val publicKey = PublicKey(txIn.witness.stack.tail.head)
      val OP_PUSHDATA(script, _) :: Nil = Script.parse(txIn.signatureScript)
      require(Script.write(Script pay2wpkh publicKey) == script)
      publicKey
    }.toOption
  }

  override def addUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      pubKeyScript = Script.write(Script pay2wpkh utxo.key.publicKey)
      witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    } yield TxIn(utxo.item.outPoint, Script.write(OP_PUSHDATA(pubKeyScript) :: Nil), sequenceFlag, witness)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      pubKeyScript = Script.write(Script pay2wpkh utxo.key.publicKey)
      privateKey = derivePrivateKey(secrets.get.master, utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, privateKey)
    } yield txIn.copy(signatureScript = Script.write(OP_PUSHDATA(pubKeyScript) :: Nil), witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil))
    tx.copy(txIn = txIn1)
  }
}

case class ElectrumWallet84(secrets: Option[AccountAndXPrivKey], xPub: ExtendedPublicKey, chainHash: ByteVector32) extends ElectrumWalletType(EclairWallet.BIP84) {

  override def textAddress(key: ExtendedPublicKey): String = computeBIP84Address(key.publicKey, chainHash)

  override def extractPubKeySpentFrom(txIn: TxIn): Option[PublicKey] = Try(txIn.witness.stack.last).map(PublicKey.apply).toOption

  override def computePublicKeyScript(key: PublicKey): Seq[ScriptElt] = Script.pay2wpkh(key)

  override def addUtxosWithDummySig(usableUtxos: Seq[Utxo], tx: Transaction, sequenceFlag: Long): Transaction = {
    val txIn1 = for {
      utxo <- usableUtxos
      witness = ScriptWitness(ByteVector.fill(71)(1) :: utxo.key.publicKey.value :: Nil)
    } yield TxIn(utxo.item.outPoint, signatureScript = ByteVector.empty, sequenceFlag, witness)
    tx.copy(txIn = txIn1)
  }

  override def signTransaction(usableUtxos: Seq[Utxo], tx: Transaction): Transaction = {
    val txIn1 = for {
      (txIn, idx) <- tx.txIn.zipWithIndex
      utxo <- usableUtxos.find(_.item.outPoint == txIn.outPoint)
      privateKey = derivePrivateKey(secrets.get.master, utxo.key.path).privateKey
      sig = Transaction.signInput(tx, idx, Script.pay2pkh(utxo.key.publicKey), SIGHASH_ALL, utxo.item.value.sat, SigVersion.SIGVERSION_WITNESS_V0, privateKey)
    } yield txIn.copy(witness = ScriptWitness(sig :: utxo.key.publicKey.value :: Nil), signatureScript = ByteVector.empty)
    tx.copy(txIn = txIn1)
  }
}
