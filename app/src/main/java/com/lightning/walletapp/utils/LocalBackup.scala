package com.lightning.walletapp.utils

import android.os.Environment._
import fr.acinq.bitcoin.{Block, ByteVector32, Crypto}
import fr.acinq.eclair.randomBytes
import com.google.common.io.Files
import android.content.Context
import scodec.bits.ByteVector
import immortan.crypto.Tools
import scala.util.Try
import java.io.File


object LocalBackup { me =>
  final val BACKUP_NAME = "encrypted.channels"
  final val GRAPH_NAME = "graph.snapshot"
  final val BACKUP_EXTENSION = ".bin"
  final val GRAPH_EXTENSION = ".zlib"

  def getNetwork(chainHash: ByteVector32): String = chainHash match {
    case Block.LivenetGenesisBlock.hash => "mainnet"
    case Block.TestnetGenesisBlock.hash => "testnet"
    case Block.RegtestGenesisBlock.hash => "regtest"
    case _ => "unknown"
  }

  def getBackupFileUnsafe(chainHash: ByteVector32, seed: ByteVector): File = {
    val specifics = s"${me getNetwork chainHash}-${Crypto.hash160(seed).take(4).toHex}"
    new File(downloadsDir, s"$BACKUP_NAME-$specifics$BACKUP_EXTENSION")
  }

  def downloadsDir: File = getExternalStoragePublicDirectory(DIRECTORY_DOWNLOADS)

  def encryptBackup(backup: ByteVector, seed: ByteVector): ByteVector = Tools.chaChaEncrypt(Crypto.sha256(seed), randomBytes(12), backup)

  def decryptBackup(backup: ByteVector, seed: ByteVector): Try[ByteVector] = Tools.chaChaDecrypt(Crypto.sha256(seed), backup)

  def encryptAndWritePlainBackup(context: Context, dbFileName: String, chainHash: ByteVector32, seed: ByteVector): Unit = {
    val dataBaseFile = new File(context.getDatabasePath(dbFileName).getPath)
    val plainBytes = ByteVector.view(Files toByteArray dataBaseFile)
    val backupFile = getBackupFileUnsafe(chainHash, seed)
    val cipherBytes = encryptBackup(plainBytes, seed)
    Files.write(cipherBytes.toArray, backupFile)
  }

  // It is assumed that we try to decrypt a backup before running this and only proceed on success
  def copyPlainDataToDbLocation(context: Context, dbFileName: String, plainBytes: ByteVector): Unit = {
    val dataBaseFile = new File(context.getDatabasePath(dbFileName).getPath)
    if (!dataBaseFile.exists) dataBaseFile.getParentFile.mkdirs
    Files.write(plainBytes.toArray, dataBaseFile)
  }

  // Graph implanting

  // Separate method because we save the same file both in Downloads and in local assets folders
  def getGraphResourceName(chainHash: ByteVector32): String = s"$GRAPH_NAME-${me getNetwork chainHash}$GRAPH_EXTENSION"

  def getGraphFileUnsafe(chainHash: ByteVector32): File = new File(downloadsDir, me getGraphResourceName chainHash)
}
