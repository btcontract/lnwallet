package com.lightning.walletapp.utils

import android.os.Environment._
import fr.acinq.bitcoin.{Block, ByteVector32, Crypto}
import android.Manifest.permission.WRITE_EXTERNAL_STORAGE
import androidx.appcompat.app.AppCompatActivity
import androidx.core.content.ContextCompat
import android.content.pm.PackageManager
import androidx.core.app.ActivityCompat
import fr.acinq.eclair.randomBytes
import com.google.common.io.Files
import android.content.Context
import scodec.bits.ByteVector
import immortan.crypto.Tools
import scala.util.Try
import java.io.File


object LocalBackup { me =>
  final val BACKUP_DIR = "BLW"
  final val LOCAL_BACKUP_REQUEST_NUMBER = 105

  final val BACKUP_NAME = "encrypted.channels"
  final val BACKUP_EXTENSION = ".bkp"

  final val GRAPH_NAME = "graph.snapshot"
  final val GRAPH_EXTENSION = ".zlib"

  def isExternalStorageWritable: Boolean = MEDIA_MOUNTED.equals(getExternalStorageState) && getExternalStorageDirectory.canWrite

  def isAllowed(activity: AppCompatActivity): Boolean = ContextCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED

  def askPermission(activity: AppCompatActivity): Unit = ActivityCompat.requestPermissions(activity, Array(WRITE_EXTERNAL_STORAGE), LOCAL_BACKUP_REQUEST_NUMBER)

  def getSuffix(seed: ByteVector32): String = Crypto.hash160(seed).take(4).toHex

  def getNetwork(chainHash: ByteVector32): String = chainHash match {
    case Block.LivenetGenesisBlock.hash => "mainnet"
    case Block.TestnetGenesisBlock.hash => "testnet"
    case Block.RegtestGenesisBlock.hash => "regtest"
    case _ => "unknown"
  }

  def getBackupFileUnsafe(chainHash: ByteVector32, seed: ByteVector32): File = {
    val fileName = s"$BACKUP_NAME-${me getNetwork chainHash}-${me getSuffix seed}$BACKUP_EXTENSION"
    val directory = new File(getExternalStorageDirectory, BACKUP_DIR)
    val backup = new File(directory, fileName)
    if (!backup.isFile) directory.mkdirs
    backup
  }

  def getGraphResourceName(chainHash: ByteVector32): String = s"$GRAPH_NAME-${me getNetwork chainHash}$GRAPH_EXTENSION"

  def getGraphFileUnsafe(chainHash: ByteVector32): File = {
    val directory = new File(getExternalStorageDirectory, BACKUP_DIR)
    val backup = new File(directory, me getGraphResourceName chainHash)
    if (!backup.isFile) directory.mkdirs
    backup
  }

  def encryptBackup(backup: ByteVector, seed: ByteVector): ByteVector = Tools.chaChaEncrypt(Crypto.sha256(seed), randomBytes(12), backup)

  def decryptBackup(backup: ByteVector, seed: ByteVector): Try[ByteVector] = Tools.chaChaDecrypt(Crypto.sha256(seed), backup)

  // It is assumed that database file already exists at this point
  def encryptAndWritePlainBackup(context: Context, dbFileName: String, chainHash: ByteVector32, seed: ByteVector32): Unit = {
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
}
