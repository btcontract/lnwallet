package com.lightning.walletapp

import fr.acinq.eclair._
import immortan.sqlite._
import com.lightning.walletapp.sqlite._
import androidx.test.ext.junit.runners.AndroidJUnit4
import fr.acinq.bitcoin.BlockHeader
import org.junit.runner.RunWith
import org.junit.Test


object DBSpec {
  def randomDBName: String = {
    def alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    List.fill(12)(secureRandom nextInt alphabet.length).map(alphabet).mkString
  }

  def getRandomNetworkStores: (SQLiteNetwork, SQLiteNetwork) = {
    def db = new DBInterfaceSQLiteAndroidGraph(WalletApp.app, randomDBName)
    val normal = new SQLiteNetwork(db, NormalChannelUpdateTable, NormalChannelAnnouncementTable, NormalExcludedChannelTable)
    val hosted = new SQLiteNetwork(db, HostedChannelUpdateTable, HostedChannelAnnouncementTable, HostedExcludedChannelTable)
    (normal, hosted)
  }

  def randInt: Int = secureRandom.nextInt(2000000000)

  def getRandomMiscInterface: DBInterfaceSQLiteAndroidMisc =
    new DBInterfaceSQLiteAndroidMisc(WalletApp.app, randomDBName)
}

@RunWith(classOf[AndroidJUnit4])
class DBSpec {

  @Test
  def insertAndReadHeaders: Unit = {
    val h1 = BlockHeader(version = 1, hashPreviousBlock = randomBytes32, hashMerkleRoot = randomBytes32, time = DBSpec.randInt, bits = DBSpec.randInt, nonce = DBSpec.randInt)
    val h2 = BlockHeader(version = 1, hashPreviousBlock = randomBytes32, hashMerkleRoot = randomBytes32, time = DBSpec.randInt, bits = DBSpec.randInt, nonce = DBSpec.randInt)
    val h3 = BlockHeader(version = 1, hashPreviousBlock = randomBytes32, hashMerkleRoot = randomBytes32, time = DBSpec.randInt, bits = DBSpec.randInt, nonce = DBSpec.randInt)
    val sqLiteData = new SQLiteData(DBSpec.getRandomMiscInterface)

    sqLiteData.addHeaders(startHeight = 10, h1 :: h2 :: h3 :: Nil)
    assert(sqLiteData.getHeaders(11, maxCount = 2) == h2 :: h3 :: Nil)
    assert(sqLiteData.getHeader(11) contains h2)
  }
}
