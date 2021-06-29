package com.btcontract.wallettest.sqlite

import immortan.sqlite._
import android.database.sqlite._
import android.content.Context


class DBInterfaceSQLiteAndroidMisc(context: Context, name: String) extends SQLiteOpenHelper(context, name, null, 1) with DBInterfaceSQLiteAndroid {
  val base: SQLiteDatabase = getWritableDatabase

  def onCreate(dbs: SQLiteDatabase): Unit = {
    TxTable.createStatements.foreach(dbs.execSQL)
    ChannelTxFeesTable.createStatements.foreach(dbs.execSQL)
    ElectrumHeadersTable.createStatements.foreach(dbs.execSQL)
    PayMarketTable.createStatements.foreach(dbs.execSQL)
    PaymentTable.createStatements.foreach(dbs.execSQL)
    RelayTable.createStatements.foreach(dbs.execSQL)
    DataTable.createStatements.foreach(dbs.execSQL)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int): Unit = {
    // Do nothing for now
  }
}