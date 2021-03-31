package com.lightning.walletapp.sqlite

import immortan.sqlite.{DBInterface, PreparedQuery, RichCursor}
import android.database.sqlite.SQLiteDatabase
import immortan.utils.uri.Uri


trait DBInterfaceSQLiteAndroid extends DBInterface {
  def sqlPath(targetTable: String): Uri = Uri.parse(s"sqlite://com.lightning.walletapp/table/$targetTable")

  val base: SQLiteDatabase

  def change(sql: String, params: Object*): Unit = base.execSQL(sql, params.toArray)

  def change(prepared: PreparedQuery, params: Object*): Unit = prepared.bound(params:_*).executeUpdate

  def select(prepared: PreparedQuery, params: String*): RichCursor = throw new RuntimeException("Not supported")

  def select(sql: String, params: String*): RichCursor = {
    val cursor = base.rawQuery(sql, params.toArray)
    RichCursorSQLiteAndroid(cursor)
  }

  def makePreparedQuery(sql: String): PreparedQuery = PreparedQuerySQLiteAndroid(base compileStatement sql)

  def txWrap[T](run: => T): T =
    try {
      base.beginTransaction
      val executionResult = run
      base.setTransactionSuccessful
      executionResult
    } finally {
      base.endTransaction
    }
}
