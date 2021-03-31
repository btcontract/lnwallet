package com.lightning.walletapp.sqlite

import java.lang.{Double => JDouble, Integer => JInt, Long => JLong}
import immortan.sqlite.{PreparedQuery, RichCursor}
import android.database.sqlite.SQLiteStatement
import immortan.crypto.Tools.Bytes


case class PreparedQuerySQLiteAndroid(prepared: SQLiteStatement) extends PreparedQuery { me =>

  def bound(params: Object*): PreparedQuery = {
    params.zipWithIndex.foreach {
      case (queryParameter: JInt, positionIndex) => prepared.bindLong(positionIndex + 1, queryParameter.toLong)
      case (queryParameter: JDouble, positionIndex) => prepared.bindDouble(positionIndex + 1, queryParameter)
      case (queryParameter: String, positionIndex) => prepared.bindString(positionIndex + 1, queryParameter)
      case (queryParameter: Bytes, positionIndex) => prepared.bindBlob(positionIndex + 1, queryParameter)
      case (queryParameter: JLong, positionIndex) => prepared.bindLong(positionIndex + 1, queryParameter)
      case _ => throw new RuntimeException
    }

    me
  }

  def executeQuery: RichCursor = throw new RuntimeException("Not supported")

  def executeUpdate: Unit = prepared.executeUpdateDelete

  def close: Unit = prepared.close
}
