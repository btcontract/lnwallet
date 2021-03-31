package immortan.sqlite

import java.lang.{Double => JDouble, Integer => JInt, Long => JLong}
import immortan.crypto.Tools.Bytes
import java.sql.PreparedStatement


trait PreparedQuery {
  def bound(params: Object*): PreparedQuery

  def executeQuery: RichCursor

  def executeUpdate: Unit

  def close: Unit
}

case class PreparedQuerySQLiteGeneral(stmt: PreparedStatement) extends PreparedQuery { me =>

  def bound(params: Object*): PreparedQuery = {
    params.zipWithIndex.foreach {
      case (queryParameter: JDouble, positionIndex) => stmt.setDouble(positionIndex + 1, queryParameter)
      case (queryParameter: String, positionIndex) => stmt.setString(positionIndex + 1, queryParameter)
      case (queryParameter: Bytes, positionIndex) => stmt.setBytes(positionIndex + 1, queryParameter)
      case (queryParameter: JLong, positionIndex) => stmt.setLong(positionIndex + 1, queryParameter)
      case (queryParameter: JInt, positionIndex) => stmt.setInt(positionIndex + 1, queryParameter)
      case _ => throw new RuntimeException
    }

    me
  }

  def executeQuery: RichCursor = RichCursorSQLiteGeneral(stmt.executeQuery)

  def executeUpdate: Unit = stmt.executeUpdate

  def close: Unit = stmt.close
}