package immortan.sqlite

import java.lang.{Double => JDouble, Integer => JInt, Long => JLong}
import java.sql.{Connection, PreparedStatement}
import immortan.crypto.Tools.Bytes


case class DBInterfaceSqliteGeneral(connection: Connection) extends DBInterface {
  def change(sql: String, params: Object*): Unit = change(connection.prepareStatement(sql), params:_*)

  def change(stmt: PreparedStatement, params: Object*): Unit = bindParameters(params, stmt).executeUpdate

  def select(sql: String, params: String*): RichCursor = select(connection.prepareStatement(sql), params:_*)

  def select(stmt: PreparedStatement, params: String*): RichCursor = RichCursorSqliteGeneral(bindParameters(params, stmt).executeQuery)

  def bindParameters(params: Seq[Object], stmt: PreparedStatement): PreparedStatement = {
    params.zipWithIndex.foreach {
      case (queryParameter: JDouble, positionIndex) => stmt.setDouble(positionIndex + 1, queryParameter)
      case (queryParameter: String, positionIndex) => stmt.setString(positionIndex + 1, queryParameter)
      case (queryParameter: Bytes, positionIndex) => stmt.setBytes(positionIndex + 1, queryParameter)
      case (queryParameter: JLong, positionIndex) => stmt.setLong(positionIndex + 1, queryParameter)
      case (queryParameter: JInt, positionIndex) => stmt.setInt(positionIndex + 1, queryParameter)
      case _ => throw new RuntimeException
    }

    stmt
  }

  def txWrap[T](run: => T): T = {
    val old = connection.getAutoCommit
    connection.setAutoCommit(false)

    try {
      val runResult = run
      connection.commit
      runResult
    } catch {
      case error: Throwable =>
        connection.rollback
        throw error
    } finally {
      connection.setAutoCommit(old)
    }
  }
}
