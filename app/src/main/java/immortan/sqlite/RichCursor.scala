package immortan.sqlite

import immortan.crypto.Tools.Bytes
import scodec.bits.ByteVector
import scala.util.Try


trait RichCursor extends Iterable[RichCursor] {
  def byteVec(key: String): ByteVector = ByteVector view bytes(key)

  def iterable[T](transform: RichCursor => T): Iterable[T]

  def set[T](transform: RichCursor => T): Set[T]

  def headTry[T](fun: RichCursor => T): Try[T]

  def bytes(key: String): Bytes

  def string(key: String): String

  def long(key: String): Long

  def long(pos: Int): Long

  def int(key: String): Int
}
