package com.lightning.walletapp.sqlite

import androidx.loader.content.AsyncTaskLoader
import immortan.sqlite.RichCursor
import android.content.Context
import android.database.Cursor


// Loading data with side effect, can be automatically notified to update
abstract class ReactiveLoader[T](ct: Context) extends AsyncTaskLoader[Cursor](ct) {

  def loadInBackground: Cursor = {
    val currentCursor: Cursor = getCursor
    val rc = RichCursorSQLiteAndroid(currentCursor)
    consume(rc iterable createItem)
    currentCursor
  }

  val consume: Iterable[T] => Unit

  def createItem(wrap: RichCursor): T

  def getCursor: Cursor
}