package com.lightning.walletapp

import immortan.utils._
import immortan.crypto.Tools._
import com.lightning.walletapp.R.string._
import android.widget.RelativeLayout
import android.content.ClipData
import android.os.Bundle
import android.view.View
import immortan.LNParams
import scala.util.Try


class HubActivity extends BaseActivity with ExternalDataChecker { me =>
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]

  override def onResume: Unit = {
    checkCurrentClipboard
    super.onResume
  }

  def checkExternalData: Unit = {

  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def checkCurrentClipboard: Unit =
    Try(WalletApp.app.getBufferUnsafe) foreach { content =>
      runInFutureProcessOnUI(InputParser.parse(content), none) {

        case _: PaymentRequestExt =>
          val message = getString(buffer_invoice_found)
          snack(contentWindow, message, dialog_view, _.dismiss)
          clearClipboard

        case _: BitcoinUri =>
          val message = getString(buffer_address_found)
          snack(contentWindow, message, dialog_view, _.dismiss)
          clearClipboard

        case _: LNUrl =>
          val message = getString(buffer_link_found)
          snack(contentWindow, message, dialog_view, _.dismiss)
          clearClipboard

        case _ =>
        // Do nothing
      }
    }

  def clearClipboard: Unit = {
    val nothing = ClipData.newPlainText(null, new String)
    WalletApp.app.clipboardManager.setPrimaryClip(nothing)
  }

  def bringUpScanner(view: View): Unit = callScanner(me)
}
