package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import android.widget.{ImageView, RelativeLayout, TextView}
import immortan.utils.{InputParser, PaymentRequestExt}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import immortan.crypto.Tools
import immortan.LNParams
import android.os.Bundle
import android.view.View


class QRInvoiceActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val activityQRInvoiceMain = findViewById(R.id.activityQRInvoiceMain).asInstanceOf[RelativeLayout]
  lazy private[this] val invoiceQrCaption = findViewById(R.id.invoiceQrCaption).asInstanceOf[TextView]
  lazy private[this] val invoiceSuccess = findViewById(R.id.invoiceSuccess).asInstanceOf[ImageView]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.invoiceQr)

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_lightning_invoice)
      invoiceQrCaption.setText(getString(dialog_receive_ln).html)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def showInvoice(prExt: PaymentRequestExt): Unit =
    runInFutureProcessOnUI(QRActivity.get(prExt.raw.toUpperCase, qrSize), onFail) { bitmap =>
      def share: Unit = runInFutureProcessOnUI(shareData(bitmap, prExt.raw), onFail)(Tools.none)
      val amountHuman = LNParams.denomination.parsedWithSign(prExt.pr.amount.get, Colors.totalZero)
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy prExt.raw)
      qrViewHolder.qrShare setOnClickListener onButtonTap(share)
      qrViewHolder.qrLabel setText amountHuman.html
      qrViewHolder.qrCode setImageBitmap bitmap
    }

  def displaySuccess: Unit = {
    TransitionManager.beginDelayedTransition(activityQRInvoiceMain)
    invoiceSuccess setVisibility View.VISIBLE
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case paymentRequestExt: PaymentRequestExt => showInvoice(paymentRequestExt)
    case _ => finish
  }

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }
}
