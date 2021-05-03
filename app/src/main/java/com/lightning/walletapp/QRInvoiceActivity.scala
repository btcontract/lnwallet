package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import immortan.utils.{InputParser, PaymentRequestExt}
import com.lightning.walletapp.BaseActivity.StringOps
import android.widget.TextView
import immortan.crypto.Tools
import immortan.LNParams
import android.os.Bundle


class QRInvoiceActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.invoiceQr)
  lazy private[this] val invoiceQrCaption = findViewById(R.id.invoiceQrCaption).asInstanceOf[TextView]

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

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case paymentRequestExt: PaymentRequestExt => showInvoice(paymentRequestExt)
    case _ => finish
  }

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }
}
