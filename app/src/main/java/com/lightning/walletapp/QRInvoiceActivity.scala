package com.lightning.walletapp

import android.os.{Bundle, Handler}
import android.widget.{ImageView, RelativeLayout, TextView}
import immortan.utils.{InputParser, PaymentRequestExt}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import android.database.ContentObserver
import fr.acinq.bitcoin.ByteVector32
import immortan.crypto.Tools
import immortan.LNParams
import android.view.View


class QRInvoiceActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val invoiceQrCaption = findViewById(R.id.invoiceQrCaption).asInstanceOf[TextView]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.invoiceQr)

  private var hash: ByteVector32 = ByteVector32.Zeroes
  private val observer: ContentObserver = new ContentObserver(new Handler) {
    override def onChange(self: Boolean): Unit = if (LNParams.cm.payBag.getPreimage(hash).isSuccess) UITask {
      TransitionManager beginDelayedTransition findViewById(R.id.activityQRInvoiceMain).asInstanceOf[RelativeLayout]
      findViewById(R.id.invoiceSuccess).asInstanceOf[ImageView] setVisibility View.VISIBLE
    }.run
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_lightning_invoice)
      invoiceQrCaption.setText(getString(R.string.dialog_receive_ln).html)
      // Piggyback on vibrator event to also mark incoming payment as done here
      getContentResolver.registerContentObserver(Vibrator.uri, true, observer)
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
      hash = prExt.pr.paymentHash
    }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case paymentRequestExt: PaymentRequestExt => showInvoice(paymentRequestExt)
    case _ => finish
  }

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def onDestroy: Unit = {
    getContentResolver.unregisterContentObserver(observer)
    super.onDestroy
  }
}
