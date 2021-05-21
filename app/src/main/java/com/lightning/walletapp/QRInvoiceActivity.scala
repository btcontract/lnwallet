package com.lightning.walletapp

import immortan.{ChannelMaster, LNParams}
import immortan.utils.{InputParser, PaymentRequestExt}
import android.widget.{ImageView, RelativeLayout, TextView}
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.transition.TransitionManager
import fr.acinq.bitcoin.ByteVector32
import immortan.crypto.Tools.none
import android.os.Bundle
import android.view.View


class QRInvoiceActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val invoiceQrCaption = findViewById(R.id.invoiceQrCaption).asInstanceOf[TextView]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.invoiceQr)

  private var hashOfInterest: ByteVector32 = ByteVector32.Zeroes
  private val subscription = ChannelMaster.hashRevealStream.subscribe(paymentHash => UITask {
    TransitionManager beginDelayedTransition findViewById(R.id.activityQRInvoiceMain).asInstanceOf[RelativeLayout]
    if (paymentHash == hashOfInterest) findViewById(R.id.invoiceSuccess).asInstanceOf[ImageView] setVisibility View.VISIBLE
  }.run)

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_lightning_invoice)
      invoiceQrCaption.setText(getString(R.string.dialog_receive_ln).html)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def showInvoice(prExt: PaymentRequestExt): Unit =
    runInFutureProcessOnUI(QRActivity.get(prExt.raw.toUpperCase, qrSize), onFail) { bitmap =>
      val amountHuman = LNParams.denomination.parsedWithSign(prExt.pr.amount.get, Colors.totalZero)
      def share: Unit = runInFutureProcessOnUI(shareData(bitmap, prExt.raw), onFail)(none)
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy prExt.raw)
      qrViewHolder.qrCode setOnClickListener onButtonTap(WalletApp.app copy prExt.raw)
      qrViewHolder.qrShare setOnClickListener onButtonTap(share)
      qrViewHolder.qrLabel setText amountHuman.html
      qrViewHolder.qrCode setImageBitmap bitmap
      hashOfInterest = prExt.pr.paymentHash
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
    subscription.unsubscribe
    super.onDestroy
  }
}
