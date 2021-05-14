package com.lightning.walletapp

import immortan.LNParams
import android.os.Bundle
import android.widget.TextView
import immortan.crypto.Tools.none
import com.ornach.nobobutton.NoboButton
import com.lightning.walletapp.BaseActivity.StringOps
import immortan.utils.{InputParser, PaymentSplit}


class QRSplitActivity extends QRActivity with ExternalDataChecker { me =>
  lazy private[this] val splitQrCaption = findViewById(R.id.splitQrCaption).asInstanceOf[TextView]
  lazy private[this] val splitQrMore = findViewById(R.id.splitQrMore).asInstanceOf[NoboButton]
  lazy private[this] val qrViewHolder = new QRViewHolder(me findViewById R.id.splitQr)

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_qr_split_invoice)
      val splitCaption = getString(R.string.dialog_split_ln)
      splitQrCaption setText splitCaption.format(new String).html
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def showSplitInvoice(split: PaymentSplit): Unit = {
    val mySplitHuman = LNParams.denomination.parsedWithSign(split.mySplit, Colors.totalZero)
    val remainderHuman = LNParams.denomination.parsedWithSign(split.remainder, Colors.totalZero)
    val remainderHumanDesc = getString(R.string.dialog_split_ln_remain).format(remainderHuman)
    splitQrMore setText s"${me getString R.string.dialog_pay} $mySplitHuman".html

    runInFutureProcessOnUI(QRActivity.get(split.uriWithAllSplits.toUpperCase, qrSize), onFail) { bitmap =>
      def share: Unit = runInFutureProcessOnUI(shareData(bitmap, split.uriWithAllSplits), onFail)(none)
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy split.uriWithAllSplits)
      qrViewHolder.qrCode setOnClickListener onButtonTap(WalletApp.app copy split.uriWithAllSplits)
      qrViewHolder.qrShare setOnClickListener onButtonTap(share)
      qrViewHolder.qrLabel setText remainderHumanDesc.html
      qrViewHolder.qrCode setImageBitmap bitmap
    }
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case split: PaymentSplit if split.isValid => showSplitInvoice(split)
    case _ => finish
  }

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }
}
