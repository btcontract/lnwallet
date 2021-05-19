package com.lightning.walletapp

import android.os.Bundle
import android.widget.TextView
import immortan.crypto.Tools.none
import immortan.utils.InputParser
import com.ornach.nobobutton.NoboButton
import com.lightning.walletapp.BaseActivity.StringOps
import immortan.{LNParams, PaymentSplit}


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

  def showSplitInvoice(ps: PaymentSplit): Unit = {
    val nextSplitLink = ps.prExt.withNewSplit(newSplit = ps.cmd.split.myPart)
    val leftHuman = LNParams.denomination.parsedWithSign(ps.prExt.splitLeftover - ps.cmd.split.myPart, Colors.totalZero)
    val mySplitHuman = LNParams.denomination.parsedWithSign(ps.cmd.split.myPart, Colors.totalZero)
    splitQrMore.setText(s"${me getString R.string.dialog_pay} $mySplitHuman".html)

    runInFutureProcessOnUI(QRActivity.get(nextSplitLink.toUpperCase, qrSize), onFail) { bitmap =>
      def shareSplitLink: Unit = runInFutureProcessOnUI(shareData(bitmap, nextSplitLink), onFail)(none)
      qrViewHolder.qrLabel setText getString(R.string.dialog_split_ln_left).format(s"<br>$leftHuman").html
      qrViewHolder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy nextSplitLink)
      qrViewHolder.qrCode setOnClickListener onButtonTap(WalletApp.app copy nextSplitLink)
      qrViewHolder.qrShare setOnClickListener onButtonTap(shareSplitLink)
      qrViewHolder.qrCode setImageBitmap bitmap
    }
  }

  override def checkExternalData(whenNone: Runnable): Unit = InputParser.checkAndMaybeErase {
    case ps: PaymentSplit if ps.prExt.splitLeftover - ps.cmd.split.myPart < LNParams.minPayment => finish
    case ps: PaymentSplit if ps.prExt.pr.amount.nonEmpty => showSplitInvoice(ps)
    case _ => finish
  }

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }
}
