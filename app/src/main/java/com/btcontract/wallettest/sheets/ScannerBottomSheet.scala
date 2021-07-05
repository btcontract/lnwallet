package com.btcontract.wallettest.sheets

import android.widget.{ImageButton, TextView}
import android.view.{LayoutInflater, View, ViewGroup}
import com.btcontract.wallettest.{BaseActivity, R, WalletApp}
import com.journeyapps.barcodescanner.{BarcodeCallback, BarcodeResult, BarcodeView}
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import com.ornach.nobobutton.NoboButton
import immortan.utils.InputParser
import immortan.crypto.Tools
import android.os.Bundle
import scala.util.Try


class ScannerBottomSheet(host: BaseActivity, onScan: Runnable, onPaste: Runnable) extends BottomSheetDialogFragment with BarcodeCallback { me =>
  var lastAttempt: Long = System.currentTimeMillis
  var barcodeReader: BarcodeView = _
  var flashlight: ImageButton = _

  def pauseBarcodeReader: Unit = Tools.runAnd(barcodeReader setTorch false)(barcodeReader.pause)
  def resumeBarcodeReader: Unit = Tools.runAnd(barcodeReader decodeContinuous me)(barcodeReader.resume)
  def failedScan(err: Throwable): Unit = Tools.runAnd(WalletApp.app quickToast err.getMessage)(resumeBarcodeReader)
  def failedPaste(err: Throwable): Unit = WalletApp.app quickToast err.getMessage
  def successfulScan: Unit = Tools.runAnd(dismiss)(onScan.run)

  override def onDestroy: Unit = Tools.runAnd(barcodeReader.stopDecoding)(super.onStop)
  override def onResume: Unit = Tools.runAnd(resumeBarcodeReader)(super.onResume)
  override def onStop: Unit = Tools.runAnd(pauseBarcodeReader)(super.onStop)

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View =
    inflater.inflate(R.layout.sheet_scanner, container, false)

  override def onViewCreated(view: View, savedState: Bundle): Unit = {
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    flashlight = view.findViewById(R.id.flashlight).asInstanceOf[ImageButton]
    flashlight setOnClickListener host.onButtonTap(toggleTorch)

    // Look into clipboard and prefill a text so user can see what is going to be pasted
    val clipboardContent = view.findViewById(R.id.clipboardContent).asInstanceOf[TextView]
    Try(WalletApp.app.getBufferUnsafe).foreach(clipboardContent.setText)

    view.findViewById(R.id.pasteFromClipbard).asInstanceOf[NoboButton] setOnClickListener host.onButtonTap {
      host.runInFutureProcessOnUI(InputParser recordValue WalletApp.app.getBufferUnsafe, failedPaste)(_ => onPaste.run)
      dismiss
    }
  }

  type Points = java.util.List[com.google.zxing.ResultPoint]
  override def possibleResultPoints(points: Points): Unit = Tools.none

  override def barcodeResult(res: BarcodeResult): Unit = for {
    text <- Option(res.getText) if System.currentTimeMillis - lastAttempt > 2000
    _ = host.runInFutureProcessOnUI(InputParser recordValue text, failedScan)(_ => successfulScan)
    _ = lastAttempt = System.currentTimeMillis
  } pauseBarcodeReader

  def toggleTorch: Unit = {
    val currentTag = flashlight.getTag.asInstanceOf[Int]

    if (currentTag != R.drawable.flashlight_on) {
      flashlight.setImageResource(R.drawable.flashlight_on)
      flashlight.setTag(R.drawable.flashlight_on)
      barcodeReader.setTorch(true)
    } else {
      flashlight.setImageResource(R.drawable.flashlight_off)
      flashlight.setTag(R.drawable.flashlight_off)
      barcodeReader.setTorch(false)
    }
  }
}