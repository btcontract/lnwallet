package com.btcontract.wallettest.sheets

import android.view.{LayoutInflater, View, ViewGroup}
import com.btcontract.wallettest.{BaseActivity, ExternalDataChecker, R, WalletApp}
import com.journeyapps.barcodescanner.{BarcodeCallback, BarcodeResult, BarcodeView}
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import android.widget.ImageButton
import immortan.utils.InputParser
import immortan.crypto.Tools
import android.os.Bundle


class ScannerBottomSheet(host: BaseActivity, checker: ExternalDataChecker) extends BottomSheetDialogFragment with BarcodeCallback { me =>
  var lastAttempt: Long = System.currentTimeMillis
  var barcodeReader: BarcodeView = _
  var flashlight: ImageButton = _

  def pauseBarcodeReader: Unit = Tools.runAnd(barcodeReader setTorch false)(barcodeReader.pause)
  def resumeBarcodeReader: Unit = Tools.runAnd(barcodeReader decodeContinuous me)(barcodeReader.resume)
  override def onResume: Unit = Tools.runAnd(resumeBarcodeReader)(super.onResume)
  override def onStop: Unit = Tools.runAnd(pauseBarcodeReader)(super.onStop)

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View =
    inflater.inflate(R.layout.sheet_scanner, container, false)

  override def onViewCreated(view: View, savedState: Bundle): Unit = {
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    flashlight = view.findViewById(R.id.flashlight).asInstanceOf[ImageButton]
    flashlight setOnClickListener host.onButtonTap(toggleTorch)
  }

  type Points = java.util.List[com.google.zxing.ResultPoint]
  override def possibleResultPoints(points: Points): Unit = Tools.none
  override def barcodeResult(res: BarcodeResult): Unit = Option(res.getText) foreach {
    scannedText => if (System.currentTimeMillis - lastAttempt > 2000) tryParseQR(scannedText)
  }

  def tryParseQR(scannedText: String): Unit = {
    def successfulDecode: Unit = Tools.runAnd(dismiss)(checker checkExternalData checker.noneRunnable)
    def fail(err: Throwable): Unit = Tools.runAnd(WalletApp.app quickToast err.getMessage)(barcodeReader.resume)
    host.runInFutureProcessOnUI(InputParser recordValue scannedText, fail)(_ => successfulDecode)
    lastAttempt = System.currentTimeMillis
    pauseBarcodeReader
  }

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