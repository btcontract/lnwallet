package com.lightning.walletapp

import R.string._
import fr.acinq.eclair._
import java.util.{Timer, TimerTask}
import scala.util.{Failure, Success}
import android.view.{View, ViewGroup}
import java.io.{File, FileOutputStream}
import android.graphics.{Bitmap, Color}
import android.graphics.Color.{BLACK, WHITE}
import android.content.{DialogInterface, Intent}
import immortan.utils.{Denomination, InputParser}
import immortan.crypto.Tools.{Fiat2Btc, none, runAnd}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import android.text.{Editable, Html, Spanned, TextWatcher}
import androidx.core.content.{ContextCompat, FileProvider}
import com.google.android.material.snackbar.{BaseTransientBottomBar, Snackbar}
import android.widget.{ArrayAdapter, EditText, ImageView, LinearLayout, ListView, TextView}
import com.cottacush.android.currencyedittext.CurrencyEditText
import com.google.android.material.textfield.TextInputLayout
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.lightning.walletapp.BaseActivity.StringOps
import concurrent.ExecutionContext.Implicits.global
import androidx.appcompat.widget.AppCompatButton
import androidx.recyclerview.widget.RecyclerView
import android.graphics.Bitmap.Config.ARGB_8888
import androidx.appcompat.app.AppCompatActivity
import android.text.method.LinkMovementMethod
import com.google.zxing.qrcode.QRCodeWriter
import androidx.appcompat.app.AlertDialog
import scala.language.implicitConversions
import android.content.pm.PackageManager
import android.view.View.OnClickListener
import androidx.core.graphics.ColorUtils
import androidx.core.app.ActivityCompat
import scala.concurrent.Future
import scodec.bits.ByteVector
import android.app.Dialog
import android.os.Bundle
import immortan.LNParams


object BaseActivity {
  val viewMap: Map[Boolean, Int] = Map(true -> View.VISIBLE, false -> View.GONE)

  implicit class StringOps(source: String) {
    def s2hex: String = ByteVector.view(source getBytes "UTF-8").toHex
    def noSpaces: String = source.replace(" ", "").replace("\u00A0", "")
    def humanSix: String = source.grouped(6).mkString(s"\u0020")
    def html: Spanned = Html.fromHtml(source)
  }
}

trait ExternalDataChecker {
  val noneRunnable: Runnable = new Runnable { override def run: Unit = none }
  def checkExternalData(onNothing: Runnable): Unit
}

trait ChoiceReceiver {
  def onChoiceMade(tag: String, pos: Int): Unit
}

trait BaseActivity extends AppCompatActivity { me =>
  var currentSnackbar = Option.empty[Snackbar]
  val btcDenominationDarkZero = "#555555"
  val btcDenominationBtcZero = "#B38722"
  val timer = new Timer

  val goTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    InputParser.DoNotEraseRecordedValue
  }

  val exitTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    runAnd(InputParser.DoNotEraseRecordedValue)(finish)
  }

  override def onCreate(savedActivityState: Bundle): Unit = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityState)
    INIT(savedActivityState)
  }

  override def onDestroy: Unit = {
    super.onDestroy
    timer.cancel
  }

  override def onBackPressed: Unit = currentSnackbar match {
    case Some(bar) => runAnd(bar.dismiss) { currentSnackbar = None }
    case None => super.onBackPressed
  }

  def INIT(state: Bundle): Unit

  def contrastedTextColor(color: Int): Int = {
    val whiteContrast = ColorUtils.calculateContrast(WHITE, color)
    val blackContrast = ColorUtils.calculateContrast(BLACK, color)
    if (whiteContrast > blackContrast * 3.75) BLACK else WHITE
  }

  def share(text: String): Unit = startActivity {
    val share = new Intent setAction Intent.ACTION_SEND
    share.setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)
  }

  def snack(parent: View, msg: CharSequence, actionRes: Int, onTap: Snackbar => Unit): Unit = try {
    val bottomSnackbar: Snackbar = Snackbar.make(parent, msg, BaseTransientBottomBar.LENGTH_INDEFINITE)
    bottomSnackbar.getView.findViewById(com.google.android.material.R.id.snackbar_text).asInstanceOf[TextView].setMaxLines(15)

    val listener = me onButtonTap {
      currentSnackbar = None
      onTap(bottomSnackbar)
    }

    bottomSnackbar.setAction(actionRes, listener).show
    currentSnackbar = Some(bottomSnackbar)
  } catch none

  def onButtonTap(exec: => Unit): OnClickListener = new OnClickListener { def onClick(view: View): Unit = exec }

  def onTextChange(exec: CharSequence => Unit): TextWatcher = new TextWatcher {
    override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int): Unit = exec(c)
    override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int): Unit = none
    override def afterTextChanged(e: Editable): Unit = none
  }

  def runInFutureProcessOnUI[T](fun: => T, no: Throwable => Unit)(ok: T => Unit): Unit =
    runFutureProcessOnUI[T](Future(fun), no)(ok)

  def runFutureProcessOnUI[T](fun: Future[T], no: Throwable => Unit)(ok: T => Unit): Unit = fun onComplete {
    case Success(result) => UITask(ok apply result).run case Failure(error) => UITask(no apply error).run
  }

  implicit def UITask(exec: => Any): TimerTask = {
    val runnableExec = new Runnable { override def run: Unit = exec }
    new TimerTask { def run: Unit = me runOnUiThread runnableExec }
  }

  // Builders

  def makeChoiceList[T <: Object](actions: Array[T], itemId: Int = android.R.layout.simple_list_item_1): ListView = {
    val list = getLayoutInflater.inflate(R.layout.frag_list, null).asInstanceOf[ListView]
    list setAdapter new ArrayAdapter(me, itemId, actions)
    list
  }

  def removeAndProceedWithTimeout(prev: Dialog)(exe: => Unit): Unit = {
    // Add some delay between dismissing previous popup and doing something next
    timer.schedule(exe, 225)
    prev.dismiss
  }

  implicit def str2View(textFieldData: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    clickableTextField(view findViewById R.id.titleTip) setText textFieldData
    view setBackgroundColor 0x22AAAAAA
    view
  }

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def updateView2Blue(oldView: View, newText: String): View = {
    val titleTip = oldView.findViewById(R.id.titleTip).asInstanceOf[TextView]
    oldView setBackgroundColor ContextCompat.getColor(me, R.color.ln)
    titleTip setText s"<font color=#FFFFFF>$newText</font>".html
    oldView
  }

  def simpleTextBuilder(msg: CharSequence): AlertDialog.Builder = new AlertDialog.Builder(me).setMessage(msg)
  def simpleTextWithNegBuilder(neg: Int, msg: CharSequence): AlertDialog.Builder = simpleTextBuilder(msg).setNegativeButton(neg, null)

  def titleBodyAsViewBuilder(title: View, body: View): AlertDialog.Builder = new AlertDialog.Builder(me).setCustomTitle(title).setView(body)
  def titleBodyAsViewWithNegBuilder(neg: Int, title: View, body: View): AlertDialog.Builder = titleBodyAsViewBuilder(title, body).setNegativeButton(neg, null)
  def onFail(error: CharSequence): Unit = UITask(me showForm titleBodyAsViewWithNegBuilder(dialog_ok, null, error).create).run
  def onFail(error: Throwable): Unit = onFail(error.getMessage)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int): AlertDialog = {
    // Create alert dialog where NEGATIVE button removes a dialog AND calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noRes) bld.setNegativeButton(noRes, null)
    if (-1 != okRes) bld.setPositiveButton(okRes, null)

    val alert = showForm(bld.create)
    val posAct = me onButtonTap ok(alert)
    val negAct = me onButtonTap removeAndProceedWithTimeout(alert)(no)
    if (-1 != noRes) alert getButton DialogInterface.BUTTON_NEGATIVE setOnClickListener negAct
    if (-1 != okRes) alert getButton DialogInterface.BUTTON_POSITIVE setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int, neutralRes: Int): AlertDialog = {
    if (-1 != neutralRes) bld.setNeutralButton(neutralRes, null)
    val alert = mkCheckForm(ok, no, bld, okRes, noRes)
    val neutralAct = me onButtonTap neutral(alert)

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralRes) alert getButton DialogInterface.BUTTON_NEUTRAL setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog): AlertDialog = {
    // This may be called after a host activity is destroyed!
    alertDialog setCanceledOnTouchOutside false

    // First, make sure it does not blow up if called on destroyed activity, then bound its width in case if this is a tablet, finally attempt to make dialog links clickable
    try alertDialog.show catch none finally if (WalletApp.app.scrWidth > 2.3) alertDialog.getWindow.setLayout(WalletApp.app.maxDialog.toInt, ViewGroup.LayoutParams.WRAP_CONTENT)
    try clickableTextField(alertDialog findViewById android.R.id.message) catch none
    alertDialog
  }

  // Scanner

  final val scannerRequestCode = 101

  type GrantResults = Array[Int]

  override def onRequestPermissionsResult(reqCode: Int, permissions: Array[String], grantResults: GrantResults): Unit =
    if (reqCode == scannerRequestCode && grantResults.nonEmpty && grantResults.head == PackageManager.PERMISSION_GRANTED)
      callScanner(null)

  def callScanner(checker: ExternalDataChecker): Unit = {
    val allowed = ContextCompat.checkSelfPermission(me, android.Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED
    if (allowed) new sheets.ScannerBottomSheet(me, checker).show(getSupportFragmentManager, "scanner-bottom-sheet-fragment")
    else ActivityCompat.requestPermissions(me, Array(android.Manifest.permission.CAMERA), scannerRequestCode)
  }

  // Fiat / BTC converter

  val bigDecimalValue: CurrencyEditText => BigDecimal = _.getNumericValueBigDecimal

  class RateManager(val content: View, extraHint: Option[String], rates: Fiat2Btc, fiatCode: String) {
    val inputAmount: CurrencyEditText = content.findViewById(R.id.inputAmount).asInstanceOf[CurrencyEditText]
    val fiatInputAmount: CurrencyEditText = content.findViewById(R.id.fiatInputAmount).asInstanceOf[CurrencyEditText]
    val hintFiatDenom: TextView = clickableTextField(content findViewById R.id.hintFiatDenom)
    val hintDenom: TextView = clickableTextField(content findViewById R.id.hintDenom)

    val inputAmountHint: TextView = content.findViewById(R.id.inputAmountHint).asInstanceOf[TextView]
    val fiatInputAmountHint: TextView = content.findViewById(R.id.fiatInputAmountHint).asInstanceOf[TextView]
    val extraInputLayout: TextInputLayout = content.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput: EditText = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
    def result: MilliSatoshi = MilliSatoshi(bigDecimalValue(inputAmount).toLong * 1000L)

    def updatedSat: String =
      WalletApp.currentRate(rates, fiatCode).map(bigDecimalValue(fiatInputAmount) / _).filter(0D.!=)
        .map(Denomination.btcBigDecimal2MSat).map(LNParams.denomination.asString).getOrElse(null)

    def updatedFiat: String =
      WalletApp.msatInFiat(rates, fiatCode)(result).filter(0D.!=)
        .map(Denomination.formatFiat.format).getOrElse(null)

    extraHint match { case Some(hint) => extraInputLayout setHint hint case None => extraInputLayout setVisibility View.GONE }
    fiatInputAmount addTextChangedListener onTextChange { _ => if (fiatInputAmount.hasFocus) inputAmount setText updatedSat }
    inputAmount addTextChangedListener onTextChange { _ => if (inputAmount.hasFocus) fiatInputAmount setText updatedFiat }
    inputAmountHint setText LNParams.denomination.sign.toUpperCase
    fiatInputAmountHint setText fiatCode.toUpperCase
    inputAmount.requestFocus
  }

  class SpinnerPopup(lg: Option[CharSequence], sm: Option[CharSequence] = None) {
    val progressBar: View = getLayoutInflater.inflate(R.layout.frag_progress_bar, null)
    val builder: AlertDialog.Builder = titleBodyAsViewBuilder(title = null, progressBar)
    val alert: AlertDialog = mkCheckForm(none, cancel, builder, okRes = -1, dialog_cancel)
    var isCancelled: Boolean = false

    val largeText: TextView = progressBar.findViewById(R.id.largeText).asInstanceOf[TextView]
    val smallText: TextView = progressBar.findViewById(R.id.smallText).asInstanceOf[TextView]

    lg match { case Some(text) => largeText setText text case None => largeText setVisibility View.GONE }
    sm match { case Some(text) => smallText setText text case None => smallText setVisibility View.GONE }
    def cancel: Unit = removeAndProceedWithTimeout(alert) { isCancelled = true }
  }
}

trait QRActivity extends BaseActivity { me =>
  lazy val qrSize: Int = getResources getDimensionPixelSize R.dimen.bitmap_qr_size

  def shareData(bitmap: Bitmap, bech32: String): Unit = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 85, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "com.lightning.walletapp", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain" addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_TEXT, bech32).putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }

  class QRViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val qrCode: ImageView = itemView.findViewById(R.id.qrCode).asInstanceOf[ImageView]
    val qrLabel: TextView = itemView.findViewById(R.id.qrLabel).asInstanceOf[TextView]
    val qrShare: AppCompatButton = itemView.findViewById(R.id.qrShare).asInstanceOf[AppCompatButton]
    val qrCopy: AppCompatButton = itemView.findViewById(R.id.qrCopy).asInstanceOf[AppCompatButton]
  }
}

object QRActivity {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(data: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size, hints)
    val (width, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](width * height)

    for {
      xPos <- 0 until width
      yPos <- 0 until height
      isBlack = bitMatrix.get(xPos, yPos)
      color = if (isBlack) Color.BLACK else Color.WHITE
    } pixels(yPos * width + xPos) = color

    val qrBitmap = Bitmap.createBitmap(width, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, size, 0, 0, width, height)
    qrBitmap
  }
}
