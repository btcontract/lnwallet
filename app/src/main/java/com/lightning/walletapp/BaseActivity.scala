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
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import fr.acinq.bitcoin.{ByteVector32, Crypto, Satoshi}
import android.text.{Editable, Html, Spanned, TextWatcher}
import androidx.core.content.{ContextCompat, FileProvider}
import immortan.crypto.Tools.{Fiat2Btc, Any2Some, none, runAnd}
import immortan.{Channel, LNParams, PaymentDescription, PaymentInfo}
import fr.acinq.eclair.blockchain.fee.{FeeratePerKw, FeeratePerVByte}
import com.google.android.material.snackbar.{BaseTransientBottomBar, Snackbar}
import immortan.utils.{BitcoinUri, Denomination, InputParser, PaymentRequestExt}
import android.widget.{ArrayAdapter, Button, EditText, ImageView, LinearLayout, ListView, TextView}

import com.cottacush.android.currencyedittext.CurrencyEditText
import com.google.android.material.textfield.TextInputLayout
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.lightning.walletapp.BaseActivity.StringOps
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.eclair.transactions.Transactions
import androidx.appcompat.widget.AppCompatButton
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.slider.Slider
import android.graphics.Bitmap.Config.ARGB_8888
import androidx.appcompat.app.AppCompatActivity
import android.text.method.LinkMovementMethod
import fr.acinq.eclair.payment.PaymentRequest
import com.google.zxing.qrcode.QRCodeWriter
import fr.acinq.eclair.channel.Commitments
import androidx.appcompat.app.AlertDialog
import scala.language.implicitConversions
import android.content.pm.PackageManager
import android.view.View.OnClickListener
import androidx.core.graphics.ColorUtils
import androidx.core.app.ActivityCompat
import scala.concurrent.Future
import android.os.Bundle


object BaseActivity {
  implicit class StringOps(source: String) {
    def shortAddress: String = s"${source take 4}&#160;<sup><small><small>&#8230;</small></small></sup>&#160;${source takeRight 4}"
    def humanFour: String = source.grouped(4).mkString(s"\u0020")
    def html: Spanned = Html.fromHtml(source)
  }

  def formattedBitcoinUri(uri: BitcoinUri): String = {
    val formattedLabel = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
    val formattedMessage = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
    formattedLabel + formattedMessage
  }
}

object Colors {
  val cardZero = "#777777"
  val totalZero = "#555555"
  val btcCardZero = "#B38722"
  val lnCardZero = "#7D7DB8"
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

  override def onBackPressed: Unit =
    removeCurrentSnack(super.onBackPressed)

  def INIT(state: Bundle): Unit

  // Helpers

  def contrastedTextColor(color: Int): Int = {
    val whiteContrast = ColorUtils.calculateContrast(WHITE, color)
    val blackContrast = ColorUtils.calculateContrast(BLACK, color)
    if (whiteContrast > blackContrast * 3.75) BLACK else WHITE
  }

  def share(text: String): Unit = startActivity {
    val share = new Intent setAction Intent.ACTION_SEND
    share.setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)
  }

  def removeCurrentSnack(onNoBar: => Unit): Unit = currentSnackbar match {
    case Some(snackBar) => runAnd { snackBar.dismiss } { currentSnackbar = None }
    case None => onNoBar
  }

  def snack(parent: View, msg: CharSequence, actionRes: Int, fun: Snackbar => Unit): Unit = try {
    val bottomSnackbar: Snackbar = Snackbar.make(parent, msg, BaseTransientBottomBar.LENGTH_INDEFINITE)
    bottomSnackbar.getView.findViewById(com.google.android.material.R.id.snackbar_text).asInstanceOf[TextView].setMaxLines(15)

    val listener = me onButtonTap {
      currentSnackbar = None
      fun(bottomSnackbar)
    }

    bottomSnackbar.setAction(actionRes, listener).show
    currentSnackbar = Some(bottomSnackbar)
  } catch none

  def onButtonTap(fun: => Unit): OnClickListener = new OnClickListener {
    def onClick(view: View): Unit = fun
  }

  def onTextChange(fun: CharSequence => Unit): TextWatcher = new TextWatcher {
    override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int): Unit = fun(c)
    override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int): Unit = none
    override def afterTextChanged(e: Editable): Unit = none
  }

  def runInFutureProcessOnUI[T](fun: => T, no: Throwable => Unit)(ok: T => Unit): Unit =
    runFutureProcessOnUI[T](Future(fun), no)(ok)

  def runFutureProcessOnUI[T](fun: Future[T], no: Throwable => Unit)(ok: T => Unit): Unit = fun onComplete {
    case Success(result) => UITask(ok apply result).run case Failure(error) => UITask(no apply error).run
  }

  def setVis(isVisible: Boolean, view: View): Unit = {
    val nextMode = if (isVisible) View.VISIBLE else View.GONE
    if (view.getVisibility != nextMode) view.setVisibility(nextMode)
  }

  implicit def UITask(fun: => Any): TimerTask = {
    val runnableExec = new Runnable { override def run: Unit = fun }
    new TimerTask { def run: Unit = me runOnUiThread runnableExec }
  }

  // Builders

  def makeChoiceList[T <: Object](actions: Array[T], itemId: Int = android.R.layout.simple_list_item_1): ListView = {
    val list = getLayoutInflater.inflate(R.layout.frag_list, null).asInstanceOf[ListView]
    list setAdapter new ArrayAdapter(me, itemId, actions)
    list
  }

  implicit def str2View(textFieldData: CharSequence): LinearLayout = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    clickableTextField(view findViewById R.id.titleTip) setText textFieldData
    view setBackgroundColor 0x22AAAAAA
    view
  }

  def updateView2Color(oldView: View, newText: String, colorRes: Int): View = {
    val titleTip = oldView.findViewById(R.id.titleTip).asInstanceOf[TextView]
    oldView setBackgroundColor ContextCompat.getColor(me, colorRes)
    titleTip setText s"<font color=#FFFFFF>$newText</font>".html
    oldView
  }

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def titleBodyAsViewBuilder(title: View, body: View): AlertDialog.Builder = new AlertDialog.Builder(me).setCustomTitle(title).setView(body)
  def titleBodyAsViewWithNegBuilder(neg: Int, title: View, body: View): AlertDialog.Builder = titleBodyAsViewBuilder(title, body).setNegativeButton(neg, null)
  def onFail(error: CharSequence): Unit = UITask(me showForm titleBodyAsViewWithNegBuilder(dialog_ok, null, error).create).run
  def onFail(error: Throwable): Unit = onFail(error.toString)

  def getPositiveButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_POSITIVE)
  def getNegativeButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEGATIVE)
  def getNeutralButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEUTRAL)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int): AlertDialog = {
    // Create alert dialog where NEGATIVE button removes a dialog AND calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noRes) bld.setNegativeButton(noRes, null)
    if (-1 != okRes) bld.setPositiveButton(okRes, null)
    val alert = showForm(bld.create)

    val posAct = me onButtonTap {
      ok(alert)
    }

    val negAct = me onButtonTap {
      alert.dismiss
      no
    }

    if (-1 != noRes) getNegativeButton(alert) setOnClickListener negAct
    if (-1 != okRes) getPositiveButton(alert) setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit,
                         bld: AlertDialog.Builder, okRes: Int, noRes: Int, neutralRes: Int): AlertDialog = {

    if (-1 != neutralRes) bld.setNeutralButton(neutralRes, null)
    val alert = mkCheckForm(ok, no, bld, okRes, noRes)

    val neutralAct = me onButtonTap {
      neutral(alert)
    }

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralRes) getNeutralButton(alert) setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog): AlertDialog = {
    // First, make sure it does not blow up if called on destroyed activity, then bound its width in case if this is a tablet, finally attempt to make dialog links clickable
    try alertDialog.show catch none finally if (WalletApp.app.scrWidth > 2.3) alertDialog.getWindow.setLayout(WalletApp.app.maxDialog.toInt, ViewGroup.LayoutParams.WRAP_CONTENT)
    try clickableTextField(alertDialog findViewById android.R.id.message) catch none
    alertDialog.setCanceledOnTouchOutside(false)
    alertDialog
  }

  // Scanner

  final private val scannerRequestCode = 101

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

  class RateManager(val content: ViewGroup, extraText: Option[String], visHintRes: Int, rates: Fiat2Btc, fiatCode: String) {
    val fiatInputAmount: CurrencyEditText = content.findViewById(R.id.fiatInputAmount).asInstanceOf[CurrencyEditText]
    val fiatInputAmountHint: TextView = content.findViewById(R.id.fiatInputAmountHint).asInstanceOf[TextView]
    val inputAmount: CurrencyEditText = content.findViewById(R.id.inputAmount).asInstanceOf[CurrencyEditText]
    val inputAmountHint: TextView = content.findViewById(R.id.inputAmountHint).asInstanceOf[TextView]
    val hintFiatDenom: TextView = clickableTextField(content findViewById R.id.hintFiatDenom)
    val hintDenom: TextView = clickableTextField(content findViewById R.id.hintDenom)

    val extraInputOption: TextView = content.findViewById(R.id.extraInputOption).asInstanceOf[TextView]
    val extraInputVisibility: TextView = content.findViewById(R.id.extraInputVisibility).asInstanceOf[TextView]
    val extraInputLayout: TextInputLayout = content.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput: EditText = content.findViewById(R.id.extraInput).asInstanceOf[EditText]

    def updateButton(button: Button, isEnabled: Boolean): TimerTask = UITask {
      val alpha = if (isEnabled) 1F else 0.3F
      button.setEnabled(isEnabled)
      button.setAlpha(alpha)
    }

    def updateText(value: MilliSatoshi): Unit = runAnd(inputAmount.requestFocus)(inputAmount setText value.truncateToSatoshi.toLong.toString)
    def bigDecimalFrom(input: CurrencyEditText, times: Long = 1L): BigDecimal = (input.getNumericValueBigDecimal: BigDecimal) * times
    def resultExtraInput: Option[String] = Option(extraInput.getText.toString).filterNot(_.trim.isEmpty)
    def resultMsat: MilliSatoshi = MilliSatoshi(bigDecimalFrom(inputAmount, times = 1000L).toLong)
    def resultSat: Satoshi = resultMsat.truncateToSatoshi

    def updatedFiat: String =
      WalletApp.msatInFiat(rates, fiatCode)(resultMsat)
        .filter(0D.!=).map(Denomination.formatFiatPrecise.format)
        .getOrElse(null)

    def updatedSat: String =
      WalletApp.currentRate(rates, fiatCode)
        .map(perBtc => bigDecimalFrom(fiatInputAmount) / perBtc)
        .filter(0D.!=).map(Denomination.btcBigDecimal2MSat)
        .map(LNParams.denomination.asString)
        .getOrElse(null)

    extraText match {
      case Some(hintText) =>
        val revealExtraInputListener = onButtonTap {
          extraInputLayout setVisibility View.VISIBLE
          extraInputOption setVisibility View.GONE
        }

        extraInputLayout setHint hintText
        extraInputOption setText hintText
        extraInputVisibility setText visHintRes
        extraInputOption setOnClickListener revealExtraInputListener
        extraInputVisibility setOnClickListener revealExtraInputListener

      case None =>
        extraInputOption setVisibility View.GONE
        extraInputVisibility setVisibility View.GONE
    }

    fiatInputAmount addTextChangedListener onTextChange { _ => if (fiatInputAmount.hasFocus) inputAmount setText updatedSat }
    inputAmount addTextChangedListener onTextChange { _ => if (inputAmount.hasFocus) fiatInputAmount setText updatedFiat }
    inputAmountHint setText LNParams.denomination.sign.toUpperCase
    fiatInputAmountHint setText fiatCode.toUpperCase
    inputAmount.requestFocus
  }

  class FeeView(val content: View) {
    val feeRate: TextView = content.findViewById(R.id.feeRate).asInstanceOf[TextView]
    val txIssues: TextView = content.findViewById(R.id.txIssues).asInstanceOf[TextView]
    val bitcoinFee: TextView = content.findViewById(R.id.bitcoinFee).asInstanceOf[TextView]
    val fiatFee: TextView = content.findViewById(R.id.fiatFee).asInstanceOf[TextView]

    val customFeerate: Slider = content.findViewById(R.id.customFeerate).asInstanceOf[Slider]
    val customFeerateOption: TextView = content.findViewById(R.id.customFeerateOption).asInstanceOf[TextView]
    var rate: FeeratePerKw = _

    def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): TimerTask = UITask {
      feeRate setText getString(dialog_fee_sat_vbyte).format(rate.toLong / 1000).html
      setVis(feeOpt.isDefined, bitcoinFee)
      setVis(feeOpt.isDefined, fiatFee)
      setVis(showIssue, txIssues)

      feeOpt.foreach { fee =>
        bitcoinFee setText LNParams.denomination.parsedWithSign(fee, Colors.cardZero).html
        fiatFee setText WalletApp.currentMsatInFiatHuman(fee).html
      }
    }

    customFeerateOption setOnClickListener onButtonTap {
      val currentFeerate = FeeratePerVByte(rate).feerate.toLong
      customFeerate.setValueTo(currentFeerate * 10)
      customFeerate.setValue(currentFeerate)
      customFeerate.setValueFrom(1L)

      customFeerateOption setVisibility View.GONE
      customFeerate setVisibility View.VISIBLE
    }
  }

  // Guards and send/receive helpers

  def lnSendGuard(prExt: PaymentRequestExt, container: View)(onOK: Option[MilliSatoshi] => Unit): Unit = LNParams.cm.checkIfSendable(prExt.pr.paymentHash) match {
    case _ if !LNParams.ourInit.features.areSupported(prExt.pr.features.features) => snack(container, getString(error_ln_send_features).html, dialog_ok, _.dismiss)
    case _ if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) => snack(container, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)
    case _ if LNParams.cm.all.values.forall(Channel.isWaiting) => snack(container, getString(error_ln_waiting).html, dialog_ok, _.dismiss)
    case _ if LNParams.isChainDisconnectTooLong => snack(container, getString(error_ln_send_chain_disconnect).html, dialog_ok, _.dismiss)

    case _ if LNParams.cm.allSortedSendable.last.commits.availableForSend < LNParams.minPayment =>
      val reserveHuman = LNParams.denomination.parsedWithSign(-LNParams.cm.allSortedSendable.head.commits.availableForSend, Colors.cardZero)
      snack(container, getString(error_ln_send_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)

    case _ if prExt.pr.amount.exists(_ < LNParams.minPayment) =>
      val requestedHuman = LNParams.denomination.parsedWithSign(prExt.pr.amount.get, Colors.cardZero)
      val minHuman = LNParams.denomination.parsedWithSign(LNParams.minPayment, Colors.cardZero)
      val msg = getString(error_ln_send_small).format(requestedHuman, minHuman).html
      snack(container, msg, dialog_ok, _.dismiss)

    case _ if prExt.hasSplitIssue => snack(container, getString(error_ln_send_split).html, dialog_ok, _.dismiss)
    case _ if prExt.pr.isExpired => snack(container, getString(error_ln_send_expired).html, dialog_ok, _.dismiss)
    case Some(PaymentInfo.NOT_SENDABLE_IN_FLIGHT) => snack(container, getString(error_ln_send_in_flight).html, dialog_ok, _.dismiss)
    case Some(PaymentInfo.NOT_SENDABLE_SUCCESS) => snack(container, getString(error_ln_send_done_already).html, dialog_ok, _.dismiss)
    case _ if prExt.pr.prefix != PaymentRequest.prefixes(LNParams.chainHash) => snack(container, getString(error_ln_send_network).html, dialog_ok, _.dismiss)
    case _ => onOK(prExt.pr.amount)
  }

  def lnReceiveGuard(container: View)(onOk: => Unit): Unit = LNParams.cm.allSortedReceivable.lastOption match {
    case _ if !LNParams.cm.all.values.exists(Channel.isOperationalOrWaiting) => snack(container, getString(error_ln_no_chans).html, dialog_ok, _.dismiss)
    case _ if LNParams.cm.all.values.forall(Channel.isWaiting) => snack(container, getString(error_ln_waiting).html, dialog_ok, _.dismiss)
    case None => snack(container, getString(error_ln_receive_no_update).html, dialog_ok, _.dismiss)

    case Some(cnc) =>
      if (cnc.commits.availableForReceive < 0L.msat) {
        val reserveHuman = LNParams.denomination.parsedWithSign(-cnc.commits.availableForReceive, Colors.cardZero)
        snack(container, getString(error_ln_receive_reserve).format(reserveHuman).html, dialog_ok, _.dismiss)
      } else onOk
  }

  abstract class OffChainSender(val maxSendable: MilliSatoshi, val minSendable: MilliSatoshi) {
    val body: ViewGroup = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
    val manager = new RateManager(body, getString(dialog_add_ln_memo).toSome, dialog_visibility_private, LNParams.fiatRatesInfo.rates, WalletApp.fiatCode)
    val alert: AlertDialog = getAlertDialog

    val canSendHuman: String = LNParams.denomination.parsedWithSign(maxSendable, Colors.cardZero)
    val canSendFiatHuman: String = WalletApp.currentMsatInFiatHuman(maxSendable)

    manager.hintFiatDenom.setText(getString(dialog_can_send).format(canSendFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_can_send).format(canSendHuman).html)

    manager.inputAmount addTextChangedListener onTextChange { _ =>
      manager.updateButton(getPositiveButton(alert), isPayEnabled).run
      manager.updateButton(getNeutralButton(alert), isNeutralEnabled).run
    }

    def neutral(alert: AlertDialog): Unit
    def send(alert: AlertDialog): Unit
    def getAlertDialog: AlertDialog
    def isNeutralEnabled: Boolean
    def isPayEnabled: Boolean
  }

  abstract class OffChainReceiver(maxReceivable: MilliSatoshi, minReceivable: MilliSatoshi, lnBalance: MilliSatoshi) {
    val body: ViewGroup = getLayoutInflater.inflate(R.layout.frag_input_off_chain, null).asInstanceOf[ViewGroup]
    // Currently a single relatively smallest channel is used to improve privacy and maximize delivery chances
    val commits: Commitments = LNParams.cm.maxReceivable(LNParams.cm.allSortedReceivable).head.commits
    val manager: RateManager = getManager

    val finalMaxReceivable: MilliSatoshi = maxReceivable min commits.availableForReceive
    val finalMinReceivable: MilliSatoshi = minReceivable max LNParams.minPayment

    val canReceiveHuman: String = LNParams.denomination.parsedWithSign(finalMaxReceivable, Colors.cardZero)
    val canReceiveFiatHuman: String = WalletApp.currentMsatInFiatHuman(finalMaxReceivable)

    def receive(alert: AlertDialog): Unit = {
      val preimage: ByteVector32 = randomBytes32
      val hash: ByteVector32 = Crypto.sha256(preimage)
      val invoiceKey = LNParams.secret.keys.fakeInvoiceKey(hash)
      val description = getDescription(manager.resultExtraInput getOrElse new String)
      val hop = List(commits.updateOpt.map(_ extraHop commits.remoteInfo.nodeId).toList)
      val prExt = PaymentRequestExt from PaymentRequest(LNParams.chainHash, Some(manager.resultMsat), hash, invoiceKey, description.invoiceText, LNParams.incomingFinalCltvExpiry, hop)
      val chainFee = Transactions.weight2fee(LNParams.feeRatesInfo.onChainFeeConf.feeEstimator.getFeeratePerKw(LNParams.feeRatesInfo.onChainFeeConf.feeTargets.fundingBlockTarget), 700)
      LNParams.cm.payBag.replaceIncomingPayment(prExt, preimage, description, lnBalance, LNParams.fiatRatesInfo.rates, chainFee.toMilliSatoshi)
      processInvoice(prExt)
      alert.dismiss
    }

    val alert: AlertDialog =
      mkCheckFormNeutral(receive, none, _ => manager.updateText(finalMaxReceivable),
        titleBodyAsViewBuilder(getTitleText, manager.content), dialog_ok, dialog_cancel, dialog_max)

    manager.hintFiatDenom.setText(getString(dialog_can_receive).format(canReceiveFiatHuman).html)
    manager.hintDenom.setText(getString(dialog_can_receive).format(canReceiveHuman).html)
    manager.updateButton(getPositiveButton(alert), isEnabled = false).run

    manager.inputAmount addTextChangedListener onTextChange { _ =>
      val withinBounds = finalMinReceivable <= manager.resultMsat && finalMaxReceivable >= manager.resultMsat
      manager.updateButton(getPositiveButton(alert), isEnabled = withinBounds).run
    }

    def getManager: RateManager
    def getTitleText: CharSequence
    def getDescription(input: String): PaymentDescription
    def processInvoice(prExt: PaymentRequestExt): Unit
  }
}

trait QRActivity extends BaseActivity { me =>
  lazy val qrSize: Int = getResources getDimensionPixelSize R.dimen.qr_size

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
