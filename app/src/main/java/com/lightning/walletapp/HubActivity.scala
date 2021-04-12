package com.lightning.walletapp

import immortan.crypto.Tools._
import com.lightning.walletapp.R.string._
import immortan.{LNParams, RemoteNodeInfo}
import android.widget.{LinearLayout, ListView, RelativeLayout, TextView}
import immortan.utils.{BitcoinUri, InputParser, LNUrl, PaymentRequestExt}
import com.lightning.walletapp.BaseActivity.StringOps
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.mmin18.widget.RealtimeBlurView
import org.ndeftools.Message
import android.os.Bundle
import android.view.View


class HubActivity extends NfcReaderActivity with BaseActivity with ExternalDataChecker with ChoiceReceiver { me =>
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]

  private[this] lazy val topInfoLayout = findViewById(R.id.topInfoLayout).asInstanceOf[LinearLayout]
  private[this] lazy val topBlurringArea = findViewById(R.id.topBlurringArea).asInstanceOf[RealtimeBlurView]

  private[this] lazy val bottomBlurringArea = findViewById(R.id.bottomBlurringArea).asInstanceOf[RealtimeBlurView]
  private[this] lazy val bottomActionBar = findViewById(R.id.bottomActionBar).asInstanceOf[LinearLayout]

  private[this] lazy val itemsList = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] lazy val totalBalance = findViewById(R.id.totalBalance).asInstanceOf[TextView]
  private[this] lazy val totalFiatBalance = findViewById(R.id.totalFiatBalance).asInstanceOf[TextView]
  private[this] lazy val fiatUnitPriceAndChange = findViewById(R.id.fiatUnitPriceAndChange).asInstanceOf[TextView]
  private val CHOICE_RECEIVE_TAG = "choiceReceiveTag"

  // NFC

  def readEmptyNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def readNonNdefMessage: Unit = WalletApp.app quickToast error_nothing_useful
  def onNfcStateChange(ok: Boolean): Unit = none
  def onNfcFeatureNotFound: Unit = none
  def onNfcStateDisabled: Unit = none
  def onNfcStateEnabled: Unit = none

  def readNdefMessage(nfcMessage: Message): Unit =
    runInFutureProcessOnUI(InputParser recordValue ndefMessageString(nfcMessage),
      _ => WalletApp.app quickToast error_nothing_useful)(_ => me checkExternalData noneRunnable)

  // IMPLEMENTATIONS

  override def onResume: Unit = {
    checkExternalData(noneRunnable)
    super.onResume
  }

  override def checkExternalData(whenNone: Runnable): Unit =
    InputParser.checkAndMaybeErase {
      case _: RemoteNodeInfo =>
      case _: PaymentRequestExt =>
      case _: BitcoinUri =>
      case _: LNUrl =>
      case _ => whenNone.run
    }

  override def onChoiceMade(tag: String, pos: Int): Unit = {
//    val body = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null)
//    val rateManager = new RateManager(body, None, Map("usd" -> 57500D), "usd")
//    val alertBuilder = titleBodyAsViewBuilder(getString(dialog_receive_ln_title), rateManager.content)
//    mkCheckFormNeutral(_.dismiss, none, _.dismiss, alertBuilder, dialog_ok, dialog_cancel, dialog_split)
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      topInfoLayout post UITask(topBlurringArea setHeightTo topInfoLayout)
      bottomActionBar post UITask(bottomBlurringArea setHeightTo bottomActionBar)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  // VIEW HANDLERS

  def bringSettings(view: View): Unit = {

  }

  def bringSendFromClipboard(view: View): Unit = {
    def explain: Unit = snack(contentWindow, getString(error_nothing_in_clipboard).html, dialog_ok, _.dismiss)
    runInFutureProcessOnUI(InputParser.parse(WalletApp.app.getBufferUnsafe), _ => explain)(_ => me checkExternalData explain)
  }

  def bringScanner(view: View): Unit = callScanner(me)

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(res => getString(res).html)
    val list = makeChoiceList(options, android.R.layout.simple_expandable_list_item_1)
    val sheet = new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me)
    sheet.show(getSupportFragmentManager, CHOICE_RECEIVE_TAG)
  }
}
