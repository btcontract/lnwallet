package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import com.lightning.walletapp.BaseActivity.StringOps
import android.widget.RelativeLayout
import android.os.Bundle
import android.view.View
import immortan.LNParams


class HubActivity extends BaseActivity with ExternalDataChecker with ChoiceReceiver { me =>
  private[this] lazy val contentWindow = findViewById(R.id.contentWindow).asInstanceOf[RelativeLayout]
  private val CHOICE_RECEIVE_TAG = "choiceReceiveTag"

  override def onResume: Unit = {
    super.onResume
  }

  override def checkExternalData: Unit = {

  }

  override def onChoiceMade(tag: String, pos: Int): Unit = {

  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def bringUpScanner(view: View): Unit = callScanner(me)

  def bringReceiveOptions(view: View): Unit = {
    val options = Array(dialog_receive_btc, dialog_receive_ln).map(res => getString(res).html)
    val list = makeChoiceList(options, android.R.layout.simple_expandable_list_item_1)
    val sheet = new sheets.ChoiceBottomSheet(list, CHOICE_RECEIVE_TAG, me)
    sheet.show(getSupportFragmentManager, CHOICE_RECEIVE_TAG)
  }
}
