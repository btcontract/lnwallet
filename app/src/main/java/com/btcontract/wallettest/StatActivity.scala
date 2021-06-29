package com.btcontract.wallettest

import android.os.Bundle
import immortan.LNParams


class StatActivity extends BaseActivity { me =>

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(R.layout.activity_stat)

      WalletApp.txDataBag.db.txWrap {
        val txSummary = WalletApp.txDataBag.txSummary
        val channelTxFeesSummary = LNParams.cm.chanBag.channelTxFeesSummary
        val paymentSummary = LNParams.cm.payBag.paymentSummary
        val relaySummary = LNParams.cm.payBag.relaySummary
      }

    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
}
