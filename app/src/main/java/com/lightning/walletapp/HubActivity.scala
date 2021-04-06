package com.lightning.walletapp

import com.lightning.walletapp.R.string._
import com.aurelhubert.ahbottomnavigation._
import android.widget.FrameLayout
import android.os.Bundle
import immortan.LNParams


class HubActivity extends BaseActivity with AHBottomNavigation.OnTabSelectedListener { me =>
  lazy val bottomNavigation: AHBottomNavigation = findViewById(R.id.bottomNavigation).asInstanceOf[AHBottomNavigation]
  lazy val contentWindow: FrameLayout = findViewById(R.id.contentWindow).asInstanceOf[FrameLayout]

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      setContentView(com.lightning.walletapp.R.layout.activity_hub)
      val wallet = new AHBottomNavigationItem(item_wallet, R.drawable.ic_item_wallet_black_24dp, R.color.accent, "wallet")
      val shopping = new AHBottomNavigationItem(item_shopping, R.drawable.ic_item_shopping_black_24dp, R.color.accent, "shopping")
      val addons = new AHBottomNavigationItem(item_addons, R.drawable.ic_item_add_black_24dp, R.color.accent, "addons")
      bottomNavigation addItems java.util.Arrays.asList(wallet, shopping, addons)
      bottomNavigation setOnTabSelectedListener me
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }

  def onTabSelected(position: Int, tag: String, wasSelected: Boolean): Boolean = true
}
