package com.lightning.walletapp.sqlite

import immortan.sqlite.{SQLiteTx, TxTable}
import com.lightning.walletapp.{Vibrator, WalletApp}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi
import immortan.TxDescription


class SQLiteTxExtended(app: WalletApp, val db: DBInterfaceSQLiteAndroidMisc) extends SQLiteTx(db) {
  override def updStatus(txid: ByteVector32, depth: Long, isDoubleSpent: Boolean): Unit = {
    super.updStatus(txid, depth, isDoubleSpent)
    app.sqlNotify(TxTable.table)
  }

  override def putTx(event: ElectrumWallet.TransactionReceived, isIncoming: Long, description: TxDescription, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit = {
    super.putTx(event, isIncoming, description, balanceSnap, fiatRateSnap)
    if (event.depth < 1) app.notify(Vibrator.uri)
    app.sqlNotify(TxTable.table)
  }
}
