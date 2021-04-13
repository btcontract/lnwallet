package com.lightning.walletapp.sqlite

import immortan.sqlite.{SQLiteTx, TxTable}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet
import com.lightning.walletapp.WalletApp
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi
import immortan.TxDescription


class SQLiteTxExtended(app: WalletApp, db: DBInterfaceSQLiteAndroidMisc) extends SQLiteTx(db) {
  override def updConfidence(confidenceChanged: ElectrumWallet.TransactionConfidenceChanged): Unit = {
    super.updConfidence(confidenceChanged)
    app.sqlNotify(TxTable.table)
  }

  override def updDoubleSpent(chainTxId: ByteVector32): Unit = {
    super.updDoubleSpent(chainTxId)
    app.sqlNotify(TxTable.table)
  }

  override def putTx(event: ElectrumWallet.TransactionReceived, description: TxDescription, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit = {
    super.putTx(event, description, balanceSnap, fiatRateSnap)
    app.sqlNotify(TxTable.table)
  }
}
