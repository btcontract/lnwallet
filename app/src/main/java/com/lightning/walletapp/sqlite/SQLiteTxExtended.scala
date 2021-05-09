package com.lightning.walletapp.sqlite

import immortan.sqlite.{SQLiteTx, TxTable}
import com.lightning.walletapp.{Vibrator, WalletApp}
import fr.acinq.bitcoin.{ByteVector32, Satoshi, Transaction}
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.eclair.MilliSatoshi
import immortan.TxDescription


class SQLiteTxExtended(app: WalletApp, val db: DBInterfaceSQLiteAndroidMisc) extends SQLiteTx(db) {
  // When sending a tx locally we know recipent address and user provided memo
  // store this info here to use it when chain wallet receives a sent tx
  var descriptions: Map[ByteVector32, TxDescription] = Map.empty

  override def updStatus(txid: ByteVector32, depth: Long, isDoubleSpent: Boolean): Unit = {
    super.updStatus(txid, depth, isDoubleSpent)
    app.sqlNotify(TxTable.table)
  }

  override def replaceTx(tx: Transaction, depth: Long, received: Satoshi, sent: Satoshi, feeOpt: Option[Satoshi], description: TxDescription, isIncoming: Long, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc): Unit = {
    super.replaceTx(tx, depth, received, sent, feeOpt, description, isIncoming, balanceSnap, fiatRateSnap)
    app.sqlNotify(TxTable.table)
  }
}
