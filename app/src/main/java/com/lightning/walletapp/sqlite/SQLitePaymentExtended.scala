package com.lightning.walletapp.sqlite

import immortan.{PaymentAction, PaymentDescription}
import com.lightning.walletapp.{Vibrator, WalletApp}
import immortan.sqlite.{PaymentTable, RelayTable, SQLitePayment}
import fr.acinq.eclair.transactions.RemoteFulfill
import fr.acinq.eclair.wire.FullPaymentTag
import immortan.utils.PaymentRequestExt
import immortan.crypto.Tools.Fiat2Btc
import fr.acinq.bitcoin.ByteVector32
import fr.acinq.eclair.MilliSatoshi


class SQLitePaymentExtended(app: WalletApp, db: DBInterfaceSQLiteAndroidMisc, preimageDb: DBInterfaceSQLiteAndroidEssential) extends SQLitePayment(db, preimageDb) {
  override def addRelayedPreimageInfo(fullTag: FullPaymentTag, preimage: ByteVector32, relayed: MilliSatoshi, earned: MilliSatoshi): Unit = {
    super.addRelayedPreimageInfo(fullTag, preimage, relayed, earned)
    app.sqlNotify(RelayTable.table)
  }

  override def updAbortedOutgoing(paymentHash: ByteVector32): Unit = {
    super.updAbortedOutgoing(paymentHash)
    app.sqlNotify(PaymentTable.table)
  }

  override def updOkOutgoing(fulfill: RemoteFulfill, fee: MilliSatoshi): Unit = {
    super.updOkOutgoing(fulfill, fee)
    app.sqlNotify(PaymentTable.table)
    app.notify(Vibrator.uri)
  }

  override def updOkIncoming(receivedAmount: MilliSatoshi, paymentHash: ByteVector32): Unit = {
    super.updOkIncoming(receivedAmount, paymentHash)
    app.sqlNotify(PaymentTable.table)
    app.notify(Vibrator.uri)
  }

  override def replaceOutgoingPayment(prex: PaymentRequestExt, description: PaymentDescription, action: Option[PaymentAction],
                                      finalAmount: MilliSatoshi, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, chainFee: MilliSatoshi): Unit = {

    super.replaceOutgoingPayment(prex, description, action, finalAmount, balanceSnap, fiatRateSnap, chainFee)
    app.sqlNotify(PaymentTable.table)
  }

  override def replaceIncomingPayment(prex: PaymentRequestExt, preimage: ByteVector32, description: PaymentDescription,
                                      balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, chainFee: MilliSatoshi): Unit = {

    super.replaceIncomingPayment(prex, preimage, description, balanceSnap, fiatRateSnap, chainFee)
    app.sqlNotify(PaymentTable.table)
  }
}
