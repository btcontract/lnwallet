package fr.acinq.eclair.io

import fr.acinq.eclair._
import fr.acinq.bitcoin.{ByteVector32, DeterministicWallet, Satoshi, Script}
import fr.acinq.eclair.channel.{ChannelVersion, Helpers, LocalParams}
import immortan.{LNParams, RemoteNodeInfo}

import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector


object Peer {
  def makeChannelParams(remoteInfo: RemoteNodeInfo, wallet: EclairWallet, funder: Boolean, fundingAmount: Satoshi, channelVersion: ChannelVersion): LocalParams = {
    val (finalScript, walletStaticPaymentBasepoint) = channelVersion match {
      case v if v.paysDirectlyToWallet =>
        val walletKey = Helpers.getWalletPaymentBasepoint(wallet)
        (Script.write(Script.pay2wpkh(walletKey)), Some(walletKey))
      case _ =>
        (Helpers.getFinalScriptPubKey(wallet, LNParams.chainHash), None)
    }
    makeChannelParams(remoteInfo, finalScript, walletStaticPaymentBasepoint, funder, fundingAmount)
  }

  // We make sure that funder and fundee key path end differently
  def makeChannelParams(remoteInfo: RemoteNodeInfo, defaultFinalScriptPubkey: ByteVector, walletStaticPaymentBasepoint: Option[PublicKey], isFunder: Boolean, fundingAmount: Satoshi): LocalParams =
    makeChannelParams(defaultFinalScriptPubkey, walletStaticPaymentBasepoint, isFunder, fundingAmount, remoteInfo newFundingKeyPath isFunder)

  def makeChannelParams(defaultFinalScriptPubkey: ByteVector, walletStaticPaymentBasepoint: Option[PublicKey], isFunder: Boolean, fundingAmount: Satoshi, fundingKeyPath: DeterministicWallet.KeyPath): LocalParams =
    LocalParams(
      fundingKeyPath,
      dustLimit = LNParams.minDustLimit,
      maxHtlcValueInFlightMsat = UInt64(fundingAmount.toMilliSatoshi.toLong),
      channelReserve = (fundingAmount * LNParams.reserveToFundingRatio).max(LNParams.minDustLimit), // BOLT #2: make sure that our reserve is above our dust limit
      htlcMinimum = LNParams.minPayment,
      toSelfDelay = LNParams.maxToLocalDelay, // we choose their delay
      maxAcceptedHtlcs = LNParams.maxAcceptedHtlcs,
      isFunder = isFunder,
      defaultFinalScriptPubKey = defaultFinalScriptPubkey,
      walletStaticPaymentBasepoint = walletStaticPaymentBasepoint)
}