/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.channel

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import fr.acinq.eclair.wire._
import fr.acinq.bitcoin.Script._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.blockchain.fee._
import fr.acinq.eclair.transactions.Scripts._
import fr.acinq.eclair.transactions.DirectedHtlc._
import fr.acinq.eclair.transactions.Transactions._

import scala.util.{Success, Try}
import immortan.{ChannelBag, LNParams, RemoteNodeInfo}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, ripemd160, sha256}
import fr.acinq.eclair.crypto.Generators
import scodec.bits.ByteVector

/**
 * Created by PM on 20/05/2016.
 */

object Helpers {
  /**
   * Called by the fundee
   */
  def validateParamsFundee(features: Features, open: OpenChannel, conf: OnChainFeeConf): Unit = {
    // BOLT #2: if the chain_hash value, within the open_channel, message is set to a hash of a chain that is unknown to the receiver:
    // MUST reject the channel.
    if (LNParams.chainHash != open.chainHash) throw InvalidChainHash(open.temporaryChannelId, local = LNParams.chainHash, remote = open.chainHash)

    if (open.fundingSatoshis < LNParams.minFundingSatoshis || open.fundingSatoshis > LNParams.maxFundingSatoshis)
      throw InvalidFundingAmount(open.temporaryChannelId, open.fundingSatoshis, LNParams.minFundingSatoshis, LNParams.maxFundingSatoshis)

    // BOLT #2: Channel funding limits
    if (open.fundingSatoshis >= LNParams.maxFundingSatoshis && !features.hasFeature(Features.Wumbo))
      throw InvalidFundingAmount(open.temporaryChannelId, open.fundingSatoshis, LNParams.minFundingSatoshis, LNParams.maxFundingSatoshis)

    // BOLT #2: The receiving node MUST fail the channel if: push_msat is greater than funding_satoshis * 1000.
    if (open.pushMsat > open.fundingSatoshis) throw InvalidPushAmount(open.temporaryChannelId, open.pushMsat, open.fundingSatoshis.toMilliSatoshi)

    // BOLT #2: The receiving node MUST fail the channel if: to_self_delay is unreasonably large.
    if (open.toSelfDelay > LNParams.maxToLocalDelay) throw ToSelfDelayTooHigh(open.temporaryChannelId, open.toSelfDelay, LNParams.maxToLocalDelay)

    // BOLT #2: The receiving node MUST fail the channel if: max_accepted_htlcs is greater than 483.
    if (open.maxAcceptedHtlcs > LNParams.maxAcceptedHtlcs) throw InvalidMaxAcceptedHtlcs(open.temporaryChannelId, open.maxAcceptedHtlcs, LNParams.maxAcceptedHtlcs)

    // BOLT #2: The receiving node MUST fail the channel if: it considers feerate_per_kw too small for timely processing.
    if (isFeeTooSmall(open.feeratePerKw)) throw FeerateTooSmall(open.temporaryChannelId, open.feeratePerKw)

    // BOLT #2: The receiving node MUST fail the channel if: dust_limit_satoshis is greater than channel_reserve_satoshis.
    if (open.dustLimitSatoshis > open.channelReserveSatoshis) throw DustLimitTooLarge(open.temporaryChannelId, open.dustLimitSatoshis, open.channelReserveSatoshis)

    // BOLT #2: The receiving node MUST fail the channel if both to_local and to_remote amounts for the initial commitment
    // transaction are less than or equal to channel_reserve_satoshis (see BOLT 3).
    val (toLocalMsat, toRemoteMsat) = (open.pushMsat, open.fundingSatoshis.toMilliSatoshi - open.pushMsat)
    if (toLocalMsat < open.channelReserveSatoshis && toRemoteMsat < open.channelReserveSatoshis) {
      throw ChannelReserveNotMet(open.temporaryChannelId, toLocalMsat, toRemoteMsat, open.channelReserveSatoshis)
    }

    // BOLT #2: The receiving node MUST fail the channel if: it considers feerate_per_kw too small for timely processing or unreasonably large.
    val localFeeratePerKw = conf.feeEstimator.getFeeratePerKw(target = conf.feeTargets.commitmentBlockTarget)
    if (isFeeDiffTooHigh(localFeeratePerKw, open.feeratePerKw, conf.feerateTolerance)) throw FeerateTooDifferent(open.temporaryChannelId, localFeeratePerKw, open.feeratePerKw)
    // only enforce dust limit check on mainnet
    if (LNParams.chainHash == Block.LivenetGenesisBlock.hash) {
      if (open.dustLimitSatoshis < LNParams.minDustLimit) throw DustLimitTooSmall(open.temporaryChannelId, open.dustLimitSatoshis, LNParams.minDustLimit)
    }

    // we don't check that the funder's amount for the initial commitment transaction is sufficient for full fee payment
    // now, but it will be done later when we receive `funding_created`

    val reserveToFundingRatio = open.channelReserveSatoshis.toLong.toDouble / Math.max(open.fundingSatoshis.toLong, 1)
    if (reserveToFundingRatio > LNParams.maxReserveToFundingRatio) throw ChannelReserveTooHigh(open.temporaryChannelId, open.channelReserveSatoshis, reserveToFundingRatio, LNParams.maxReserveToFundingRatio)

    None
  }

  /**
   * Called by the funder
   */
  def validateParamsFunder(open: OpenChannel, accept: AcceptChannel): Unit = {
    if (accept.maxAcceptedHtlcs > LNParams.maxAcceptedHtlcs) throw InvalidMaxAcceptedHtlcs(accept.temporaryChannelId, accept.maxAcceptedHtlcs, LNParams.maxAcceptedHtlcs)
    // only enforce dust limit check on mainnet
    if (LNParams.chainHash == Block.LivenetGenesisBlock.hash) {
      if (accept.dustLimitSatoshis < LNParams.minDustLimit) throw DustLimitTooSmall(accept.temporaryChannelId, accept.dustLimitSatoshis, LNParams.minDustLimit)
    }

    // BOLT #2: The receiving node MUST fail the channel if: dust_limit_satoshis is greater than channel_reserve_satoshis.
    if (accept.dustLimitSatoshis > accept.channelReserveSatoshis) throw DustLimitTooLarge(accept.temporaryChannelId, accept.dustLimitSatoshis, accept.channelReserveSatoshis)

    // if minimum_depth is unreasonably large:
    // MAY reject the channel.
    if (accept.toSelfDelay > LNParams.maxToLocalDelay) throw ToSelfDelayTooHigh(accept.temporaryChannelId, accept.toSelfDelay, LNParams.maxToLocalDelay)

    // if channel_reserve_satoshis is less than dust_limit_satoshis within the open_channel message:
    //  MUST reject the channel.
    if (accept.channelReserveSatoshis < open.dustLimitSatoshis) throw ChannelReserveBelowOurDustLimit(accept.temporaryChannelId, accept.channelReserveSatoshis, open.dustLimitSatoshis)

    // if channel_reserve_satoshis from the open_channel message is less than dust_limit_satoshis:
    // MUST reject the channel. Other fields have the same requirements as their counterparts in open_channel.
    if (open.channelReserveSatoshis < accept.dustLimitSatoshis) throw DustLimitAboveOurChannelReserve(accept.temporaryChannelId, accept.dustLimitSatoshis, open.channelReserveSatoshis)

    val reserveToFundingRatio = accept.channelReserveSatoshis.toLong.toDouble / Math.max(open.fundingSatoshis.toLong, 1)
    if (reserveToFundingRatio > LNParams.maxReserveToFundingRatio) throw ChannelReserveTooHigh(open.temporaryChannelId, accept.channelReserveSatoshis, reserveToFundingRatio, LNParams.maxReserveToFundingRatio)

    None
  }

  /**
   * To avoid spamming our peers with fee updates every time there's a small variation, we only update the fee when the
   * difference exceeds a given ratio (updateFeeMinDiffRatio).
   */
  def shouldUpdateFee(currentFeeratePerKw: FeeratePerKw, nextFeeratePerKw: FeeratePerKw, updateFeeMinDiffRatio: Double): Boolean =
    currentFeeratePerKw.toLong == 0 || Math.abs((currentFeeratePerKw.toLong - nextFeeratePerKw.toLong).toDouble / currentFeeratePerKw.toLong) > updateFeeMinDiffRatio

  /**
   * @param referenceFeePerKw  reference fee rate per kiloweight
   * @param currentFeePerKw    current fee rate per kiloweight
   * @param maxFeerateMismatch maximum fee rate mismatch tolerated
   * @return true if the difference between proposed and reference fee rates is too high.
   */
  def isFeeDiffTooHigh(referenceFeePerKw: FeeratePerKw, currentFeePerKw: FeeratePerKw, maxFeerateMismatch: FeerateTolerance): Boolean =
    currentFeePerKw < referenceFeePerKw * maxFeerateMismatch.ratioLow || referenceFeePerKw * maxFeerateMismatch.ratioHigh < currentFeePerKw

  /**
   * @param remoteFeeratePerKw remote fee rate per kiloweight
   * @return true if the remote fee rate is too small
   */
  def isFeeTooSmall(remoteFeeratePerKw: FeeratePerKw): Boolean = {
    remoteFeeratePerKw < FeeratePerKw.MinimumFeeratePerKw
  }

  /**
   * This indicates whether our side of the channel is above the reserve requested by our counterparty. In other words,
   * this tells if we can use the channel to make a payment.
   */
  def aboveReserve(commitments: NormalCommits): Boolean = {
    val remoteCommit = commitments.remoteNextCommitInfo match {
      case Left(waitingForRevocation) => waitingForRevocation.nextRemoteCommit
      case _ => commitments.remoteCommit
    }
    val toRemoteSatoshis = remoteCommit.spec.toRemote.truncateToSatoshi
    // NB: this is an approximation (we don't take network fees into account)
    val result = toRemoteSatoshis > commitments.remoteParams.channelReserve
    result
  }

  object Funding {
    def makeFundingInputInfo(fundingTxId: ByteVector32, fundingTxOutputIndex: Int, fundingSatoshis: Satoshi, fundingPubkey1: PublicKey, fundingPubkey2: PublicKey): InputInfo = {
      val fundingScript = multiSig2of2(fundingPubkey1, fundingPubkey2)
      val fundingTxOut = TxOut(fundingSatoshis, pay2wsh(fundingScript))
      InputInfo(OutPoint(fundingTxId, fundingTxOutputIndex), fundingTxOut, write(fundingScript))
    }

    /**
     * Creates both sides' first commitment transaction
     *
     * @return (localSpec, localTx, remoteSpec, remoteTx, fundingTxOutput)
     */
    def makeFirstCommitTxs(remoteInfo: RemoteNodeInfo, channelVersion: ChannelVersion, localParams: LocalParams,
                           remoteParams: RemoteParams, fundingAmount: Satoshi, pushMsat: MilliSatoshi, initialFeeratePerKw: FeeratePerKw, fundingTxHash: ByteVector32,
                           fundingTxOutputIndex: Int, remoteFirstPerCommitmentPoint: PublicKey): (CommitmentSpec, CommitTx, CommitmentSpec, CommitTx) = {

      val toLocalMsat = if (localParams.isFunder) fundingAmount.toMilliSatoshi - pushMsat else pushMsat
      val toRemoteMsat = if (localParams.isFunder) pushMsat else fundingAmount.toMilliSatoshi - pushMsat

      val localSpec = CommitmentSpec(feeratePerKw = initialFeeratePerKw, toLocal = toLocalMsat, toRemote = toRemoteMsat)
      val remoteSpec = CommitmentSpec(feeratePerKw = initialFeeratePerKw, toLocal = toRemoteMsat, toRemote = toLocalMsat)

      if (!localParams.isFunder) {
        val toRemoteMsat = remoteSpec.toLocal
        val fees = commitTxFee(remoteParams.dustLimit, remoteSpec, channelVersion.commitmentFormat)
        val missing = toRemoteMsat.truncateToSatoshi - localParams.channelReserve - fees
        if (missing < 0L.sat) throw new RuntimeException
      }

      val channelKeyPath = remoteInfo.keyPath(localParams)
      val localPerCommitmentPoint = remoteInfo.commitmentPoint(channelKeyPath, 0)
      val fundingPubKey = remoteInfo.fundingPublicKey(localParams.fundingKeyPath)
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex, fundingAmount, fundingPubKey.publicKey, remoteParams.fundingPubKey)
      val (localCommitTx, _, _) = NormalCommits.makeLocalTxs(remoteInfo, channelVersion, 0, localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      val (remoteCommitTx, _, _) = NormalCommits.makeRemoteTxs(remoteInfo, channelVersion, 0, localParams, remoteParams, commitmentInput, remoteFirstPerCommitmentPoint, remoteSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }

  }

  /**
   * Tells whether or not their expected next remote commitment number matches with our data
   *
   * @return
   *         - true if parties are in sync or remote is behind
   *         - false if we are behind
   */
  def checkLocalCommit(d: HasNormalCommitments, nextRemoteRevocationNumber: Long): Boolean = {
    if (d.commitments.localCommit.index == nextRemoteRevocationNumber) {
      // they just sent a new commit_sig, we have received it but they didn't receive our revocation
      true
    } else if (d.commitments.localCommit.index == nextRemoteRevocationNumber + 1) {
      // we are in sync
      true
    } else if (d.commitments.localCommit.index > nextRemoteRevocationNumber + 1) {
      // remote is behind: we return true because things are fine on our side
      true
    } else {
      // we are behind
      false
    }
  }

  /**
   * Tells whether or not their expected next local commitment number matches with our data
   *
   * @return
   *         - true if parties are in sync or remote is behind
   *         - false if we are behind
   */
  def checkRemoteCommit(d: HasNormalCommitments, nextLocalCommitmentNumber: Long): Boolean = {
    d.commitments.remoteNextCommitInfo match {
      case Left(waitingForRevocation) if nextLocalCommitmentNumber == waitingForRevocation.nextRemoteCommit.index =>
        // we just sent a new commit_sig but they didn't receive it
        true
      case Left(waitingForRevocation) if nextLocalCommitmentNumber == (waitingForRevocation.nextRemoteCommit.index + 1) =>
        // we just sent a new commit_sig, they have received it but we haven't received their revocation
        true
      case Left(waitingForRevocation) if nextLocalCommitmentNumber < waitingForRevocation.nextRemoteCommit.index =>
        // they are behind
        true
      case Right(_) if nextLocalCommitmentNumber == (d.commitments.remoteCommit.index + 1) =>
        // they have acknowledged the last commit_sig we sent
        true
      case Right(_) if nextLocalCommitmentNumber < (d.commitments.remoteCommit.index + 1) =>
        // they are behind
        true
      case _ =>
        // we are behind
        false
    }
  }


  object Closing {

    // @formatter:off
    sealed trait ClosingType
    case class MutualClose(tx: Transaction) extends ClosingType
    case class LocalClose(localCommit: LocalCommit, localCommitPublished: LocalCommitPublished) extends ClosingType
    sealed trait RemoteClose extends ClosingType { def remoteCommit: RemoteCommit; def remoteCommitPublished: RemoteCommitPublished }
    case class CurrentRemoteClose(remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished) extends RemoteClose
    case class NextRemoteClose(remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished) extends RemoteClose
    case class RecoveryClose(remoteCommitPublished: RemoteCommitPublished) extends ClosingType
    case class RevokedClose(revokedCommitPublished: RevokedCommitPublished) extends ClosingType
    // @formatter:on

    /**
     * Indicates whether local has anything at stake in this channel
     *
     * @return true if channel was never open, or got closed immediately, had never any htlcs and local never had a positive balance
     */
    def nothingAtStake(data: HasNormalCommitments): Boolean =
      data.commitments.localCommit.index == 0 &&
        data.commitments.localCommit.spec.toLocal == 0.msat &&
        data.commitments.remoteCommit.index == 0 &&
        data.commitments.remoteCommit.spec.toRemote == 0.msat &&
        data.commitments.remoteNextCommitInfo.isRight

    /**
     * As soon as a tx spending the funding tx has reached min_depth, we know what the closing type will be, before
     * the whole closing process finishes (e.g. there may still be delayed or unconfirmed child transactions). It can
     * save us from attempting to publish some transactions.
     *
     * Note that we can't tell for mutual close before it is already final, because only one tx needs to be confirmed.
     *
     * @param closing channel state data
     * @return the channel closing type, if applicable
     */
    def isClosingTypeAlreadyKnown(closing: DATA_CLOSING): Option[ClosingType] = {
      closing match {
        case _ if closing.localCommitPublished.exists(_.isConfirmed) =>
          Some(LocalClose(closing.commitments.localCommit, closing.localCommitPublished.get))
        case _ if closing.remoteCommitPublished.exists(_.isConfirmed) =>
          Some(CurrentRemoteClose(closing.commitments.remoteCommit, closing.remoteCommitPublished.get))
        case _ if closing.nextRemoteCommitPublished.exists(_.isConfirmed) =>
          Some(NextRemoteClose(closing.commitments.remoteNextCommitInfo.left.get.nextRemoteCommit, closing.nextRemoteCommitPublished.get))
        case _ if closing.futureRemoteCommitPublished.exists(_.isConfirmed) =>
          Some(RecoveryClose(closing.futureRemoteCommitPublished.get))
        case _ if closing.revokedCommitPublished.exists(rcp => rcp.irrevocablySpent.values.toSet.contains(rcp.commitTx.txid)) =>
          Some(RevokedClose(closing.revokedCommitPublished.find(rcp => rcp.irrevocablySpent.values.toSet.contains(rcp.commitTx.txid)).get))
        case _ => None // we don't know yet what the closing type will be
      }
    }

    /**
     * Checks if a channel is closed (i.e. its closing tx has been confirmed)
     *
     * @param data                      channel state data
     * @param additionalConfirmedTx_opt additional confirmed transaction; we need this for the mutual close scenario
     *                                  because we don't store the closing tx in the channel state
     * @return the channel closing type, if applicable
     */
    def isClosed(data: HasNormalCommitments, additionalConfirmedTx_opt: Option[Transaction]): Option[ClosingType] = data match {
      case closing: DATA_CLOSING if additionalConfirmedTx_opt.exists(closing.mutualClosePublished.contains) =>
        Some(MutualClose(additionalConfirmedTx_opt.get))
      case closing: DATA_CLOSING if closing.localCommitPublished.exists(Closing.isLocalCommitDone) =>
        Some(LocalClose(closing.commitments.localCommit, closing.localCommitPublished.get))
      case closing: DATA_CLOSING if closing.remoteCommitPublished.exists(Closing.isRemoteCommitDone) =>
        Some(CurrentRemoteClose(closing.commitments.remoteCommit, closing.remoteCommitPublished.get))
      case closing: DATA_CLOSING if closing.nextRemoteCommitPublished.exists(Closing.isRemoteCommitDone) =>
        Some(NextRemoteClose(closing.commitments.remoteNextCommitInfo.left.get.nextRemoteCommit, closing.nextRemoteCommitPublished.get))
      case closing: DATA_CLOSING if closing.futureRemoteCommitPublished.exists(Closing.isRemoteCommitDone) =>
        Some(RecoveryClose(closing.futureRemoteCommitPublished.get))
      case closing: DATA_CLOSING if closing.revokedCommitPublished.exists(Closing.isRevokedCommitDone) =>
        Some(RevokedClose(closing.revokedCommitPublished.find(Closing.isRevokedCommitDone).get))
      case _ => None
    }

    def isValidFinalScriptPubkey(scriptPubKey: ByteVector): Boolean = {
      Try(Script.parse(scriptPubKey)) match {
        case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubkeyHash, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) if pubkeyHash.size == 20 => true
        case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) if scriptHash.size == 20 => true
        case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.size == 20 => true
        case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.size == 32 => true
        case _ => false
      }
    }

    def firstClosingFee(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, feeratePerKw: FeeratePerKw): Satoshi = {
      import commitments._
      // this is just to estimate the weight, it depends on size of the pubkey scripts
      val dummyClosingTx = Transactions.makeClosingTx(commitInput, localScriptPubkey, remoteScriptPubkey, localParams.isFunder, Satoshi(0), Satoshi(0), localCommit.spec)
      val closingWeight = Transaction.weight(Transactions.addSigs(dummyClosingTx, invalidPubKey, remoteParams.fundingPubKey, Transactions.PlaceHolderSig, Transactions.PlaceHolderSig).tx)
      Transactions.weight2fee(feeratePerKw, closingWeight)
    }

    def firstClosingFee(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, feeEstimator: FeeEstimator, feeTargets: FeeTargets): Satoshi = {
      val requestedFeerate = feeEstimator.getFeeratePerKw(feeTargets.mutualCloseBlockTarget)
      // we "MUST set fee_satoshis less than or equal to the base fee of the final commitment transaction"
      val feeratePerKw = requestedFeerate.min(commitments.localCommit.spec.feeratePerKw)
      firstClosingFee(commitments, localScriptPubkey, remoteScriptPubkey, feeratePerKw)
    }

    def nextClosingFee(localClosingFee: Satoshi, remoteClosingFee: Satoshi): Satoshi = ((localClosingFee + remoteClosingFee) / 4) * 2

    def makeFirstClosingTx(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, feeEstimator: FeeEstimator, feeTargets: FeeTargets): (ClosingTx, ClosingSigned) = {
      val closingFee = firstClosingFee(commitments, localScriptPubkey, remoteScriptPubkey, feeEstimator, feeTargets)
      makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, closingFee)
    }

    def makeClosingTx(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, closingFee: Satoshi): (ClosingTx, ClosingSigned) = {
      import commitments._
      require(isValidFinalScriptPubkey(localScriptPubkey), "invalid localScriptPubkey")
      require(isValidFinalScriptPubkey(remoteScriptPubkey), "invalid remoteScriptPubkey")
      val dustLimitSatoshis = localParams.dustLimit.max(remoteParams.dustLimit)
      val closingTx = Transactions.makeClosingTx(commitInput, localScriptPubkey, remoteScriptPubkey, localParams.isFunder, dustLimitSatoshis, closingFee, localCommit.spec)
      val localClosingSig = commitments.remoteInfo.sign(closingTx, commitments.remoteInfo.fundingPublicKey(commitments.localParams.fundingKeyPath), TxOwner.Local, channelVersion.commitmentFormat)
      val closingSigned = ClosingSigned(channelId, closingFee, localClosingSig)
      (closingTx, closingSigned)
    }

    def checkClosingSignature(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, remoteClosingFee: Satoshi, remoteClosingSig: ByteVector64): Transaction = {
      import commitments._
      val lastCommitFeeSatoshi = commitments.commitInput.txOut.amount - commitments.localCommit.publishableTxs.commitTx.tx.txOut.map(_.amount).sum
      if (remoteClosingFee > lastCommitFeeSatoshi) {
        throw new RuntimeException
      } else {
        val (closingTx, closingSigned) = makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, remoteClosingFee)
        val signedClosingTx = Transactions.addSigs(closingTx, commitments.remoteInfo.fundingPublicKey(commitments.localParams.fundingKeyPath).publicKey, remoteParams.fundingPubKey, closingSigned.signature, remoteClosingSig)
        Transactions.checkSpendable(signedClosingTx) match {
          case Success(_) => signedClosingTx.tx
          case _ => throw new RuntimeException
        }
      }
    }

    /** Wraps transaction generation in a Try and filters failures to avoid one transaction negatively impacting a whole commitment. */
    private def generateTx(desc: String)(attempt: => Either[TxGenerationSkipped, TransactionWithInputInfo]): Option[TransactionWithInputInfo] =
      Try(attempt) map { case Right(txinfo) => Some(txinfo) case _ => None } getOrElse None

    /**
     * Claim all the HTLCs that we've received from our current commit tx. This will be
     * done using 2nd stage HTLC transactions
     *
     * @param commitments our commitment data, which include payment preimages
     * @return a list of transactions (one per HTLC that we can claim)
     */
    def claimCurrentLocalCommitTxOutputs(commitments: NormalCommits, tx: Transaction, feeEstimator: FeeEstimator, feeTargets: FeeTargets): LocalCommitPublished = {
      import commitments._
      require(localCommit.publishableTxs.commitTx.tx.txid == tx.txid, "txid mismatch, provided tx is not the current local commit tx")
      val localPerCommitmentPoint = commitments.remoteInfo.commitmentPoint(channelKeyPath, commitments.localCommit.index.toInt)
      val localRevocationPubkey = Generators.revocationPubKey(remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayedPubkey = Generators.derivePubKey(commitments.remoteInfo.delayedPaymentPoint(channelKeyPath).publicKey, localPerCommitmentPoint)
      val feeratePerKwDelayed = feeEstimator.getFeeratePerKw(feeTargets.claimMainBlockTarget)

      // first we will claim our main output as soon as the delay is over
      val mainDelayedTx = generateTx("main-delayed-output") {
        Transactions.makeClaimLocalDelayedOutputTx(tx, localParams.dustLimit, localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPubkey, localParams.defaultFinalScriptPubKey, feeratePerKwDelayed).right.map(claimDelayed => {
          val sig = commitments.remoteInfo.sign(claimDelayed, commitments.remoteInfo.delayedPaymentPoint(channelKeyPath), localPerCommitmentPoint, TxOwner.Local, channelVersion.commitmentFormat)
          Transactions.addSigs(claimDelayed, sig)
        })
      }

      // those are the preimages to existing received htlcs
      val preimages = commitments.localChanges.all.collect { case u: UpdateFulfillHtlc => u.paymentHash -> u.paymentPreimage }.toMap

      val htlcTxes = localCommit.publishableTxs.htlcTxsAndSigs.collect {
        // incoming htlc for which we have the preimage: we spend it directly
        case HtlcTxAndSigs(txinfo@HtlcSuccessTx(_, _, paymentHash), localSig, remoteSig) if preimages.contains(paymentHash) =>
          generateTx("htlc-success") {
            Right(Transactions.addSigs(txinfo, localSig, remoteSig, preimages(paymentHash), channelVersion.commitmentFormat))
          }

        // outgoing htlc: they may or may not have the preimage, the only thing to do is try to get back our funds after timeout
        case HtlcTxAndSigs(txinfo: HtlcTimeoutTx, localSig, remoteSig) =>
          generateTx("htlc-timeout") {
            Right(Transactions.addSigs(txinfo, localSig, remoteSig, channelVersion.commitmentFormat))
          }
      }.flatten

      // all htlc output to us are delayed, so we need to claim them as soon as the delay is over
      val htlcDelayedTxes = htlcTxes.flatMap {
        txinfo: TransactionWithInputInfo =>
          generateTx("claim-htlc-delayed") {
            Transactions.makeClaimLocalDelayedOutputTx(txinfo.tx, localParams.dustLimit, localRevocationPubkey, remoteParams.toSelfDelay, localDelayedPubkey, localParams.defaultFinalScriptPubKey, feeratePerKwDelayed).right.map(claimDelayed => {
              val sig = commitments.remoteInfo.sign(claimDelayed, commitments.remoteInfo.delayedPaymentPoint(channelKeyPath), localPerCommitmentPoint, TxOwner.Local, channelVersion.commitmentFormat)
              Transactions.addSigs(claimDelayed, sig)
            })
          }
      }

      LocalCommitPublished(
        commitTx = tx,
        claimMainDelayedOutputTx = mainDelayedTx.map(_.tx),
        htlcSuccessTxs = htlcTxes.collect { case c: HtlcSuccessTx => c.tx },
        htlcTimeoutTxs = htlcTxes.collect { case c: HtlcTimeoutTx => c.tx },
        claimHtlcDelayedTxs = htlcDelayedTxes.map(_.tx),
        irrevocablySpent = Map.empty)
    }

    /**
     * Claim all the HTLCs that we've received from their current commit tx, if the channel used option_static_remotekey
     * we don't need to claim our main output because it directly pays to one of our wallet's p2wpkh addresses.
     *
     * @param commitments  our commitment data, which include payment preimages
     * @param remoteCommit the remote commitment data to use to claim outputs (it can be their current or next commitment)
     * @param tx           the remote commitment transaction that has just been published
     * @return a list of transactions (one per HTLC that we can claim)
     */
    def claimRemoteCommitTxOutputs(commitments: NormalCommits, remoteCommit: RemoteCommit, tx: Transaction, feeEstimator: FeeEstimator, feeTargets: FeeTargets): RemoteCommitPublished = {
      import commitments.{channelVersion, commitInput, localParams, remoteParams, channelKeyPath}
      require(remoteCommit.txid == tx.txid, "txid mismatch, provided tx is not the current remote commit tx")
      val (remoteCommitTx, _, _) = NormalCommits.makeRemoteTxs(commitments.remoteInfo, channelVersion, remoteCommit.index, localParams, remoteParams, commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)
      require(remoteCommitTx.tx.txid == tx.txid, "txid mismatch, cannot recompute the current remote commit tx")
      val localFundingPubkey = commitments.remoteInfo.fundingPublicKey(localParams.fundingKeyPath).publicKey
      val localHtlcPubkey = Generators.derivePubKey(commitments.remoteInfo.htlcPoint(channelKeyPath).publicKey, remoteCommit.remotePerCommitmentPoint)
      val remoteHtlcPubkey = Generators.derivePubKey(remoteParams.htlcBasepoint, remoteCommit.remotePerCommitmentPoint)
      val remoteRevocationPubkey = Generators.revocationPubKey(commitments.remoteInfo.revocationPoint(channelKeyPath).publicKey, remoteCommit.remotePerCommitmentPoint)
      val remoteDelayedPaymentPubkey = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remoteCommit.remotePerCommitmentPoint)
      val localPaymentPubkey = Generators.derivePubKey(commitments.remoteInfo.paymentPoint(channelKeyPath).publicKey, remoteCommit.remotePerCommitmentPoint)
      val outputs = makeCommitTxOutputs(!localParams.isFunder, remoteParams.dustLimit, remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPaymentPubkey, localPaymentPubkey, remoteHtlcPubkey, localHtlcPubkey, remoteParams.fundingPubKey, localFundingPubkey, remoteCommit.spec, commitments.channelVersion.commitmentFormat)

      // we need to use a rather high fee for htlc-claim because we compete with the counterparty
      val feeratePerKwHtlc = feeEstimator.getFeeratePerKw(target = 2)

      // those are the preimages to existing received htlcs
      val preimages = commitments.localChanges.all.collect { case u: UpdateFulfillHtlc => u.paymentPreimage }

      // remember we are looking at the remote commitment so IN for them is really OUT for us and vice versa
      val txes = remoteCommit.spec.htlcs.collect {
        // incoming htlc for which we have the preimage: we spend it directly.
        // NB: we are looking at the remote's commitment, from its point of view it's an outgoing htlc.
        case OutgoingHtlc(add: UpdateAddHtlc) if preimages.exists(r => sha256(r) == add.paymentHash) => generateTx("claim-htlc-success") {
          val preimage = preimages.find(r => sha256(r) == add.paymentHash).get
          Transactions.makeClaimHtlcSuccessTx(remoteCommitTx.tx, outputs, localParams.dustLimit, localHtlcPubkey, remoteHtlcPubkey, remoteRevocationPubkey, localParams.defaultFinalScriptPubKey, add, feeratePerKwHtlc, commitments.channelVersion.commitmentFormat).right.map(txinfo => {
            val sig = commitments.remoteInfo.sign(txinfo, commitments.remoteInfo.htlcPoint(channelKeyPath), remoteCommit.remotePerCommitmentPoint, TxOwner.Local, commitments.channelVersion.commitmentFormat)
            Transactions.addSigs(txinfo, sig, preimage)
          })
        }

        // (incoming htlc for which we don't have the preimage: nothing to do, it will timeout eventually and they will get their funds back)

        // outgoing htlc: they may or may not have the preimage, the only thing to do is try to get back our funds after timeout
        case IncomingHtlc(add: UpdateAddHtlc) => generateTx("claim-htlc-timeout") {
          Transactions.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, outputs, localParams.dustLimit, localHtlcPubkey, remoteHtlcPubkey, remoteRevocationPubkey, localParams.defaultFinalScriptPubKey, add, feeratePerKwHtlc, commitments.channelVersion.commitmentFormat).right.map(txinfo => {
            val sig = commitments.remoteInfo.sign(txinfo, commitments.remoteInfo.htlcPoint(channelKeyPath), remoteCommit.remotePerCommitmentPoint, TxOwner.Local, commitments.channelVersion.commitmentFormat)
            Transactions.addSigs(txinfo, sig)
          })
        }
      }.toSeq.flatten

      channelVersion match {
        case v if v.paysDirectlyToWallet =>
          RemoteCommitPublished(
            commitTx = tx,
            claimMainOutputTx = None,
            claimHtlcSuccessTxs = txes.toList.collect { case c: ClaimHtlcSuccessTx => c.tx },
            claimHtlcTimeoutTxs = txes.toList.collect { case c: ClaimHtlcTimeoutTx => c.tx },
            irrevocablySpent = Map.empty
          )
        case _ =>
          claimRemoteCommitMainOutput(commitments, remoteCommit.remotePerCommitmentPoint, tx, feeEstimator, feeTargets).copy(
            claimHtlcSuccessTxs = txes.toList.collect { case c: ClaimHtlcSuccessTx => c.tx },
            claimHtlcTimeoutTxs = txes.toList.collect { case c: ClaimHtlcTimeoutTx => c.tx }
          )
      }
    }

    /**
     * Claim our Main output only, not used if option_static_remotekey was negotiated
     *
     * @param commitments              either our current commitment data in case of usual remote uncooperative closing
     *                                 or our outdated commitment data in case of data loss protection procedure; in any case it is used only
     *                                 to get some constant parameters, not commitment data
     * @param remotePerCommitmentPoint the remote perCommitmentPoint corresponding to this commitment
     * @param tx                       the remote commitment transaction that has just been published
     * @return a list of transactions (one per HTLC that we can claim)
     */
    def claimRemoteCommitMainOutput(commitments: NormalCommits, remotePerCommitmentPoint: PublicKey, tx: Transaction, feeEstimator: FeeEstimator, feeTargets: FeeTargets): RemoteCommitPublished = {
      val localPubkey = Generators.derivePubKey(commitments.remoteInfo.paymentPoint(commitments.channelKeyPath).publicKey, remotePerCommitmentPoint)
      val localPaymentPoint = commitments.remoteInfo.paymentPoint(commitments.channelKeyPath).publicKey
      val feeratePerKwMain = feeEstimator.getFeeratePerKw(feeTargets.claimMainBlockTarget)

      val mainTx = commitments.channelVersion.commitmentFormat match {
        case DefaultCommitmentFormat => generateTx("claim-p2wpkh-output") {
          Transactions.makeClaimP2WPKHOutputTx(tx, commitments.localParams.dustLimit, localPubkey, commitments.localParams.defaultFinalScriptPubKey, feeratePerKwMain).right.map(claimMain => {
            val sig = commitments.remoteInfo.sign(claimMain, commitments.remoteInfo.paymentPoint(commitments.channelKeyPath), remotePerCommitmentPoint, TxOwner.Local, commitments.channelVersion.commitmentFormat)
            Transactions.addSigs(claimMain, localPubkey, sig)
          })
        }
        case AnchorOutputsCommitmentFormat => generateTx("claim-remote-delayed-output") {
          Transactions.makeClaimRemoteDelayedOutputTx(tx, commitments.localParams.dustLimit, localPaymentPoint, commitments.localParams.defaultFinalScriptPubKey, feeratePerKwMain).right.map(claimMain => {
            val sig = commitments.remoteInfo.sign(claimMain, commitments.remoteInfo.paymentPoint(commitments.channelKeyPath), TxOwner.Local, commitments.channelVersion.commitmentFormat)
            Transactions.addSigs(claimMain, sig)
          })
        }
      }

      RemoteCommitPublished(
        commitTx = tx,
        claimMainOutputTx = mainTx.map(_.tx),
        claimHtlcSuccessTxs = Nil,
        claimHtlcTimeoutTxs = Nil,
        irrevocablySpent = Map.empty
      )
    }

    /**
     * When an unexpected transaction spending the funding tx is detected:
     * 1) we find out if the published transaction is one of remote's revoked txs
     * 2) and then:
     * a) if it is a revoked tx we build a set of transactions that will punish them by stealing all their funds
     * b) otherwise there is nothing we can do
     *
     * @return a [[RevokedCommitPublished]] object containing penalty transactions if the tx is a revoked commitment
     */
    def claimRevokedRemoteCommitTxOutputs(commitments: NormalCommits, commitTx: Transaction, db: ChannelBag, feeEstimator: FeeEstimator, feeTargets: FeeTargets): Option[RevokedCommitPublished] = {
      require(commitTx.txIn.size == 1, "commitment tx should have 1 input")
      val channelKeyPath = commitments.remoteInfo.keyPath(commitments.localParams)
      val obscuredTxNumber = Transactions.decodeTxNumber(commitTx.txIn.head.sequence, commitTx.lockTime)
      // this tx has been published by remote, so we need to invert local/remote params
      val txnumber = Transactions.obscuredCommitTxNumber(obscuredTxNumber, !commitments.localParams.isFunder, commitments.remoteParams.paymentBasepoint, commitments.localParams.walletStaticPaymentBasepoint)
      require(txnumber <= 0xffffffffffffL, "txnumber must be lesser than 48 bits long")
      // now we know what commit number this tx is referring to, we can derive the commitment point from the shachain
      commitments.remotePerCommitmentSecrets.getHash(0xFFFFFFFFFFFFL - txnumber)
        .map(d => PrivateKey(d))
        .map { remotePerCommitmentSecret =>
          val remotePerCommitmentPoint = remotePerCommitmentSecret.publicKey
          val remoteDelayedPaymentPubkey = Generators.derivePubKey(commitments.remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
          val remoteRevocationPubkey = Generators.revocationPubKey(commitments.remoteInfo.revocationPoint(channelKeyPath).publicKey, remotePerCommitmentPoint)
          val remoteHtlcPubkey = Generators.derivePubKey(commitments.remoteParams.htlcBasepoint, remotePerCommitmentPoint)
          val localPaymentPubkey = Generators.derivePubKey(commitments.remoteInfo.paymentPoint(channelKeyPath).publicKey, remotePerCommitmentPoint)
          val localHtlcPubkey = Generators.derivePubKey(commitments.remoteInfo.htlcPoint(channelKeyPath).publicKey, remotePerCommitmentPoint)

          val feeratePerKwMain = feeEstimator.getFeeratePerKw(feeTargets.claimMainBlockTarget)
          // we need to use a high fee here for punishment txes because after a delay they can be spent by the counterparty
          val feeratePerKwPenalty = feeEstimator.getFeeratePerKw(target = 2)

          // first we will claim our main output right away
          val mainTx = commitments.channelVersion match {
            case v if v.paysDirectlyToWallet =>
              None
            case v if v.hasAnchorOutputs => generateTx("claim-remote-delayed-output") {
              Transactions.makeClaimRemoteDelayedOutputTx(commitTx, commitments.localParams.dustLimit, commitments.localParams.walletStaticPaymentBasepoint, commitments.localParams.defaultFinalScriptPubKey, feeratePerKwMain).right.map(claimMain => {
                val sig = commitments.remoteInfo.sign(claimMain, commitments.remoteInfo.paymentPoint(channelKeyPath), TxOwner.Local, commitments.channelVersion.commitmentFormat)
                Transactions.addSigs(claimMain, sig)
              })
            }
            case _ => generateTx("claim-p2wpkh-output") {
              Transactions.makeClaimP2WPKHOutputTx(commitTx, commitments.localParams.dustLimit, localPaymentPubkey, commitments.localParams.defaultFinalScriptPubKey, feeratePerKwMain).right.map(claimMain => {
                val sig = commitments.remoteInfo.sign(claimMain, commitments.remoteInfo.paymentPoint(channelKeyPath), remotePerCommitmentPoint, TxOwner.Local, commitments.channelVersion.commitmentFormat)
                Transactions.addSigs(claimMain, localPaymentPubkey, sig)
              })
            }
          }

          // then we punish them by stealing their main output
          val mainPenaltyTx = generateTx("main-penalty") {
            Transactions.makeMainPenaltyTx(commitTx, commitments.localParams.dustLimit, remoteRevocationPubkey, commitments.localParams.defaultFinalScriptPubKey, commitments.localParams.toSelfDelay, remoteDelayedPaymentPubkey, feeratePerKwPenalty).right.map(txinfo => {
              val sig = commitments.remoteInfo.sign(txinfo, commitments.remoteInfo.revocationPoint(channelKeyPath), remotePerCommitmentSecret, TxOwner.Local, commitments.channelVersion.commitmentFormat)
              Transactions.addSigs(txinfo, sig)
            })
          }

          // we retrieve the informations needed to rebuild htlc scripts
          val htlcInfos = db.htlcInfos(txnumber)
          val htlcsRedeemScripts = (
            htlcInfos.map(item => Scripts.htlcReceived(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, item.hash160, item.cltvExpiry, commitments.channelVersion.commitmentFormat)) ++
              htlcInfos.map(item => Scripts.htlcOffered(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, item.hash160, commitments.channelVersion.commitmentFormat))
            )
            .map(redeemScript => Script.write(pay2wsh(redeemScript)) -> Script.write(redeemScript))
            .toMap

          // and finally we steal the htlc outputs
          val htlcPenaltyTxs = commitTx.txOut.zipWithIndex.collect { case (txOut, outputIndex) if htlcsRedeemScripts.contains(txOut.publicKeyScript) =>
            val htlcRedeemScript = htlcsRedeemScripts(txOut.publicKeyScript)
            generateTx("htlc-penalty") {
              Transactions.makeHtlcPenaltyTx(commitTx, outputIndex, htlcRedeemScript, commitments.localParams.dustLimit, commitments.localParams.defaultFinalScriptPubKey, feeratePerKwPenalty).right.map(htlcPenalty => {
                val sig = commitments.remoteInfo.sign(htlcPenalty, commitments.remoteInfo.revocationPoint(channelKeyPath), remotePerCommitmentSecret, TxOwner.Local, commitments.channelVersion.commitmentFormat)
                Transactions.addSigs(htlcPenalty, sig, remoteRevocationPubkey)
              })
            }
          }.toList.flatten

          RevokedCommitPublished(
            commitTx = commitTx,
            claimMainOutputTx = mainTx.map(_.tx),
            mainPenaltyTx = mainPenaltyTx.map(_.tx),
            htlcPenaltyTxs = htlcPenaltyTxs.map(_.tx),
            claimHtlcDelayedPenaltyTxs = Nil, // we will generate and spend those if they publish their HtlcSuccessTx or HtlcTimeoutTx
            irrevocablySpent = Map.empty
          )
        }
    }

    /**
     * Claims the output of an [[HtlcSuccessTx]] or [[HtlcTimeoutTx]] transaction using a revocation key.
     *
     * In case a revoked commitment with pending HTLCs is published, there are two ways the HTLC outputs can be taken as punishment:
     * - by spending the corresponding output of the commitment tx, using [[HtlcPenaltyTx]] that we generate as soon as we detect that a revoked commit
     * as been spent; note that those transactions will compete with [[HtlcSuccessTx]] and [[HtlcTimeoutTx]] published by the counterparty.
     * - by spending the delayed output of [[HtlcSuccessTx]] and [[HtlcTimeoutTx]] if those get confirmed; because the output of these txes is protected by
     * an OP_CSV delay, we will have time to spend them with a revocation key. In that case, we generate the spending transactions "on demand",
     * this is the purpose of this method.
     */
    def claimRevokedHtlcTxOutputs(commitments: NormalCommits, revokedCommitPublished: RevokedCommitPublished, htlcTx: Transaction, feeEstimator: FeeEstimator): (RevokedCommitPublished, Option[Transaction]) = {
      if (htlcTx.txIn.map(_.outPoint.txid).contains(revokedCommitPublished.commitTx.txid) &&
        !(revokedCommitPublished.claimMainOutputTx ++ revokedCommitPublished.mainPenaltyTx ++ revokedCommitPublished.htlcPenaltyTxs).map(_.txid).toSet.contains(htlcTx.txid)) {
        // Let's assume that htlcTx is an HtlcSuccessTx or HtlcTimeoutTx and try to generate a tx spending its output using a revocation key
        import commitments._
        val tx = revokedCommitPublished.commitTx
        val obscuredTxNumber = Transactions.decodeTxNumber(tx.txIn.head.sequence, tx.lockTime)
        // this tx has been published by remote, so we need to invert local/remote params
        val txnumber = Transactions.obscuredCommitTxNumber(obscuredTxNumber, !localParams.isFunder, remoteParams.paymentBasepoint, localParams.walletStaticPaymentBasepoint)
        // now we know what commit number this tx is referring to, we can derive the commitment point from the shachain
        remotePerCommitmentSecrets.getHash(0xFFFFFFFFFFFFL - txnumber)
          .map(d => PrivateKey(d))
          .flatMap { remotePerCommitmentSecret =>
            val remotePerCommitmentPoint = remotePerCommitmentSecret.publicKey
            val remoteDelayedPaymentPubkey = Generators.derivePubKey(remoteParams.delayedPaymentBasepoint, remotePerCommitmentPoint)
            val remoteRevocationPubkey = Generators.revocationPubKey(commitments.remoteInfo.revocationPoint(channelKeyPath).publicKey, remotePerCommitmentPoint)

            // we need to use a high fee here for punishment txes because after a delay they can be spent by the counterparty
            val feeratePerKwPenalty = feeEstimator.getFeeratePerKw(target = 1)

            generateTx("claim-htlc-delayed-penalty") {
              Transactions.makeClaimHtlcDelayedOutputPenaltyTx(htlcTx, localParams.dustLimit, remoteRevocationPubkey, localParams.toSelfDelay, remoteDelayedPaymentPubkey, localParams.defaultFinalScriptPubKey, feeratePerKwPenalty).right.map(htlcDelayedPenalty => {
                val sig = commitments.remoteInfo.sign(htlcDelayedPenalty, commitments.remoteInfo.revocationPoint(channelKeyPath), remotePerCommitmentSecret, TxOwner.Local, channelVersion.commitmentFormat)
                val signedTx = Transactions.addSigs(htlcDelayedPenalty, sig)
                // we need to make sure that the tx is indeed valid
                Transaction.correctlySpends(signedTx.tx, Seq(htlcTx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
                signedTx
              })
            }
          } match {
          case Some(penaltyTx) =>
            val revokedCommitPublished1 = revokedCommitPublished.copy(claimHtlcDelayedPenaltyTxs = revokedCommitPublished.claimHtlcDelayedPenaltyTxs :+ penaltyTx.tx)
            (revokedCommitPublished1, Some(penaltyTx.tx))
          case None =>
            (revokedCommitPublished, None)
        }
      } else {
        (revokedCommitPublished, None)
      }
    }

    /**
     * In CLOSING state, any time we see a new transaction, we try to extract a preimage from it in order to fulfill the
     * corresponding incoming htlc in an upstream channel.
     *
     * Not doing that would result in us losing money, because the downstream node would pull money from one side, and
     * the upstream node would get refunded after a timeout.
     *
     * @return a set of pairs (add, preimage) if extraction was successful:
     *           - add is the htlc in the downstream channel from which we extracted the preimage
     *           - preimage needs to be sent to the upstream channel
     */
    def extractPreimages(localCommit: LocalCommit, tx: Transaction): Set[(UpdateAddHtlc, ByteVector32)] = {
      val htlcSuccess = tx.txIn.map(_.witness).collect(Scripts.extractPreimageFromHtlcSuccess)
      val claimHtlcSuccess = tx.txIn.map(_.witness).collect(Scripts.extractPreimageFromClaimHtlcSuccess)
      val paymentPreimages = (htlcSuccess ++ claimHtlcSuccess).toSet
      paymentPreimages.flatMap { paymentPreimage =>
        // we only consider htlcs in our local commitment, because we only care about outgoing htlcs, which disappear first in the remote commitment
        // if an outgoing htlc is in the remote commitment, then:
        // - either it is in the local commitment (it was never fulfilled)
        // - or we have already received the fulfill and forwarded it upstream
        localCommit.spec.htlcs.collect {
          case OutgoingHtlc(add) if add.paymentHash == sha256(paymentPreimage) => (add, paymentPreimage)
        }
      }
    }

    /**
     * We may have multiple HTLCs with the same payment hash because of MPP.
     * When a timeout transaction is confirmed, we need to find the best matching HTLC to fail upstream.
     * We need to handle potentially duplicate HTLCs (same amount and expiry): this function will use a deterministic
     * ordering of transactions and HTLCs to handle this.
     */
    private def findTimedOutHtlc(tx: Transaction, paymentHash160: ByteVector, htlcs: Seq[UpdateAddHtlc], timeoutTxs: Seq[Transaction], extractPaymentHash: PartialFunction[ScriptWitness, ByteVector]): Option[UpdateAddHtlc] = {
      // We use a deterministic ordering to match HTLCs to their corresponding HTLC-timeout tx.
      // We don't match on the expected amounts because this is error-prone: computing the correct weight of a claim-htlc-timeout
      // is hard because signatures can be either 71, 72 or 73 bytes long (ECDSA DER encoding).
      // We could instead look at the spent outpoint, but that requires more lookups and access to the published commitment transaction.
      // It's simpler to just use the amount as the first ordering key: since the feerate is the same for all timeout
      // transactions we will find the right HTLC to fail upstream.
      val matchingHtlcs = htlcs
        .filter(add => add.cltvExpiry.toLong == tx.lockTime && ripemd160(add.paymentHash) == paymentHash160)
        .sortBy(add => (add.amountMsat.toLong, add.id))
      val matchingTxs = timeoutTxs
        .filter(timeoutTx => timeoutTx.lockTime == tx.lockTime && timeoutTx.txIn.map(_.witness).collect(extractPaymentHash).contains(paymentHash160))
        .sortBy(timeoutTx => (timeoutTx.txOut.map(_.amount.toLong).sum, timeoutTx.txid.toHex))
      matchingHtlcs.zip(matchingTxs).collectFirst {
        case (add, timeoutTx) if timeoutTx.txid == tx.txid => add
      }
    }

    /**
     * In CLOSING state, when we are notified that a transaction has been confirmed, we analyze it to find out if one or
     * more htlcs have timed out and need to be failed in an upstream channel.
     *
     * @param tx a tx that has reached mindepth
     * @return a set of htlcs that need to be failed upstream
     */
    def timedoutHtlcs(commitmentFormat: CommitmentFormat, localCommit: LocalCommit, localCommitPublished: LocalCommitPublished, localDustLimit: Satoshi, tx: Transaction): Set[UpdateAddHtlc] = {
      val untrimmedHtlcs = Transactions.trimOfferedHtlcs(localDustLimit, localCommit.spec, commitmentFormat).map(_.add)
      if (tx.txid == localCommit.publishableTxs.commitTx.tx.txid) {
        // the tx is a commitment tx, we can immediately fail all dust htlcs (they don't have an output in the tx)
        localCommit.spec.htlcs.collect(outgoing) -- untrimmedHtlcs
      } else {
        // maybe this is a timeout tx, in that case we can resolve and fail the corresponding htlc
        tx.txIn
          .map(_.witness)
          .collect(Scripts.extractPaymentHashFromHtlcTimeout)
          .flatMap { paymentHash160 =>
            findTimedOutHtlc(tx,
              paymentHash160,
              untrimmedHtlcs,
              localCommitPublished.htlcTimeoutTxs,
              Scripts.extractPaymentHashFromHtlcTimeout)
          }
          .toSet
      }
    }

    /**
     * In CLOSING state, when we are notified that a transaction has been confirmed, we analyze it to find out if one or
     * more htlcs have timed out and need to be failed in an upstream channel.
     *
     * @param tx a tx that has reached mindepth
     * @return a set of htlcs that need to be failed upstream
     */
    def timedoutHtlcs(commitmentFormat: CommitmentFormat, remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished, remoteDustLimit: Satoshi, tx: Transaction): Set[UpdateAddHtlc] = {
      val untrimmedHtlcs = Transactions.trimReceivedHtlcs(remoteDustLimit, remoteCommit.spec, commitmentFormat).map(_.add)
      if (tx.txid == remoteCommit.txid) {
        // the tx is a commitment tx, we can immediately fail all dust htlcs (they don't have an output in the tx)
        remoteCommit.spec.htlcs.collect(incoming) -- untrimmedHtlcs
      } else {
        // maybe this is a timeout tx, in that case we can resolve and fail the corresponding htlc
        tx.txIn
          .map(_.witness)
          .collect(Scripts.extractPaymentHashFromClaimHtlcTimeout)
          .flatMap { paymentHash160 =>
            findTimedOutHtlc(tx,
              paymentHash160,
              untrimmedHtlcs,
              remoteCommitPublished.claimHtlcTimeoutTxs,
              Scripts.extractPaymentHashFromClaimHtlcTimeout)
          }.toSet
      }
    }

    /**
     * As soon as a local or remote commitment reaches min_depth, we know which htlcs will be settled on-chain (whether
     * or not they actually have an output in the commitment tx).
     *
     * @param tx a transaction that is sufficiently buried in the blockchain
     */
    def onChainOutgoingHtlcs(localCommit: LocalCommit, remoteCommit: RemoteCommit, nextRemoteCommit_opt: Option[RemoteCommit], tx: Transaction): Set[UpdateAddHtlc] = {
      if (localCommit.publishableTxs.commitTx.tx.txid == tx.txid) {
        localCommit.spec.htlcs.collect(outgoing)
      } else if (remoteCommit.txid == tx.txid) {
        remoteCommit.spec.htlcs.collect(incoming)
      } else if (nextRemoteCommit_opt.map(_.txid).contains(tx.txid)) {
        nextRemoteCommit_opt.get.spec.htlcs.collect(incoming)
      } else {
        Set.empty
      }
    }

    /**
     * If a commitment tx reaches min_depth, we need to fail the outgoing htlcs that will never reach the blockchain.
     * It could be because only us had signed them, or because a revoked commitment got confirmed.
     */
    def overriddenOutgoingHtlcs(d: DATA_CLOSING, tx: Transaction): Set[UpdateAddHtlc] = {
      val localCommit = d.commitments.localCommit
      val remoteCommit = d.commitments.remoteCommit
      val nextRemoteCommit_opt = d.commitments.remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit)
      if (localCommit.publishableTxs.commitTx.tx.txid == tx.txid) {
        // our commit got confirmed, so any htlc that is in their commitment but not in ours will never reach the chain
        val htlcsInRemoteCommit = remoteCommit.spec.htlcs ++ nextRemoteCommit_opt.map(_.spec.htlcs).getOrElse(Set.empty)
        // NB: from the p.o.v of remote, their incoming htlcs are our outgoing htlcs
        htlcsInRemoteCommit.collect(incoming) -- localCommit.spec.htlcs.collect(outgoing)
      } else if (remoteCommit.txid == tx.txid) {
        // their commit got confirmed
        nextRemoteCommit_opt match {
          case Some(nextRemoteCommit) =>
            // we had signed a new commitment but they committed the previous one
            // any htlc that we signed in the new commitment that they didn't sign will never reach the chain
            nextRemoteCommit.spec.htlcs.collect(incoming) -- localCommit.spec.htlcs.collect(outgoing)
          case None =>
            // their last commitment got confirmed, so no htlcs will be overridden, they will timeout or be fulfilled on chain
            Set.empty
        }
      } else if (nextRemoteCommit_opt.map(_.txid).contains(tx.txid)) {
        // their last commitment got confirmed, so no htlcs will be overridden, they will timeout or be fulfilled on chain
        Set.empty
      } else if (d.revokedCommitPublished.map(_.commitTx.txid).contains(tx.txid)) {
        // a revoked commitment got confirmed: we will claim its outputs, but we also need to fail htlcs that are pending in the latest commitment:
        //  - outgoing htlcs that are in the local commitment but not in remote/nextRemote have already been fulfilled/failed so we don't care about them
        //  - outgoing htlcs that are in the remote/nextRemote commitment may not really be overridden, but since we are going to claim their output as a
        //    punishment we will never get the preimage and may as well consider them failed in the context of relaying htlcs
        nextRemoteCommit_opt.getOrElse(remoteCommit).spec.htlcs.collect(incoming)
      } else {
        Set.empty
      }
    }

    /**
     * In CLOSING state, when we are notified that a transaction has been confirmed, we check if this tx belongs in the
     * local commit scenario and keep track of it.
     *
     * We need to keep track of all transactions spending the outputs of the commitment tx, because some outputs can be
     * spent both by us and our counterparty. Because of that, some of our transactions may never confirm and we don't
     * want to wait forever before declaring that the channel is CLOSED.
     *
     * @param tx a transaction that has been irrevocably confirmed
     */
    def updateLocalCommitPublished(localCommitPublished: LocalCommitPublished, tx: Transaction): LocalCommitPublished = {
      // even if our txes only have one input, maybe our counterparty uses a different scheme so we need to iterate
      // over all of them to check if they are relevant
      val relevantOutpoints = tx.txIn.map(_.outPoint).filter { outPoint =>
        // is this the commit tx itself ? (we could do this outside of the loop...)
        val isCommitTx = localCommitPublished.commitTx.txid == tx.txid
        // does the tx spend an output of the local commitment tx?
        val spendsTheCommitTx = localCommitPublished.commitTx.txid == outPoint.txid
        // is the tx one of our 3rd stage delayed txes? (a 3rd stage tx is a tx spending the output of an htlc tx, which
        // is itself spending the output of the commitment tx)
        val is3rdStageDelayedTx = localCommitPublished.claimHtlcDelayedTxs.map(_.txid).contains(tx.txid)
        isCommitTx || spendsTheCommitTx || is3rdStageDelayedTx
      }
      // then we add the relevant outpoints to the map keeping track of which txid spends which outpoint
      localCommitPublished.copy(irrevocablySpent = localCommitPublished.irrevocablySpent ++ relevantOutpoints.map(o => o -> tx.txid).toMap)
    }

    /**
     * In CLOSING state, when we are notified that a transaction has been confirmed, we check if this tx belongs in the
     * remote commit scenario and keep track of it.
     *
     * We need to keep track of all transactions spending the outputs of the commitment tx, because some outputs can be
     * spent both by us and our counterparty. Because of that, some of our transactions may never confirm and we don't
     * want to wait forever before declaring that the channel is CLOSED.
     *
     * @param tx a transaction that has been irrevocably confirmed
     */
    def updateRemoteCommitPublished(remoteCommitPublished: RemoteCommitPublished, tx: Transaction): RemoteCommitPublished = {
      // even if our txes only have one input, maybe our counterparty uses a different scheme so we need to iterate
      // over all of them to check if they are relevant
      val relevantOutpoints = tx.txIn.map(_.outPoint).filter { outPoint =>
        // is this the commit tx itself ? (we could do this outside of the loop...)
        val isCommitTx = remoteCommitPublished.commitTx.txid == tx.txid
        // does the tx spend an output of the remote commitment tx?
        val spendsTheCommitTx = remoteCommitPublished.commitTx.txid == outPoint.txid
        isCommitTx || spendsTheCommitTx
      }
      // then we add the relevant outpoints to the map keeping track of which txid spends which outpoint
      remoteCommitPublished.copy(irrevocablySpent = remoteCommitPublished.irrevocablySpent ++ relevantOutpoints.map(o => o -> tx.txid).toMap)
    }

    /**
     * In CLOSING state, when we are notified that a transaction has been confirmed, we check if this tx belongs in the
     * revoked commit scenario and keep track of it.
     *
     * We need to keep track of all transactions spending the outputs of the commitment tx, because some outputs can be
     * spent both by us and our counterparty. Because of that, some of our transactions may never confirm and we don't
     * want to wait forever before declaring that the channel is CLOSED.
     *
     * @param tx a transaction that has been irrevocably confirmed
     */
    def updateRevokedCommitPublished(revokedCommitPublished: RevokedCommitPublished, tx: Transaction): RevokedCommitPublished = {
      // even if our txs only have one input, maybe our counterparty uses a different scheme so we need to iterate
      // over all of them to check if they are relevant
      val relevantOutpoints = tx.txIn.map(_.outPoint).filter { outPoint =>
        // is this the commit tx itself ? (we could do this outside of the loop...)
        val isCommitTx = revokedCommitPublished.commitTx.txid == tx.txid
        // does the tx spend an output of the remote commitment tx?
        val spendsTheCommitTx = revokedCommitPublished.commitTx.txid == outPoint.txid
        // is the tx one of our 3rd stage delayed txs? (a 3rd stage tx is a tx spending the output of an htlc tx, which
        // is itself spending the output of the commitment tx)
        val is3rdStageDelayedTx = revokedCommitPublished.claimHtlcDelayedPenaltyTxs.map(_.txid).contains(tx.txid)
        // does the tx spend an output of an htlc tx? (in which case it may invalidate one of our claim-htlc-delayed-penalty)
        val spendsHtlcOutput = revokedCommitPublished.claimHtlcDelayedPenaltyTxs.flatMap(_.txIn).map(_.outPoint).contains(outPoint)
        isCommitTx || spendsTheCommitTx || is3rdStageDelayedTx || spendsHtlcOutput
      }
      // then we add the relevant outpoints to the map keeping track of which txid spends which outpoint
      revokedCommitPublished.copy(irrevocablySpent = revokedCommitPublished.irrevocablySpent ++ relevantOutpoints.map(o => o -> tx.txid).toMap)
    }

    /**
     * A local commit is considered done when:
     * - all commitment tx outputs that we can spend have been spent and confirmed (even if the spending tx was not ours)
     * - all 3rd stage txes (txes spending htlc txes) have been confirmed
     */
    def isLocalCommitDone(localCommitPublished: LocalCommitPublished): Boolean = {
      // is the commitment tx buried? (we need to check this because we may not have any outputs)
      val isCommitTxConfirmed = localCommitPublished.irrevocablySpent.values.toSet.contains(localCommitPublished.commitTx.txid)
      // are there remaining spendable outputs from the commitment tx? we just subtract all known spent outputs from the ones we control
      // NB: we ignore anchors here, claiming them can be batched later
      val commitOutputsSpendableByUs = (localCommitPublished.claimMainDelayedOutputTx.toSeq ++ localCommitPublished.htlcSuccessTxs ++ localCommitPublished.htlcTimeoutTxs)
        .flatMap(_.txIn.map(_.outPoint)).toSet -- localCommitPublished.irrevocablySpent.keys
      // which htlc delayed txes can we expect to be confirmed?
      val unconfirmedHtlcDelayedTxes = localCommitPublished.claimHtlcDelayedTxs
        // only the txes which parents are already confirmed may get confirmed (note that this also eliminates outputs that have been double-spent by a competing tx)
        .filter(tx => (tx.txIn.map(_.outPoint.txid).toSet -- localCommitPublished.irrevocablySpent.values).isEmpty)
        .filterNot(tx => localCommitPublished.irrevocablySpent.values.toSet.contains(tx.txid)) // has the tx already been confirmed?
      isCommitTxConfirmed && commitOutputsSpendableByUs.isEmpty && unconfirmedHtlcDelayedTxes.isEmpty
    }

    /**
     * A remote commit is considered done when all commitment tx outputs that we can spend have been spent and confirmed
     * (even if the spending tx was not ours).
     */
    def isRemoteCommitDone(remoteCommitPublished: RemoteCommitPublished): Boolean = {
      // is the commitment tx buried? (we need to check this because we may not have any outputs)
      val isCommitTxConfirmed = remoteCommitPublished.irrevocablySpent.values.toSet.contains(remoteCommitPublished.commitTx.txid)
      // are there remaining spendable outputs from the commitment tx?
      val commitOutputsSpendableByUs = (remoteCommitPublished.claimMainOutputTx.toSeq ++ remoteCommitPublished.claimHtlcSuccessTxs ++ remoteCommitPublished.claimHtlcTimeoutTxs)
        .flatMap(_.txIn.map(_.outPoint)).toSet -- remoteCommitPublished.irrevocablySpent.keys
      isCommitTxConfirmed && commitOutputsSpendableByUs.isEmpty
    }

    /**
     * A remote commit is considered done when all commitment tx outputs that we can spend have been spent and confirmed
     * (even if the spending tx was not ours).
     */
    def isRevokedCommitDone(revokedCommitPublished: RevokedCommitPublished): Boolean = {
      // is the commitment tx buried? (we need to check this because we may not have any outputs)
      val isCommitTxConfirmed = revokedCommitPublished.irrevocablySpent.values.toSet.contains(revokedCommitPublished.commitTx.txid)
      // are there remaining spendable outputs from the commitment tx?
      val commitOutputsSpendableByUs = (revokedCommitPublished.claimMainOutputTx.toSeq ++ revokedCommitPublished.mainPenaltyTx ++ revokedCommitPublished.htlcPenaltyTxs)
        .flatMap(_.txIn.map(_.outPoint)).toSet -- revokedCommitPublished.irrevocablySpent.keys
      // which htlc delayed txs can we expect to be confirmed?
      val unconfirmedHtlcDelayedTxs = revokedCommitPublished.claimHtlcDelayedPenaltyTxs
        // only the txs which parents are already confirmed may get confirmed (note that this also eliminates outputs that have been double-spent by a competing tx)
        .filter(tx => (tx.txIn.map(_.outPoint.txid).toSet -- revokedCommitPublished.irrevocablySpent.values).isEmpty)
        // if one of the tx inputs has been spent, the tx has already been confirmed or a competing tx has been confirmed
        .filterNot(tx => tx.txIn.exists(txIn => revokedCommitPublished.irrevocablySpent.contains(txIn.outPoint)))
      isCommitTxConfirmed && commitOutputsSpendableByUs.isEmpty && unconfirmedHtlcDelayedTxs.isEmpty
    }

    def inputsAlreadySpent(irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty)(tx: Transaction): Boolean =
      tx.txIn.exists(txIn => irrevocablySpent contains txIn.outPoint)
  }
}
