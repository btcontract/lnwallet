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
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, ripemd160, sha256}
import immortan.crypto.Tools.{Any2Some, newFeerate}
import immortan.{ChannelBag, LNParams}
import scala.util.{Success, Try}

import fr.acinq.eclair.crypto.Generators
import scodec.bits.ByteVector


object Helpers {
  def validateParamsFundee(open: OpenChannel, commits: NormalCommits): Unit = {
    val reserveToFundingRatio = open.channelReserveSatoshis.toLong.toDouble / Math.max(open.fundingSatoshis.toLong, 1L)
    if (reserveToFundingRatio > LNParams.maxReserveToFundingRatio) throw ChannelReserveTooHigh(open.temporaryChannelId, reserveToFundingRatio, LNParams.maxReserveToFundingRatio)
    if (open.maxAcceptedHtlcs > LNParams.maxAcceptedHtlcs) throw InvalidMaxAcceptedHtlcs(open.temporaryChannelId, open.maxAcceptedHtlcs, LNParams.maxAcceptedHtlcs)
    if (open.dustLimitSatoshis > open.channelReserveSatoshis) throw DustLimitTooLarge(open.temporaryChannelId, open.dustLimitSatoshis, open.channelReserveSatoshis)
    if (open.dustLimitSatoshis < LNParams.minDustLimit) throw DustLimitTooSmall(open.temporaryChannelId, open.dustLimitSatoshis, LNParams.minDustLimit)
    if (open.pushMsat > open.fundingSatoshis) throw InvalidPushAmount(open.temporaryChannelId, open.pushMsat, open.fundingSatoshis.toMilliSatoshi)
    if (open.toSelfDelay > LNParams.maxToLocalDelay) throw ToSelfDelayTooHigh(open.temporaryChannelId, open.toSelfDelay, LNParams.maxToLocalDelay)
    if (LNParams.chainHash != open.chainHash) throw InvalidChainHash(open.temporaryChannelId, local = LNParams.chainHash, remote = open.chainHash)
    if (open.feeratePerKw < FeeratePerKw.MinimumFeeratePerKw) throw FeerateTooSmall(open.temporaryChannelId, open.feeratePerKw)

    if (open.fundingSatoshis < LNParams.minFundingSatoshis || open.fundingSatoshis > LNParams.maxFundingSatoshis)
      throw InvalidFundingAmount(open.temporaryChannelId, open.fundingSatoshis, LNParams.minFundingSatoshis, LNParams.maxFundingSatoshis)

    newFeerate(LNParams.feeRates.info, commits.localCommit.spec, LNParams.shouldRejectPaymentFeerateDiff).foreach { localFeeratePerKw =>
      throw FeerateTooDifferent(open.temporaryChannelId, localFeeratePerKw, open.feeratePerKw)
    }

    val (toLocalMsat, toRemoteMsat) = (open.pushMsat, open.fundingSatoshis.toMilliSatoshi - open.pushMsat)
    val invalidReserve = toLocalMsat < open.channelReserveSatoshis && toRemoteMsat < open.channelReserveSatoshis
    if (invalidReserve) throw ChannelReserveNotMet(open.temporaryChannelId, toLocalMsat, toRemoteMsat, open.channelReserveSatoshis)
  }

  def validateParamsFunder(open: OpenChannel, accept: AcceptChannel): Unit = {
    val reserveToFundingRatio = accept.channelReserveSatoshis.toLong.toDouble / Math.max(open.fundingSatoshis.toLong, 1)
    if (reserveToFundingRatio > LNParams.maxReserveToFundingRatio) throw ChannelReserveTooHigh(open.temporaryChannelId, reserveToFundingRatio, LNParams.maxReserveToFundingRatio)
    if (accept.channelReserveSatoshis < open.dustLimitSatoshis) throw ChannelReserveBelowOurDustLimit(accept.temporaryChannelId, accept.channelReserveSatoshis, open.dustLimitSatoshis)
    if (open.channelReserveSatoshis < accept.dustLimitSatoshis) throw DustLimitAboveOurChannelReserve(accept.temporaryChannelId, accept.dustLimitSatoshis, open.channelReserveSatoshis)
    if (accept.dustLimitSatoshis > accept.channelReserveSatoshis) throw DustLimitTooLarge(accept.temporaryChannelId, accept.dustLimitSatoshis, accept.channelReserveSatoshis)
    if (accept.maxAcceptedHtlcs > LNParams.maxAcceptedHtlcs) throw InvalidMaxAcceptedHtlcs(accept.temporaryChannelId, accept.maxAcceptedHtlcs, LNParams.maxAcceptedHtlcs)
    if (accept.dustLimitSatoshis < LNParams.minDustLimit) throw DustLimitTooSmall(accept.temporaryChannelId, accept.dustLimitSatoshis, LNParams.minDustLimit)
    if (accept.toSelfDelay > LNParams.maxToLocalDelay) throw ToSelfDelayTooHigh(accept.temporaryChannelId, accept.toSelfDelay, LNParams.maxToLocalDelay)
  }

  object Funding {
    def makeFundingInputInfo(fundingTxId: ByteVector32, fundingTxOutputIndex: Int, fundingSatoshis: Satoshi, fundingPubkey1: PublicKey, fundingPubkey2: PublicKey): InputInfo = {
      val fundingScript = multiSig2of2(fundingPubkey1, fundingPubkey2)
      val txOut = TxOut(fundingSatoshis, Script pay2wsh fundingScript)
      val outPoint = OutPoint(fundingTxId, fundingTxOutputIndex)
      InputInfo(outPoint, txOut, Script write fundingScript)
    }

    def makeFirstCommitTxs(channelVersion: ChannelVersion, localParams: LocalParams, remoteParams: RemoteParams,
                           fundingAmount: Satoshi, pushMsat: MilliSatoshi, initialFeeratePerKw: FeeratePerKw, fundingTxHash: ByteVector32,
                           fundingTxOutputIndex: Int, remoteFirstPerCommitmentPoint: PublicKey): (CommitmentSpec, CommitTx, CommitmentSpec, CommitTx) = {

      val toLocalMsat = if (localParams.isFunder) fundingAmount.toMilliSatoshi - pushMsat else pushMsat
      val toRemoteMsat = if (localParams.isFunder) pushMsat else fundingAmount.toMilliSatoshi - pushMsat

      val localSpec = CommitmentSpec(feeratePerKw = initialFeeratePerKw, toLocal = toLocalMsat, toRemote = toRemoteMsat)
      val remoteSpec = CommitmentSpec(feeratePerKw = initialFeeratePerKw, toLocal = toRemoteMsat, toRemote = toLocalMsat)

      val localPerCommitmentPoint = localParams.keys.commitmentPoint(index = 0L)
      val commitmentInput = makeFundingInputInfo(fundingTxHash, fundingTxOutputIndex, fundingAmount, localParams.keys.fundingKey.publicKey, remoteParams.fundingPubKey)
      val (remoteCommitTx, _, _) = NormalCommits.makeRemoteTxs(channelVersion, 0L, localParams, remoteParams, commitmentInput, remoteFirstPerCommitmentPoint, remoteSpec)
      val (localCommitTx, _, _) = NormalCommits.makeLocalTxs(channelVersion, 0L, localParams, remoteParams, commitmentInput, localPerCommitmentPoint, localSpec)
      (localSpec, localCommitTx, remoteSpec, remoteCommitTx)
    }
  }

  def checkLocalCommit(d: HasNormalCommitments, nextRemoteRevocationNumber: Long): Boolean =
    if (d.commitments.localCommit.index == nextRemoteRevocationNumber + 1) true // We are in sync
    else if (d.commitments.localCommit.index > nextRemoteRevocationNumber + 1) true // Remote is behind: we return true because things are fine on our side
    else if (d.commitments.localCommit.index == nextRemoteRevocationNumber) true // They sent a new commitSig, we have received it but they didn't receive our revocation
    else false // We are behind

  def checkRemoteCommit(d: HasNormalCommitments, nextLocalCommitmentNumber: Long): Boolean = d.commitments.remoteNextCommitInfo match {
    case Left(waitingForRevocation) if nextLocalCommitmentNumber == waitingForRevocation.nextRemoteCommit.index => true // We just sent a new commit_sig but they didn't receive it
    case Left(waitingForRevocation) if nextLocalCommitmentNumber == (waitingForRevocation.nextRemoteCommit.index + 1) => true // We sent a new commitSig, they have received it but we haven't received their revocation
    case Left(waitingForRevocation) if nextLocalCommitmentNumber < waitingForRevocation.nextRemoteCommit.index => true // They are behind: we return true because things are fine on our side
    case pk if pk.isRight && nextLocalCommitmentNumber == (d.commitments.remoteCommit.index + 1) => true // They have acknowledged the last commit_sig we sent
    case pk if pk.isRight && nextLocalCommitmentNumber < (d.commitments.remoteCommit.index + 1) => true // They are behind
    case _ => false // We are behind
  }

  type HashToPreimage = Map[ByteVector32, ByteVector32]
  def extractRevealedPreimages(updates: Iterable[UpdateMessage] = Nil): HashToPreimage =
    updates.collect { case upd: UpdateFulfillHtlc => upd.paymentHash -> upd.paymentPreimage }.toMap

  object Closing {
    sealed trait ClosingType

    case class MutualClose(tx: Transaction) extends ClosingType
    case class LocalClose(localCommit: LocalCommit, localCommitPublished: LocalCommitPublished) extends ClosingType

    sealed trait RemoteClose extends ClosingType {
      def remoteCommitPublished: RemoteCommitPublished
      def remoteCommit: RemoteCommit
    }

    case class CurrentRemoteClose(remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished) extends RemoteClose
    case class NextRemoteClose(remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished) extends RemoteClose
    case class RevokedClose(revokedCommitPublished: RevokedCommitPublished) extends ClosingType
    case class RecoveryClose(remoteCommitPublished: RemoteCommitPublished) extends ClosingType

    def isClosingTypeAlreadyKnown(c: DATA_CLOSING): Option[ClosingType] = c match {
      case _ if c.localCommitPublished.exists(_.isCommitConfirmed) => LocalClose(c.commitments.localCommit, c.localCommitPublished.get).asSome
      case _ if c.remoteCommitPublished.exists(_.isCommitConfirmed) => CurrentRemoteClose(c.commitments.remoteCommit, c.remoteCommitPublished.get).asSome
      case _ if c.nextRemoteCommitPublished.exists(_.isCommitConfirmed) => NextRemoteClose(c.commitments.remoteNextCommitInfo.left.get.nextRemoteCommit, c.nextRemoteCommitPublished.get).asSome
      case _ if c.futureRemoteCommitPublished.exists(_.isCommitConfirmed) => c.futureRemoteCommitPublished.map(RecoveryClose)
      case _ => c.revokedCommitPublished.find(_.isCommitConfirmed).map(RevokedClose)
    }

    def isValidFinalScriptPubkey(scriptPubKey: ByteVector): Boolean = Try(Script parse scriptPubKey) match {
      case Success(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubkeyHash, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil) if pubkeyHash.size == 20 => true
      case Success(OP_HASH160 :: OP_PUSHDATA(scriptHash, _) :: OP_EQUAL :: Nil) if scriptHash.size == 20 => true
      case Success(OP_0 :: OP_PUSHDATA(pubkeyHash, _) :: Nil) if pubkeyHash.size == 20 => true
      case Success(OP_0 :: OP_PUSHDATA(scriptHash, _) :: Nil) if scriptHash.size == 32 => true
      case _ => false
    }

    def firstClosingFee(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, feeratePerKw: FeeratePerKw): Satoshi = {
      val dummyClosingTx = Transactions.makeClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey, commitments.localParams.isFunder, 0L.sat, 0L.sat, commitments.localCommit.spec)
      val closingWeight = Transaction.weight(Transactions.addSigs(dummyClosingTx, invalidPubKey, commitments.remoteParams.fundingPubKey, Transactions.PlaceHolderSig, Transactions.PlaceHolderSig).tx)
      Transactions.weight2fee(feeratePerKw, closingWeight)
    }

    def firstClosingFee(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, conf: OnChainFeeConf): Satoshi = {
      val feeratePerKw = conf.feeEstimator.getFeeratePerKw(conf.feeTargets.mutualCloseBlockTarget).min(commitments.localCommit.spec.feeratePerKw)
      firstClosingFee(commitments, localScriptPubkey, remoteScriptPubkey, feeratePerKw)
    }

    def nextClosingFee(localClosingFee: Satoshi, remoteClosingFee: Satoshi): Satoshi = (localClosingFee + remoteClosingFee) / 4 * 2

    def makeFirstClosingTx(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, conf: OnChainFeeConf): (ClosingTx, ClosingSigned) = {
      val closingFee = firstClosingFee(commitments, localScriptPubkey, remoteScriptPubkey, conf)
      makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, closingFee)
    }

    def makeClosingTx(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, closingFee: Satoshi): (ClosingTx, ClosingSigned) = {
      require(isValidFinalScriptPubkey(remoteScriptPubkey), "invalid remoteScriptPubkey")
      require(isValidFinalScriptPubkey(localScriptPubkey), "invalid localScriptPubkey")

      val dustLimitSatoshis = commitments.localParams.dustLimit.max(commitments.remoteParams.dustLimit)
      val closingTx = Transactions.makeClosingTx(commitments.commitInput, localScriptPubkey, remoteScriptPubkey, commitments.localParams.isFunder, dustLimitSatoshis, closingFee, commitments.localCommit.spec)
      val localClosingSig = Transactions.sign(closingTx, commitments.localParams.keys.fundingKey.privateKey, TxOwner.Local, commitments.channelVersion.commitmentFormat)
      val closingSigned = ClosingSigned(commitments.channelId, closingFee, localClosingSig)
      (closingTx, closingSigned)
    }

    def checkClosingSignature(commitments: NormalCommits, localScriptPubkey: ByteVector, remoteScriptPubkey: ByteVector, remoteClosingFee: Satoshi, remoteClosingSig: ByteVector64): Transaction = {
      val lastCommitFeeSatoshi = commitments.commitInput.txOut.amount - commitments.localCommit.publishableTxs.commitTx.tx.txOut.map(_.amount).sum
      if (remoteClosingFee > lastCommitFeeSatoshi) throw ChannelTransitionFail(commitments.channelId)

      val localFundingKey = commitments.localParams.keys.fundingKey.publicKey
      val (closingTx, closingSigned) = makeClosingTx(commitments, localScriptPubkey, remoteScriptPubkey, closingFee = remoteClosingFee)
      val signedTx = Transactions.addSigs(closingTx, localFundingKey, commitments.remoteParams.fundingPubKey, closingSigned.signature, remoteClosingSig)
      if (Transactions.checkSpendable(signedTx).isFailure) throw ChannelTransitionFail(commitments.channelId)
      signedTx.tx
    }

    type SkippedOrTxInfo = Either[TxGenerationSkipped, TransactionWithInputInfo]
    private def generateTx(attempt: => SkippedOrTxInfo): Option[TransactionWithInputInfo] =
      Try(attempt) map { case Right(txinfo) => Some(txinfo) case _ => None } getOrElse None

    def claimCurrentLocalCommitTxOutputs(cs: NormalCommits, tx: Transaction, conf: OnChainFeeConf): LocalCommitPublished = {
      require(cs.localCommit.publishableTxs.commitTx.tx.txid == tx.txid, "Txid mismatch, provided tx is not the current local commit tx")

      val localPerCommitmentPoint = cs.localParams.keys.commitmentPoint(cs.localCommit.index)
      val localRevPubkey = Generators.revocationPubKey(cs.remoteParams.revocationBasepoint, localPerCommitmentPoint)
      val localDelayPubkey = Generators.derivePubKey(cs.localParams.keys.delayedPaymentKey.publicKey, localPerCommitmentPoint)
      val feeratePerKwDelayed = conf.feeEstimator.getFeeratePerKw(conf.feeTargets.claimMainBlockTarget)
      val preimages = extractRevealedPreimages(cs.localChanges.all)

      val mainDelayedTx = generateTx {
        val delayedToSig = cs.localParams.keys.sign(_: ClaimLocalDelayedOutputTx, cs.localParams.keys.delayedPaymentKey.privateKey, localPerCommitmentPoint, TxOwner.Local, cs.channelVersion.commitmentFormat)
        val tx1 = Transactions.makeClaimLocalDelayedOutputTx(tx, cs.localParams.dustLimit, localRevPubkey, cs.remoteParams.toSelfDelay, localDelayPubkey, cs.localParams.defaultFinalScriptPubKey, feeratePerKwDelayed)
        for (claimDelayed <- tx1.right) yield Transactions.addSigs(localSig = delayedToSig(claimDelayed), claimDelayedOutputTx = claimDelayed)
      }

      val htlcTxes = cs.localCommit.publishableTxs.htlcTxsAndSigs.flatMap {
        case HtlcTxAndSigs(info: HtlcSuccessTx, localSig, remoteSig) if preimages.contains(info.paymentHash) => generateTx {
          val info1 = Transactions.addSigs(info, localSig, remoteSig, preimages(info.paymentHash), cs.channelVersion.commitmentFormat)
          // Incoming htlc for which we have the preimage: we spend it directly
          Right(info1)
        }

        case HtlcTxAndSigs(info: HtlcTimeoutTx, localSig, remoteSig) => generateTx {
          val info1 = Transactions.addSigs(info, localSig, remoteSig, cs.channelVersion.commitmentFormat)
          // Outgoing htlc: the only thing to do is try to get back our funds after timeout
          Right(info1)
        }

        case _ =>
          None
      }

      val htlcDelayedTxes = htlcTxes.flatMap {
        info: TransactionWithInputInfo => generateTx {
          val delayedToSig = cs.localParams.keys.sign(_: ClaimLocalDelayedOutputTx, cs.localParams.keys.delayedPaymentKey.privateKey, localPerCommitmentPoint, TxOwner.Local, cs.channelVersion.commitmentFormat)
          val tx1 = Transactions.makeClaimLocalDelayedOutputTx(info.tx, cs.localParams.dustLimit, localRevPubkey, cs.remoteParams.toSelfDelay, localDelayPubkey, cs.localParams.defaultFinalScriptPubKey, feeratePerKwDelayed)
          for (claimDelayed <- tx1.right) yield Transactions.addSigs(localSig = delayedToSig(claimDelayed), claimDelayedOutputTx = claimDelayed)
        }
      }

      val mainDelayedTxOpt = for (info <- mainDelayedTx) yield info.tx
      val htlcDelayedTxesOpt = for (info <- htlcDelayedTxes) yield info.tx
      val successTxs = htlcTxes.collect { case success: HtlcSuccessTx => success.tx }
      val timeoutTxs = htlcTxes.collect { case timeout: HtlcTimeoutTx => timeout.tx }
      LocalCommitPublished(tx, mainDelayedTxOpt, successTxs, timeoutTxs, htlcDelayedTxesOpt)
    }

    def claimRemoteCommitTxOutputs(cs: NormalCommits, remoteCommit: RemoteCommit, tx: Transaction, feeEstimator: FeeEstimator): RemoteCommitPublished = {
      val remoteCommitTx = NormalCommits.makeRemoteTxs(cs.channelVersion, remoteCommit.index, cs.localParams, cs.remoteParams, cs.commitInput, remoteCommit.remotePerCommitmentPoint, remoteCommit.spec)._1
      require(remoteCommitTx.tx.txid == tx.txid, "txid mismatch, cannot recompute the current remote commit tx")

      val remoteHtlcPubkey = Generators.derivePubKey(cs.remoteParams.htlcBasepoint, remoteCommit.remotePerCommitmentPoint)
      val localHtlcPubkey = Generators.derivePubKey(cs.localParams.keys.htlcKey.publicKey, remoteCommit.remotePerCommitmentPoint)
      val remoteRevocationPubkey = Generators.revocationPubKey(cs.localParams.keys.revocationKey.publicKey, remoteCommit.remotePerCommitmentPoint)
      val remoteDelayedPaymentPubkey = Generators.derivePubKey(cs.remoteParams.delayedPaymentBasepoint, remoteCommit.remotePerCommitmentPoint)
      val localPaymentPubkey = Generators.derivePubKey(cs.localParams.keys.paymentKey.publicKey, remoteCommit.remotePerCommitmentPoint)

      val outputs: CommitmentOutputs =
        makeCommitTxOutputs(!cs.localParams.isFunder, cs.remoteParams.dustLimit, remoteRevocationPubkey, cs.localParams.toSelfDelay,
          remoteDelayedPaymentPubkey, localPaymentPubkey, remoteHtlcPubkey, localHtlcPubkey, cs.remoteParams.fundingPubKey,
          cs.localParams.keys.fundingKey.publicKey, remoteCommit.spec, cs.channelVersion.commitmentFormat)

      // Remember we are looking at the remote commitment so IN for them is really OUT for us and vice versa

      val format = cs.channelVersion.commitmentFormat
      val finalScriptPubKey = cs.localParams.defaultFinalScriptPubKey
      val feeratePerKwHtlc = feeEstimator.getFeeratePerKw(target = 3)
      val preimages = extractRevealedPreimages(cs.localChanges.all)

      val htlcTxes = remoteCommit.spec.htlcs.toList.flatMap {
        case OutgoingHtlc(add: UpdateAddHtlc) if preimages.contains(add.paymentHash) => generateTx {
          // Incoming (seen as outgoing from their side) htlc for which we have the preimage: we spend it directly right away
          val claimToSig = cs.localParams.keys.sign(_: ClaimHtlcSuccessTx, cs.localParams.keys.htlcKey.privateKey, remoteCommit.remotePerCommitmentPoint, TxOwner.Local, cs.channelVersion.commitmentFormat)
          val tx1 = Transactions.makeClaimHtlcSuccessTx(remoteCommitTx.tx, outputs, cs.localParams.dustLimit, localHtlcPubkey, remoteHtlcPubkey, remoteRevocationPubkey, finalScriptPubKey, add, feeratePerKwHtlc, format)
          for (info <- tx1.right) yield Transactions.addSigs(localSig = claimToSig(info), paymentPreimage = preimages(add.paymentHash), claimHtlcSuccessTx = info)
        }

        case IncomingHtlc(add: UpdateAddHtlc) => generateTx {
          // Outgoing (seen as incoming from their side) htlc: the only thing to do is try to get back our funds after timeout
          val claimToSig = cs.localParams.keys.sign(_: ClaimHtlcTimeoutTx, cs.localParams.keys.htlcKey.privateKey, remoteCommit.remotePerCommitmentPoint, TxOwner.Local, cs.channelVersion.commitmentFormat)
          val tx1 = Transactions.makeClaimHtlcTimeoutTx(remoteCommitTx.tx, outputs, cs.localParams.dustLimit, localHtlcPubkey, remoteHtlcPubkey, remoteRevocationPubkey, finalScriptPubKey, add, feeratePerKwHtlc, format)
          for (info <- tx1.right) yield Transactions.addSigs(localSig = claimToSig(info), claimHtlcTimeoutTx = info)
        }

        case _ =>
          None
      }

      val successTxs = htlcTxes.collect { case success: ClaimHtlcSuccessTx => success.tx }
      val timeoutTxs = htlcTxes.collect { case timeout: ClaimHtlcTimeoutTx => timeout.tx }
      RemoteCommitPublished(tx, claimMainOutputTx = None, successTxs, timeoutTxs)
    }

    def claimRevokedRemoteCommitTxOutputs(cs: NormalCommits, commitTx: Transaction, db: ChannelBag, feeEstimator: FeeEstimator): Option[RevokedCommitPublished] = {
      val txnumber = obscuredCommitTxNumber(decodeTxNumber(commitTx.txIn.head.sequence, commitTx.lockTime), !cs.localParams.isFunder, cs.remoteParams.paymentBasepoint, cs.localParams.walletStaticPaymentBasepoint)
      require(txnumber <= 0xffffffffffffL, "txnumber must be lesser than 48 bits long")

      cs.remotePerCommitmentSecrets.getHash(0xFFFFFFFFFFFFL - txnumber).map(PrivateKey.apply).map { remotePerCommitmentSecret =>
        val remoteDelayedPaymentPubkey = Generators.derivePubKey(cs.remoteParams.delayedPaymentBasepoint, remotePerCommitmentSecret.publicKey)
        val remoteRevocationPubkey = Generators.revocationPubKey(cs.localParams.keys.revocationKey.publicKey, remotePerCommitmentSecret.publicKey)
        val localHtlcPubkey = Generators.derivePubKey(cs.localParams.keys.htlcKey.publicKey, remotePerCommitmentSecret.publicKey)
        val remoteHtlcPubkey = Generators.derivePubKey(cs.remoteParams.htlcBasepoint, remotePerCommitmentSecret.publicKey)

        val finalScriptPubKey = cs.localParams.defaultFinalScriptPubKey
        val feeratePerKwPenalty = feeEstimator.getFeeratePerKw(target = 3)

        val mainPenaltyTx = generateTx {
          val claimToSig = cs.localParams.keys.sign(_: MainPenaltyTx, cs.localParams.keys.revocationKey.privateKey, remotePerCommitmentSecret, TxOwner.Local, cs.channelVersion.commitmentFormat)
          val tx1 = Transactions.makeMainPenaltyTx(commitTx, cs.localParams.dustLimit, remoteRevocationPubkey, finalScriptPubKey, cs.localParams.toSelfDelay, remoteDelayedPaymentPubkey, feeratePerKwPenalty)
          for (info <- tx1.right) yield Transactions.addSigs(revocationSig = claimToSig(info), mainPenaltyTx = info)
        }

        val htlcInfos = db.htlcInfos(txnumber)
        val received = for (info <- htlcInfos) yield Scripts.htlcReceived(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, info.hash160, info.cltvExpiry, cs.channelVersion.commitmentFormat)
        val offered = for (info <- htlcInfos) yield Scripts.htlcOffered(remoteHtlcPubkey, localHtlcPubkey, remoteRevocationPubkey, info.hash160, cs.channelVersion.commitmentFormat)
        val htlcsRedeemScripts = for (redeemScript <- received ++ offered) yield (Script write pay2wsh(redeemScript), Script write redeemScript)
        val htlcsRedeemScriptsMap = htlcsRedeemScripts.toMap

        val htlcPenaltyTxs = commitTx.txOut.zipWithIndex.toList.flatMap {
          case (txOut, outputIndex) if htlcsRedeemScriptsMap.contains(txOut.publicKeyScript) => generateTx {
            val claimToSig = cs.localParams.keys.sign(_: HtlcPenaltyTx, cs.localParams.keys.revocationKey.privateKey, remotePerCommitmentSecret, TxOwner.Local, cs.channelVersion.commitmentFormat)
            val tx1 = Transactions.makeHtlcPenaltyTx(commitTx, outputIndex, htlcsRedeemScriptsMap(txOut.publicKeyScript), cs.localParams.dustLimit, cs.localParams.defaultFinalScriptPubKey, feeratePerKwPenalty)
            for (info <- tx1.right) yield Transactions.addSigs(info, claimToSig(info), remoteRevocationPubkey)
          }

          case _ =>
            None
        }

        val punishMain = for (info <- mainPenaltyTx) yield info.tx
        val punishHtlcs = for (info <- htlcPenaltyTxs) yield info.tx

        RevokedCommitPublished(commitTx, claimMainOutputTx = None,
          punishMain, punishHtlcs, claimHtlcDelayedPenaltyTxs = Nil)
      }
    }

    def claimRevokedHtlcTxOutputs(cs: NormalCommits, revokedCommitPublished: RevokedCommitPublished, htlcTx: Transaction, feeEstimator: FeeEstimator): (Option[Transaction], RevokedCommitPublished) = {
      val isHtlc = !(revokedCommitPublished.claimMainOutputTx ++ revokedCommitPublished.mainPenaltyTx ++ revokedCommitPublished.htlcPenaltyTxs).map(_.txid).toSet.contains(htlcTx.txid)
      val isSpenFromRevoke = htlcTx.txIn.map(_.outPoint.txid).contains(revokedCommitPublished.commitTx.txid)

      if (isHtlc && isSpenFromRevoke) {
        val obscuredTxNumber = decodeTxNumber(revokedCommitPublished.commitTx.txIn.head.sequence, revokedCommitPublished.commitTx.lockTime)
        val txNumber = obscuredCommitTxNumber(obscuredTxNumber, !cs.localParams.isFunder, cs.remoteParams.paymentBasepoint, cs.localParams.walletStaticPaymentBasepoint)

        val infoOpt = cs.remotePerCommitmentSecrets.getHash(0xFFFFFFFFFFFFL - txNumber).map(PrivateKey.apply).flatMap { remotePerCommitmentSecret =>
          val remoteRevocationPubkey = Generators.revocationPubKey(cs.localParams.keys.revocationKey.publicKey, remotePerCommitmentSecret.publicKey)
          val remoteDelayedPaymentPubkey = Generators.derivePubKey(cs.remoteParams.delayedPaymentBasepoint, remotePerCommitmentSecret.publicKey)

          val finalScriptPubKey = cs.localParams.defaultFinalScriptPubKey
          val feeratePerKwPenalty = feeEstimator.getFeeratePerKw(target = 2)

          generateTx {
            val claimToSig = cs.localParams.keys.sign(_: ClaimHtlcDelayedOutputPenaltyTx, cs.localParams.keys.revocationKey.privateKey, remotePerCommitmentSecret, TxOwner.Local, cs.channelVersion.commitmentFormat)
            val tx1 = Transactions.makeClaimHtlcDelayedOutputPenaltyTx(htlcTx, cs.localParams.dustLimit, remoteRevocationPubkey, cs.localParams.toSelfDelay, remoteDelayedPaymentPubkey, finalScriptPubKey, feeratePerKwPenalty)

            tx1.right.map { htlcDelayedPenalty =>
              val signedTx = Transactions.addSigs(revocationSig = claimToSig(htlcDelayedPenalty), claimHtlcDelayedPenalty = htlcDelayedPenalty)
              Transaction.correctlySpends(signedTx.tx, inputs = Seq(htlcTx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
              signedTx
            }
          }
        }

        infoOpt.map { penaltyInfo =>
          val claimHtlcDelayedPenaltyTxs1 = revokedCommitPublished.claimHtlcDelayedPenaltyTxs :+ penaltyInfo.tx
          val revokedCommitPublished1 = revokedCommitPublished.copy(claimHtlcDelayedPenaltyTxs = claimHtlcDelayedPenaltyTxs1)
          Tuple2(penaltyInfo.tx.asSome, revokedCommitPublished1)
        } getOrElse Tuple2(None, revokedCommitPublished)
      } else Tuple2(None, revokedCommitPublished)
    }

    type OurAddAndPreimage = (UpdateAddHtlc, ByteVector32)
    def extractPreimages(localCommit: LocalCommit, tx: Transaction): Set[OurAddAndPreimage] = {
      val claimHtlcSuccess = tx.txIn.map(_.witness).collect(Scripts.extractPreimageFromClaimHtlcSuccess)
      val htlcSuccess = tx.txIn.map(_.witness).collect(Scripts.extractPreimageFromHtlcSuccess)
      val paymentPreimages = (htlcSuccess ++ claimHtlcSuccess).toSet

      for {
        preimage <- paymentPreimages
        OutgoingHtlc(add) <- localCommit.spec.htlcs
        if add.paymentHash == sha256(preimage)
      } yield (add, preimage)
    }

    def findTimedOutHtlc(tx: Transaction, hash160: ByteVector, htlcs: Seq[UpdateAddHtlc], extractPaymentHash: PartialFunction[ScriptWitness, ByteVector], timeoutTxs: Seq[Transaction] = Nil): Option[UpdateAddHtlc] = {
      val matchingTxs = timeoutTxs.filter(_.lockTime == tx.lockTime).filter(_.txIn.map(_.witness).collect(extractPaymentHash) contains hash160).sortBy(tx => tx.txOut.map(_.amount.toLong).sum -> tx.txid.toHex)
      val matchingHtlcs = htlcs.filter(add => add.cltvExpiry.toLong == tx.lockTime && ripemd160(add.paymentHash) == hash160).sortBy(add => add.amountMsat.toLong -> add.id)
      matchingHtlcs.zip(matchingTxs).collectFirst { case (add, timeoutTx) if timeoutTx.txid == tx.txid => add }
    }

    def timedoutHtlcs(cs: NormalCommits, localCommit: LocalCommit, localCommitPublished: LocalCommitPublished, tx: Transaction): (Set[UpdateAddHtlc], Boolean) = {
      def finder(hash: ByteVector): Option[UpdateAddHtlc] = findTimedOutHtlc(tx, hash, untrimmedHtlcs, Scripts.extractPaymentHashFromHtlcTimeout, localCommitPublished.htlcTimeoutTxs)
      lazy val untrimmedHtlcs = trimOfferedHtlcs(cs.localParams.dustLimit, localCommit.spec, cs.channelVersion.commitmentFormat).map(_.add)

      // Fail dusty HTLCs if it's a commit tx, otherwise find the ones with matching hash
      if (tx.txid == localCommit.publishableTxs.commitTx.tx.txid) (localCommit.spec.outgoingAdds -- untrimmedHtlcs, true)
      else (tx.txIn.map(_.witness).collect(Scripts.extractPaymentHashFromHtlcTimeout).flatMap(finder).toSet, false)
    }

    def timedoutHtlcs(cs: NormalCommits, remoteCommit: RemoteCommit, remoteCommitPublished: RemoteCommitPublished, tx: Transaction): (Set[UpdateAddHtlc], Boolean) = {
      def finder(hash: ByteVector): Option[UpdateAddHtlc] = findTimedOutHtlc(tx, hash, untrimmedHtlcs, Scripts.extractPaymentHashFromClaimHtlcTimeout, remoteCommitPublished.claimHtlcTimeoutTxs)
      lazy val untrimmedHtlcs = trimReceivedHtlcs(cs.remoteParams.dustLimit, remoteCommit.spec, cs.channelVersion.commitmentFormat).map(_.add)

      // Fail dusty HTLCs if it's a commit tx, otherwise find the ones with matching hash
      if (tx.txid == remoteCommit.txid) (remoteCommit.spec.incomingAdds -- untrimmedHtlcs, true)
      else (tx.txIn.map(_.witness).collect(Scripts.extractPaymentHashFromClaimHtlcTimeout).flatMap(finder).toSet, false)
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
        // NB: from the POV of remote, their incoming htlcs are our outgoing htlcs
        htlcsInRemoteCommit.collect(incoming) -- localCommit.spec.outgoingAdds
      } else if (remoteCommit.txid == tx.txid) {
        // their commit got confirmed
        nextRemoteCommit_opt match {
          case Some(nextRemoteCommit) =>
            // we had signed a new commitment but they committed the previous one
            nextRemoteCommit.spec.incomingAdds -- localCommit.spec.outgoingAdds
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
        nextRemoteCommit_opt.getOrElse(remoteCommit).spec.incomingAdds
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

  def chainFeePaid(tx: Transaction, data: DATA_CLOSING): Option[Satoshi] = {
    val isCommitTx = tx.txIn.map(_.outPoint).contains(data.commitments.commitInput.outPoint)
    val isFeeDefineable = tx.txIn.size == 1 && (data.commitments.localParams.isFunder || !isCommitTx)
    // Only the funder pays the fee for the commit tx, but 2nd-stage and 3rd-stage tx fees are paid by their recipients
    // we can compute the fees only for transactions with a single parent for which we know the output amount

    if (isFeeDefineable) {
      val outPoint = tx.txIn.head.outPoint
      val txMap = (data.balanceRefunds ++ data.paymentLeftoverRefunds).map(channelTx => channelTx.txid -> channelTx).toMap
      val parentTxOutOpt = if (isCommitTx) Some(data.commitments.commitInput.txOut) else txMap.get(outPoint.txid).map(_ txOut outPoint.index.toInt)
      parentTxOutOpt.map(_.amount - tx.txOut.map(_.amount).sum)
    } else None
  }
}
