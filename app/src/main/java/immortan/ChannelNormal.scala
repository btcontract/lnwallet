package immortan

import fr.acinq.eclair._
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import scala.concurrent.duration._
import fr.acinq.eclair.blockchain._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._
import fr.acinq.bitcoin.{ByteVector32, OutPoint, ScriptFlags, Transaction}
import fr.acinq.eclair.transactions.Transactions.TxOwner
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.channel.Helpers.Closing
import fr.acinq.eclair.payment.OutgoingPacket
import fr.acinq.bitcoin.Crypto.PrivateKey
import scala.collection.immutable.Queue
import fr.acinq.eclair.crypto.ShaChain
import scodec.bits.ByteVector
import immortan.utils.Rx
import scala.util.Try


object ChannelNormal {
  def make(initListeners: Set[ChannelListener], normalData: HasNormalCommitments, cw: WalletExt, bag: ChannelBag): ChannelNormal = new ChannelNormal(bag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, normalData.commitments.remoteInfo.nodeSpecificPair)
    def STORE(normalData1: PersistentChannelData): PersistentChannelData = bag.put(normalData1)
    val chainWallet: WalletExt = cw
    listeners = initListeners
    doProcess(normalData)
  }
}

abstract class ChannelNormal(bag: ChannelBag) extends Channel { me =>
  val chainWallet: WalletExt

  def doProcess(change: Any): Unit =
    Tuple3(data, change, state) match {

      // OPENING PHASE: FUNDER FLOW

      case (null, init: INPUT_INIT_FUNDER, -1) =>
        val emptyUpfrontShutdown: TlvStream[OpenChannelTlv] = TlvStream(ChannelTlv UpfrontShutdownScript ByteVector.empty)
        val open = OpenChannel(LNParams.chainHash, init.temporaryChannelId, init.fakeFunding.fundingAmount, init.pushAmount, init.localParams.dustLimit,
          init.localParams.maxHtlcValueInFlightMsat, init.localParams.channelReserve, init.localParams.htlcMinimum, init.initialFeeratePerKw, init.localParams.toSelfDelay,
          init.localParams.maxAcceptedHtlcs, init.localParams.keys.fundingKey.publicKey, init.localParams.keys.revocationKey.publicKey, init.localParams.walletStaticPaymentBasepoint,
          init.localParams.keys.delayedPaymentKey.publicKey, init.localParams.keys.fundingKey.publicKey, init.localParams.keys.commitmentPoint(index = 0L), init.channelFlags, emptyUpfrontShutdown)

        val data1 = DATA_WAIT_FOR_ACCEPT_CHANNEL(init, open)
        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(open)


      case (wait: DATA_WAIT_FOR_ACCEPT_CHANNEL, accept: AcceptChannel, WAIT_FOR_ACCEPT) =>
        val data1 = DATA_WAIT_FOR_FUNDING_INTERNAL(wait.initFunder, RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat,
          accept.channelReserveSatoshis, accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs, accept.fundingPubkey,
          accept.revocationBasepoint, accept.paymentBasepoint, accept.delayedPaymentBasepoint, accept.htlcBasepoint),
          accept.firstPerCommitmentPoint, wait.lastSent)

        Helpers.validateParamsFunder(data1.lastSent, accept)
        BECOME(data1, WAIT_FOR_ACCEPT)


      case (wait: DATA_WAIT_FOR_FUNDING_INTERNAL, realFunding: MakeFundingTxResponse, WAIT_FOR_ACCEPT) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFunder.channelVersion,
          wait.initFunder.localParams, wait.remoteParams, realFunding.fundingAmount, wait.initFunder.pushAmount, wait.initFunder.initialFeeratePerKw,
          realFunding.fundingTx.hash, realFunding.fundingTxOutputIndex, wait.remoteFirstPerCommitmentPoint)

        require(realFunding.fee == wait.initFunder.fakeFunding.fee)
        require(realFunding.fundingAmount == wait.initFunder.fakeFunding.fundingAmount)
        require(realFunding.fundingPubkeyScript == localCommitTx.input.txOut.publicKeyScript)

        val localSigOfRemoteTx = Transactions.sign(remoteCommitTx, wait.initFunder.localParams.keys.fundingKey.privateKey, TxOwner.Remote, wait.initFunder.channelVersion.commitmentFormat)
        val fundingCreated = FundingCreated(wait.initFunder.temporaryChannelId, realFunding.fundingTx.hash, realFunding.fundingTxOutputIndex, localSigOfRemoteTx)

        val data1 = DATA_WAIT_FOR_FUNDING_SIGNED(wait.initFunder.remoteInfo, channelId = toLongId(realFunding.fundingTx.hash, realFunding.fundingTxOutputIndex), wait.initFunder.localParams,
          wait.remoteParams, realFunding.fundingTx, realFunding.fee, localSpec, localCommitTx, RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, wait.remoteFirstPerCommitmentPoint),
          wait.lastSent.channelFlags, wait.initFunder.channelVersion, fundingCreated)

        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(fundingCreated)


      case (wait: DATA_WAIT_FOR_FUNDING_SIGNED, signed: FundingSigned, WAIT_FOR_ACCEPT) =>
        val localSigOfLocalTx = Transactions.sign(wait.localCommitTx, wait.localParams.keys.fundingKey.privateKey, TxOwner.Local, wait.channelVersion.commitmentFormat)
        val signedLocalCommitTx = Transactions.addSigs(wait.localCommitTx, wait.localParams.keys.fundingKey.publicKey, wait.remoteParams.fundingPubKey, localSigOfLocalTx, signed.signature)

        // Make sure their supplied signature is correct before committing
        require(Transactions.checkSpendable(signedLocalCommitTx).isSuccess)

        val publishableTxs = PublishableTxs(signedLocalCommitTx, Nil)
        val commits = NormalCommits(wait.channelFlags, wait.channelId, wait.channelVersion, Right(randomKey.publicKey), ShaChain.init, updateOpt = None,
          postCloseOutgoingResolvedIds = Set.empty, wait.remoteInfo, wait.localParams, wait.remoteParams, LocalCommit(index = 0L, wait.localSpec, publishableTxs),
          wait.remoteCommit, LocalChanges(Nil, Nil, Nil), RemoteChanges(Nil, Nil, Nil), localNextHtlcId = 0L, remoteNextHtlcId = 0L, signedLocalCommitTx.input)

        chainWallet.watcher ! WatchSpent(receiver, commits.commitInput.outPoint.txid, commits.commitInput.outPoint.index.toInt, commits.commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
        chainWallet.watcher ! WatchConfirmed(receiver, commits.commitInput.outPoint.txid, commits.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        // Persist a channel unconditionally, try to re-publish a funding tx on restart unconditionally (don't react to commit=false, we can't trust remote servers on this)
        StoreBecomeSend(DATA_WAIT_FOR_FUNDING_CONFIRMED(commits, wait.fundingTx.asSome, System.currentTimeMillis, Left(wait.lastSent), deferred = None), WAIT_FUNDING_DONE)
        chainWallet.wallet.commit(wait.fundingTx)

      // OPENING PHASE: FUNDEE FLOW

      case (null, init: INPUT_INIT_FUNDEE, -1) =>
        val emptyUpfrontShutdown: TlvStream[AcceptChannelTlv] = TlvStream(ChannelTlv UpfrontShutdownScript ByteVector.empty)
        val accept = AcceptChannel(init.theirOpen.temporaryChannelId, init.localParams.dustLimit, init.localParams.maxHtlcValueInFlightMsat, init.localParams.channelReserve,
          init.localParams.htlcMinimum, LNParams.minDepthBlocks, init.localParams.toSelfDelay, init.localParams.maxAcceptedHtlcs, init.localParams.keys.fundingKey.publicKey,
          init.localParams.keys.revocationKey.publicKey, init.localParams.walletStaticPaymentBasepoint, init.localParams.keys.delayedPaymentKey.publicKey,
          init.localParams.keys.htlcKey.publicKey, init.localParams.keys.commitmentPoint(index = 0L), emptyUpfrontShutdown)

        val data1 = DATA_WAIT_FOR_FUNDING_CREATED(init, RemoteParams(init.theirOpen.dustLimitSatoshis, init.theirOpen.maxHtlcValueInFlightMsat,
          init.theirOpen.channelReserveSatoshis, init.theirOpen.htlcMinimumMsat, init.theirOpen.toSelfDelay, init.theirOpen.maxAcceptedHtlcs,
          init.theirOpen.fundingPubkey, init.theirOpen.revocationBasepoint, init.theirOpen.paymentBasepoint,
          init.theirOpen.delayedPaymentBasepoint, init.theirOpen.htlcBasepoint), accept)

        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(accept)


      case (wait: DATA_WAIT_FOR_FUNDING_CREATED, created: FundingCreated, WAIT_FOR_ACCEPT) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFundee.channelVersion,
          wait.initFundee.localParams, wait.remoteParams, wait.initFundee.theirOpen.fundingSatoshis, wait.initFundee.theirOpen.pushMsat,
          wait.initFundee.theirOpen.feeratePerKw, created.fundingTxid, created.fundingOutputIndex,
          wait.initFundee.theirOpen.firstPerCommitmentPoint)

        val localSigOfLocalTx = Transactions.sign(localCommitTx, wait.initFundee.localParams.keys.fundingKey.privateKey, TxOwner.Local, wait.initFundee.channelVersion.commitmentFormat)
        val signedLocalCommitTx = Transactions.addSigs(localCommitTx, wait.initFundee.localParams.keys.fundingKey.publicKey, wait.remoteParams.fundingPubKey, localSigOfLocalTx, created.signature)
        val localSigOfRemoteTx = Transactions.sign(remoteCommitTx, wait.initFundee.localParams.keys.fundingKey.privateKey, TxOwner.Remote, wait.initFundee.channelVersion.commitmentFormat)
        val fundingSigned = FundingSigned(channelId = toLongId(created.fundingTxid, created.fundingOutputIndex), signature = localSigOfRemoteTx)

        val publishableTxs = PublishableTxs(signedLocalCommitTx, Nil)
        val commits = NormalCommits(wait.initFundee.theirOpen.channelFlags, fundingSigned.channelId, wait.initFundee.channelVersion, Right(randomKey.publicKey), ShaChain.init, updateOpt = None,
          postCloseOutgoingResolvedIds = Set.empty[Long], wait.initFundee.remoteInfo, wait.initFundee.localParams, wait.remoteParams, LocalCommit(index = 0L, localSpec, publishableTxs),
          RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, remotePerCommitmentPoint = wait.initFundee.theirOpen.firstPerCommitmentPoint), LocalChanges(Nil, Nil, Nil),
          RemoteChanges(Nil, Nil, Nil), localNextHtlcId = 0L, remoteNextHtlcId = 0L, signedLocalCommitTx.input)

        require(Transactions.checkSpendable(signedLocalCommitTx).isSuccess)
        Helpers.validateParamsFundee(wait.initFundee.theirOpen, commits)

        chainWallet.watcher ! WatchSpent(receiver, commits.commitInput.outPoint.txid, commits.commitInput.outPoint.index.toInt, commits.commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
        chainWallet.watcher ! WatchConfirmed(receiver, commits.commitInput.outPoint.txid, commits.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        StoreBecomeSend(DATA_WAIT_FOR_FUNDING_CONFIRMED(commits, None, System.currentTimeMillis, Right(fundingSigned), deferred = None), WAIT_FUNDING_DONE, fundingSigned)

      // Convert remote error into local exception in opening phase, it should be dealt with upstream

      case (_: INPUT_INIT_FUNDER | _: DATA_WAIT_FOR_ACCEPT_CHANNEL | _: DATA_WAIT_FOR_FUNDING_INTERNAL | _: DATA_WAIT_FOR_FUNDING_SIGNED, remote: Error, _) =>
        throw new RuntimeException(ErrorExt extractDescription remote)

      case (_: INPUT_INIT_FUNDEE | _: DATA_WAIT_FOR_FUNDING_CREATED, remote: Error, _) =>
        throw new RuntimeException(ErrorExt extractDescription remote)

      // AWAITING CONFIRMATION

      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, event: WatchEventConfirmed, WAIT_FUNDING_DONE) =>
        // Remote peer may send a tx which is unrelated to our agreed upon channel funding, that is, we won't be able to spend our commit tx, check this right away!
        def correct: Unit = Transaction.correctlySpends(wait.commitments.localCommit.publishableTxs.commitTx.tx, Seq(event.tx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

        if (Try(correct).isFailure) StoreBecomeSend(DATA_CLOSING(wait.commitments, System.currentTimeMillis), CLOSING) else {
          val shortChannelId = ShortChannelId(event.blockHeight, event.txIndex, wait.commitments.commitInput.outPoint.index.toInt)
          val fundingLocked = FundingLocked(nextPerCommitmentPoint = wait.commitments.localParams.keys.commitmentPoint(1L), channelId = wait.channelId)
          StoreBecomeSend(DATA_WAIT_FOR_FUNDING_LOCKED(wait.commitments, shortChannelId, fundingLocked), WAIT_FUNDING_DONE, fundingLocked)
          wait.deferred.foreach(process)
        }


      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, locked: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their message, they will re-send if we get disconnected
        BECOME(wait.copy(deferred = locked.asSome), WAIT_FUNDING_DONE)


      case (wait: DATA_WAIT_FOR_FUNDING_LOCKED, locked: FundingLocked, WAIT_FUNDING_DONE) =>
        val commits1 = wait.commitments.modify(_.remoteNextCommitInfo) setTo Right(locked.nextPerCommitmentPoint)
        StoreBecomeSend(DATA_NORMAL(commits1, wait.shortChannelId), OPEN)

      // MAIN LOOP

      case (some: HasNormalCommitments, WatchEventSpent(BITCOIN_FUNDING_SPENT, tx), OPEN | SLEEPING | CLOSING) =>
        // Catch all possible closings in one go, account for us receiving various closing txs multiple times

        some match {
          case closing: DATA_CLOSING if closing.mutualClosePublished.exists(_.txid == tx.txid) =>
          case closing: DATA_CLOSING if closing.localCommitPublished.exists(_.commitTx.txid == tx.txid) =>
          case closing: DATA_CLOSING if closing.remoteCommitPublished.exists(_.commitTx.txid == tx.txid) =>
          case closing: DATA_CLOSING if closing.nextRemoteCommitPublished.exists(_.commitTx.txid == tx.txid) =>
          case closing: DATA_CLOSING if closing.futureRemoteCommitPublished.exists(_.commitTx.txid == tx.txid) =>
          case closing: DATA_CLOSING if closing.mutualCloseProposed.exists(_.txid == tx.txid) => handleMutualClose(tx, closing)
          case negs: DATA_NEGOTIATING if negs.bestUnpublishedClosingTxOpt.exists(_.txid == tx.txid) => handleMutualClose(tx, negs)
          case negs: DATA_NEGOTIATING if negs.closingTxProposed.flatten.exists(_.unsignedTx.txid == tx.txid) => handleMutualClose(tx, negs)
          case waitPublishFuture: DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT => handleRemoteSpentFuture(tx, waitPublishFuture.commitments)
          case _ if some.commitments.remoteNextCommitInfo.left.exists(_.nextRemoteCommit.txid == tx.txid) => handleRemoteSpentNext(tx, some)
          case _ if tx.txid == some.commitments.remoteCommit.txid => handleRemoteSpentCurrent(tx, some)
          case _ => handleRemoteSpentOther(tx, some)
        }


      case (norm: DATA_NORMAL, CurrentBlockCount(tip), OPEN | SLEEPING) =>
        val sentExpiredRouted = norm.commitments.allOutgoing.exists(add => tip > add.cltvExpiry.toLong && add.fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED)
        val threshold = Transactions.receivedHtlcTrimThreshold(norm.commitments.remoteParams.dustLimit, norm.commitments.latestRemoteCommit.spec, norm.commitments.channelVersion.commitmentFormat)
        val largeReceivedRevealed = norm.commitments.revealedFulfills.filter(_.theirAdd.amountMsat > threshold * LNParams.minForceClosableIncomingHtlcAmountToFeeRatio)
        val expiredReceivedRevealed = largeReceivedRevealed.exists(tip > _.theirAdd.cltvExpiry.toLong - LNParams.ncFulfillSafetyBlocks)
        if (sentExpiredRouted || expiredReceivedRevealed) throw ChannelTransitionFail(norm.channelId)


      case (some: HasNormalCommitments, remoteInfo: RemoteNodeInfo, SLEEPING) if some.commitments.remoteInfo.nodeId == remoteInfo.nodeId =>
        StoreBecomeSend(some.modify(_.commitments.remoteInfo).setTo(remoteInfo), SLEEPING)


      case (norm: DATA_NORMAL, update: ChannelUpdate, OPEN | SLEEPING) if update.shortChannelId == norm.shortChannelId && norm.commitments.updateOpt.forall(_.timestamp < update.timestamp) =>
        StoreBecomeSend(norm.modify(_.commitments.updateOpt).setTo(update.asSome), state)


      case (norm: DATA_NORMAL, CMD_CHECK_FEERATE, OPEN) if norm.commitments.localParams.isFunder =>
        nextFeerate(norm, LNParams.shouldSendUpdateFeerateDiff).map(norm.commitments.sendFee).toList match {
          case (commits1, balanceAfterFeeUpdate, ourRatesUpdateMsg) :: Nil if balanceAfterFeeUpdate.toLong > 0L =>
            // Technically we still require feerate update at this point since local commit feerate is not refreshed yet
            // but we may start sending new HTLCs right away because remote peer will see them AFTER update signature
            StoreBecomeSend(norm.copy(commitments = commits1, feeUpdateRequired = false), OPEN, ourRatesUpdateMsg)
            doProcess(CMD_SIGN)

          case _ :: Nil =>
            // We need a feerate update but can't afford it right now so wait and reject outgoing adds
            // situation is expected to normalize by either sending it later or getting better feerates
            StoreBecomeSend(norm.copy(feeUpdateRequired = true), OPEN)

          case Nil if norm.feeUpdateRequired =>
            // We don't need a feerate update, but persisted state indicates it was required
            // we have probably gotten another feerates which are more in line with current ones
            StoreBecomeSend(norm.copy(feeUpdateRequired = false), OPEN)

          case Nil =>
            // Do nothing
        }


      // We may schedule shutdown while channel is offline
      case (norm: DATA_NORMAL, cmd: CMD_CLOSE, OPEN | SLEEPING) =>
        val localScriptPubKey = cmd.scriptPubKey.getOrElse(norm.commitments.localParams.defaultFinalScriptPubKey)
        val hasLocalHasUnsignedOutgoingHtlcs = norm.commitments.localHasUnsignedOutgoingHtlcs
        val isValidFinalScriptPubkey = Closing.isValidFinalScriptPubkey(localScriptPubKey)
        val shutdown = Shutdown(norm.channelId, localScriptPubKey)
        val norm1 = norm.copy(localShutdown = shutdown.asSome)

        if (cmd.force) {
          if (!isValidFinalScriptPubkey) spendLocalCurrent(norm)
          else if (norm.localShutdown.isDefined) spendLocalCurrent(norm)
          else if (hasLocalHasUnsignedOutgoingHtlcs) spendLocalCurrent(norm)
          else StoreBecomeSend(norm1, state, shutdown)
        } else {
          if (!isValidFinalScriptPubkey) throw CMDException(CMD_CLOSE.INVALID_CLOSING_PUBKEY, cmd)
          else if (norm.localShutdown.isDefined) throw CMDException(CMD_CLOSE.ALREADY_IN_PROGRESS, cmd)
          else if (hasLocalHasUnsignedOutgoingHtlcs) throw CMDException(CMD_CLOSE.CHANNEL_BUSY, cmd)
          else StoreBecomeSend(norm1, state, shutdown)
        }


      // In all other states except normal we force-close right away
      case (some: HasNormalCommitments, _: CMD_CLOSE, OPEN | SLEEPING) =>
        spendLocalCurrent(some)


      case (_: HasNormalCommitments, remote: Error, OPEN | SLEEPING) =>
        throw RemoteErrorException(ErrorExt extractDescription remote)


      case (norm: DATA_NORMAL, cmd: CMD_ADD_HTLC, OPEN | SLEEPING) =>
        norm.commitments.sendAdd(cmd, blockHeight = LNParams.blockCount.get) match {
          case _ if norm.localShutdown.nonEmpty || norm.remoteShutdown.nonEmpty =>
            events localAddRejected ChannelNotAbleToSend(cmd.incompleteAdd)

          case _ if SLEEPING == state =>
            // Tell outgoing FSM to not exclude this channel yet
            events localAddRejected ChannelOffline(cmd.incompleteAdd)

          case _ if norm.commitments.localParams.isFunder && norm.feeUpdateRequired =>
            // It's dangerous to send payments when we need a feerate update but can not afford it
            events localAddRejected ChannelNotAbleToSend(cmd.incompleteAdd)

          case _ if !norm.commitments.localParams.isFunder && cmd.fullTag.tag == PaymentTagTlv.TRAMPLOINE_ROUTED && nextFeerate(norm, LNParams.shouldRejectPaymentFeerateDiff).isDefined =>
            // It's dangerous to send routed payments as a fundee when we need a feerate update but peer has not sent us one yet
            events localAddRejected ChannelNotAbleToSend(cmd.incompleteAdd)

          case Left(reason) =>
            events localAddRejected reason

          case Right(commits1 ~~ updateAddHtlcMsg) =>
            BECOME(norm.copy(commitments = commits1), OPEN)
            SEND(msg = updateAddHtlcMsg)
            process(CMD_SIGN)
        }


      case (_, cmd: CMD_ADD_HTLC, _) =>
        // Instruct upstream to skip this channel in such a state
        val reason = ChannelNotAbleToSend(cmd.incompleteAdd)
        events localAddRejected reason


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (norm: DATA_NORMAL, cmd: CMD_FULFILL_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val (commits1, ourFulfillMsg) = norm.commitments.sendFulfill(cmd)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(ourFulfillMsg)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (norm: DATA_NORMAL, cmd: CMD_FAIL_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val msg = OutgoingPacket.buildHtlcFailure(cmd, theirAdd = cmd.theirAdd)
        val commits1 = norm.commitments.addLocalProposal(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(msg)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (norm: DATA_NORMAL, cmd: CMD_FAIL_MALFORMED_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFailMalformedHtlc(norm.channelId, cmd.theirAdd.id, cmd.onionHash, cmd.failureCode)
        val commits1 = norm.commitments.addLocalProposal(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(msg)


      case (norm: DATA_NORMAL, CMD_SIGN, OPEN) if norm.commitments.localHasChanges && norm.commitments.remoteNextCommitInfo.isRight =>
        // We have something to sign and remote unused pubKey, don't forget to store revoked HTLC data

        val (commits1, commitSigMessage, nextRemoteCommit) = norm.commitments.sendCommit
        val out = Transactions.trimOfferedHtlcs(norm.commitments.remoteParams.dustLimit, nextRemoteCommit.spec, norm.commitments.channelVersion.commitmentFormat)
        val in = Transactions.trimReceivedHtlcs(norm.commitments.remoteParams.dustLimit, nextRemoteCommit.spec, norm.commitments.channelVersion.commitmentFormat)
        // This includes hashing and db calls on potentially dozens of in-flight HTLCs, do this in separate thread but throw if it fails to know early
        Rx.ioQueue.foreach(_ => bag.putHtlcInfos(out ++ in, norm.shortChannelId, nextRemoteCommit.index), throw _)
        StoreBecomeSend(norm.copy(commitments = commits1), OPEN, commitSigMessage)


      case (norm: DATA_NORMAL, CMD_SIGN, OPEN) if norm.remoteShutdown.isDefined && !norm.commitments.localHasUnsignedOutgoingHtlcs =>
        // We have nothing to sign left AND no unsigned outgoing HTLCs AND remote peer wishes to close a channel
        maybeStartNegotiations(norm, norm.remoteShutdown.get)


      case (norm: DATA_NORMAL, theirAdd: UpdateAddHtlc, OPEN) =>
        val theirAddExt = UpdateAddHtlcExt(theirAdd, norm.commitments.remoteInfo)
        val commits1 = norm.commitments.receiveAdd(add = theirAdd)
        BECOME(norm.copy(commitments = commits1), OPEN)
        events addReceived theirAddExt


      case (norm: DATA_NORMAL, msg: UpdateFulfillHtlc, OPEN) =>
        val (commits1, ourAdd) = norm.commitments.receiveFulfill(msg)
        val fulfill = RemoteFulfill(ourAdd, msg.paymentPreimage)
        BECOME(norm.copy(commitments = commits1), OPEN)
        events fulfillReceived fulfill


      case (norm: DATA_NORMAL, msg: UpdateFailHtlc, OPEN) =>
        val commits1 = norm.commitments.receiveFail(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, msg: UpdateFailMalformedHtlc, OPEN) =>
        val commits1 = norm.commitments.receiveFailMalformed(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, commitSig: CommitSig, OPEN) =>
        val (commits1, revocation) = norm.commitments.receiveCommit(commitSig)
        // If feerate update is required AND becomes possible then we schedule another check shortly
        if (norm.feeUpdateRequired) Rx.ioQueue.delay(1.second).foreach(_ => process(CMD_CHECK_FEERATE), none)
        StoreBecomeSend(norm.copy(commitments = commits1), OPEN, revocation)
        process(CMD_SIGN)


      case (norm: DATA_NORMAL, revocation: RevokeAndAck, OPEN) =>
        val lastRemoteRejects: Seq[RemoteReject] = norm.commitments.remoteChanges.signed.collect {
          case fail: UpdateFailHtlc => RemoteUpdateFail(fail, norm.commitments.remoteCommit.spec.findIncomingHtlcById(fail.id).get.add)
          case malform: UpdateFailMalformedHtlc => RemoteUpdateMalform(malform, norm.commitments.remoteCommit.spec.findIncomingHtlcById(malform.id).get.add)
        }

        val commits1 = norm.commitments.receiveRevocation(revocation)
        StoreBecomeSend(norm.copy(commitments = commits1), OPEN)
        events stateUpdated lastRemoteRejects


      case (norm: DATA_NORMAL, remoteFee: UpdateFee, OPEN) =>
        val commits1 = norm.commitments.receiveFee(remoteFee)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, remote: Shutdown, OPEN) =>
        val isTheirFinalScriptPubkeyValid = Closing.isValidFinalScriptPubkey(remote.scriptPubKey)
        if (!isTheirFinalScriptPubkeyValid) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.remoteHasUnsignedOutgoingHtlcs) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.remoteHasUnsignedOutgoingUpdateFee) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.localHasUnsignedOutgoingHtlcs) BECOME(norm.copy(remoteShutdown = remote.asSome), OPEN)
        else maybeStartNegotiations(norm, remote)

      // NEGOTIATIONS

      case (negs: DATA_NEGOTIATING, remote: ClosingSigned, OPEN) =>
        val firstClosingFee = Closing.firstClosingFee(negs.commitments, negs.localShutdown.scriptPubKey, negs.remoteShutdown.scriptPubKey, LNParams.feeRatesInfo.onChainFeeConf)
        val signedClosingTx = Closing.checkClosingSignature(negs.commitments, negs.localShutdown.scriptPubKey, negs.remoteShutdown.scriptPubKey, remote.feeSatoshis, remote.signature)
        if (negs.closingTxProposed.last.lastOption.map(_.localClosingSigned.feeSatoshis).contains(remote.feeSatoshis) || negs.closingTxProposed.flatten.size >= LNParams.maxNegotiationIterations) {
          val negs1 = negs.copy(bestUnpublishedClosingTxOpt = signedClosingTx.asSome)
          handleMutualClose(signedClosingTx, negs1)
        } else {
          val lastLocalClosingFee = negs.closingTxProposed.last.lastOption.map(_.localClosingSigned.feeSatoshis)
          val nextClosingFee = Closing.nextClosingFee(localClosingFee = lastLocalClosingFee.getOrElse(firstClosingFee), remoteClosingFee = remote.feeSatoshis)
          val (closingTx, closingSignedMsg) = Closing.makeClosingTx(negs.commitments, negs.localShutdown.scriptPubKey, negs.remoteShutdown.scriptPubKey, nextClosingFee)

          val proposed = ClosingTxProposed(closingTx.tx, closingSignedMsg)
          val closingTxProposed1 = negs.closingTxProposed match { case prev :+ current => prev :+ (current :+ proposed) map identity }
          val negs1 = negs.copy(bestUnpublishedClosingTxOpt = Some(signedClosingTx), closingTxProposed = closingTxProposed1)

          if (lastLocalClosingFee contains nextClosingFee) {
            // Next computed fee is the same than the one we previously sent
            // this may happen due to rounding, anyway we can close now
            handleMutualClose(signedClosingTx, negs1)
          } else if (nextClosingFee == remote.feeSatoshis) {
            // Our next computed fee matches their, we have converged
            handleMutualClose(signedClosingTx, negs1)
            SEND(closingSignedMsg)
          } else {
            // Keep negotiating, our closing fees are different
            StoreBecomeSend(negs1, OPEN, closingSignedMsg)
          }
        }

      // OFFLINE IN PERSISTENT STATES

      case (data1: HasNormalCommitments, CMD_SOCKET_OFFLINE, WAIT_FUNDING_DONE | OPEN) =>
        val (wasUpdated, data2, localProposedAdds) = maybeRevertUnsignedOutgoing(data1)
        if (wasUpdated) StoreBecomeSend(data2, SLEEPING) else BECOME(data1, SLEEPING)
        for (add <- localProposedAdds) events localAddRejected ChannelOffline(add)


      // REESTABLISHMENT IN PERSISTENT STATES

      case (wait: DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT, CMD_SOCKET_ONLINE, SLEEPING) =>
        // There isn't much to do except asking them again to publish their current commitment by sending an error
        val error = Error(wait.channelId, "please publish your local commitment")
        BECOME(wait, CLOSING)
        SEND(error)


      case (data1: HasNormalCommitments, CMD_SOCKET_ONLINE, SLEEPING) =>
        val myCurrentPoint = data1.commitments.localParams.keys.commitmentPoint(data1.commitments.localCommit.index)
        val yourLastSecret = data1.commitments.remotePerCommitmentSecrets.lastIndex.flatMap(data1.commitments.remotePerCommitmentSecrets.getHash).getOrElse(ByteVector32.Zeroes)
        val reestablish = ChannelReestablish(data1.channelId, data1.commitments.localCommit.index + 1, data1.commitments.remoteCommit.index, PrivateKey(yourLastSecret), myCurrentPoint)
        SEND(reestablish)


      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, _: ChannelReestablish, SLEEPING) =>
        // We put back the watch (operation is idempotent) because corresponding event may have been already fired while we were in SLEEPING state
        chainWallet.watcher ! WatchConfirmed(receiver, wait.commitments.commitInput.outPoint.txid, wait.commitments.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        // Getting remote ChannelReestablish means our chain wallet is online (since we start connecting channels only after it becomes online), it makes sense to retry a funding broadcast here
        wait.fundingTx.foreach(chainWallet.wallet.commit)
        BECOME(wait, WAIT_FUNDING_DONE)


      case (wait: DATA_WAIT_FOR_FUNDING_LOCKED, _: ChannelReestablish, SLEEPING) =>
        // At this point funding tx already has a desired number of confirmations
        BECOME(wait, WAIT_FUNDING_DONE)
        SEND(wait.lastSent)


      case (norm: DATA_NORMAL, reestablish: ChannelReestablish, SLEEPING) =>
        val pleasePublishError = Error(norm.channelId, "please publish your local commitment")

        reestablish match {
          case rs if !Helpers.checkLocalCommit(norm, rs.nextRemoteRevocationNumber) =>
            // if NextRemoteRevocationNumber is greater than our local commitment index, it means that either we are using an outdated commitment, or they are lying
            // but first we need to make sure that the last PerCommitmentSecret that they claim to have received from us is correct for that NextRemoteRevocationNumber minus 1
            val peerIsAhead = norm.commitments.localParams.keys.commitmentSecret(rs.nextRemoteRevocationNumber - 1) == rs.yourLastPerCommitmentSecret
            if (peerIsAhead) StoreBecomeSend(DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(norm.commitments, rs), CLOSING, pleasePublishError)
            else throw ChannelTransitionFail(norm.commitments.channelId)

          case rs if !Helpers.checkRemoteCommit(norm, rs.nextLocalCommitmentNumber) =>
            // if NextRemoteRevocationNumber is more than one more our remote commitment index, it means that either we are using an outdated commitment, or they are lying
            // there is no way to make sure that they are saying the truth, the best thing to do is ask them to publish their commitment right now
            // note that if they don't comply, we could publish our own commitment (it is not stale, otherwise we would be in the case above)
            StoreBecomeSend(DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(norm.commitments, rs), CLOSING, pleasePublishError)

          case rs =>
            var sendQueue = Queue.empty[LightningMessage]
            if (rs.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0) {
              val nextPerCommitmentPoint = norm.commitments.localParams.keys.commitmentPoint(index = 1L)
              sendQueue :+= FundingLocked(norm.commitments.channelId, nextPerCommitmentPoint)
            }

            def resendRevocation: Unit =
              if (norm.commitments.localCommit.index == rs.nextRemoteRevocationNumber + 1) {
                val localPerCommitmentSecret = norm.commitments.localParams.keys.commitmentSecret(norm.commitments.localCommit.index - 1)
                val localNextPerCommitmentPoint = norm.commitments.localParams.keys.commitmentPoint(norm.commitments.localCommit.index + 1)
                sendQueue :+= RevokeAndAck(norm.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
              } else if (norm.commitments.localCommit.index != rs.nextRemoteRevocationNumber) {
                // Sync has failed, no sense to continue normally
                throw ChannelTransitionFail(norm.channelId)
              }

            norm.commitments.remoteNextCommitInfo match {
              case _ if norm.commitments.remoteNextCommitInfo.isRight && norm.commitments.remoteCommit.index + 1 == rs.nextLocalCommitmentNumber => resendRevocation
              case Left(waitingForRevocation) if waitingForRevocation.nextRemoteCommit.index + 1 == rs.nextLocalCommitmentNumber => resendRevocation

              case Left(waitingForRevocation) if waitingForRevocation.nextRemoteCommit.index == rs.nextLocalCommitmentNumber =>
                if (norm.commitments.localCommit.index <= waitingForRevocation.sentAfterLocalCommitIndex) resendRevocation
                (norm.commitments.localChanges.signed :+ waitingForRevocation.sent).foreach(change => sendQueue :+= change)
                if (norm.commitments.localCommit.index > waitingForRevocation.sentAfterLocalCommitIndex) resendRevocation

              case _ =>
                // Sync has failed, no sense to continue normally
                throw ChannelTransitionFail(norm.channelId)
            }

            norm.localShutdown.foreach(sendQueue :+= _)
            BECOME(data1 = norm, state1 = OPEN)
            SEND(sendQueue: _*)
        }


      case (data1: DATA_NEGOTIATING, _: ChannelReestablish, SLEEPING) if data1.commitments.localParams.isFunder =>
        // We could use the last ClosingSigned we sent, but network fees may have changed while we were offline so it is better to restart from scratch
        val (closingTx, closingSigned) = Closing.makeFirstClosingTx(data1.commitments, data1.localShutdown.scriptPubKey, data1.remoteShutdown.scriptPubKey, LNParams.feeRatesInfo.onChainFeeConf)
        StoreBecomeSend(data1.modify(_.closingTxProposed).using(_ :+ ClosingTxProposed(closingTx.tx, closingSigned).asList), OPEN, data1.localShutdown, closingSigned)


      case (data1: DATA_NEGOTIATING, _: ChannelReestablish, SLEEPING) =>
        val closingTxProposed1 = if (data1.closingTxProposed.last.isEmpty) data1.closingTxProposed else data1.closingTxProposed :+ Nil
        StoreBecomeSend(data1.copy(closingTxProposed = closingTxProposed1), OPEN, data1.localShutdown)

      // Closing phase

      case (data1: DATA_CLOSING, _: ChannelReestablish, CLOSING) =>
        val error = Error(data1.channelId, s"funding tx has been spent")
        SEND(error)


      case (closing: DATA_CLOSING, WatchEventSpent(BITCOIN_OUTPUT_SPENT, tx), CLOSING) =>
        // One of outputs in local/remote/revoked commit was spent, we should watch it gets confirmed
        chainWallet.watcher ! WatchConfirmed(receiver, tx, BITCOIN_TX_CONFIRMED(tx), LNParams.minDepthBlocks)
        // Peer might have just used a preimage on chain to claim our timeout HTLCs UTXO: consider a payment sent then
        val remoteFulfills = Closing.extractPreimages(closing.commitments.localCommit, tx).map(RemoteFulfill.tupled)
        val settledOutgoingHtlcIds = remoteFulfills.map(_.ourAdd.id)

        val rev1 = closing.revokedCommitPublished.map { revokedCommit =>
          // This might be further spend of success/timeout UTXO from an old revoked state which peer has published previously
          val (txOpt, rev1) = Closing.claimRevokedHtlcTxOutputs(closing.commitments, revokedCommit, tx, LNParams.feeRatesInfo.onChainFeeConf.feeEstimator)
          for (claimTx <- txOpt) chainWallet.watcher ! WatchSpent(receiver, tx, claimTx.txIn.filter(_.outPoint.txid == tx.txid).head.outPoint.index.toInt, BITCOIN_OUTPUT_SPENT)
          for (claimTx <- txOpt) chainWallet.watcher ! PublishAsap(claimTx)
          rev1
        }

        // Proceed as if we have normally received preimages off chain, mark related outgoing HTLCs as settled
        val closing1 = closing.modify(_.commitments.postCloseOutgoingResolvedIds).using(_ ++ settledOutgoingHtlcIds)
        StoreBecomeSend(closing1.copy(revokedCommitPublished = rev1), CLOSING)
        remoteFulfills foreach events.fulfillReceived


      case (closing: DATA_CLOSING, confirmed: BITCOIN_TX_CONFIRMED, CLOSING) =>
        val lcp1Opt = for (lcp <- closing.localCommitPublished) yield Closing.updateLocalCommitPublished(lcp, confirmed.tx)
        val rcp1Opt = for (rcp <- closing.remoteCommitPublished) yield Closing.updateRemoteCommitPublished(rcp, confirmed.tx)
        val fcp1Opt = for (fcp <- closing.futureRemoteCommitPublished) yield Closing.updateRemoteCommitPublished(fcp, confirmed.tx)
        val rcp1NextOpt = for (rcp <- closing.nextRemoteCommitPublished) yield Closing.updateRemoteCommitPublished(rcp, confirmed.tx)
        val revCp1Opt = for (revokedCp <- closing.revokedCommitPublished) yield Closing.updateRevokedCommitPublished(revokedCp, confirmed.tx)
        val closing1 = DATA_CLOSING(closing.commitments, closing.waitingSince, closing.mutualCloseProposed, closing.mutualCloseProposed, lcp1Opt, rcp1Opt, rcp1NextOpt, fcp1Opt, revCp1Opt)
        val overRiddenHtlcs = Closing.overriddenOutgoingHtlcs(closing1, confirmed.tx)

        val format = closing1.commitments.channelVersion.commitmentFormat
        val timedOutHtlcs = Closing.isClosingTypeAlreadyKnown(closing1) match {
          case Some(c: Closing.LocalClose) => Closing.timedoutHtlcs(format, c.localCommit, c.localCommitPublished, closing1.commitments.localParams.dustLimit, confirmed.tx)
          case Some(c: Closing.RemoteClose) => Closing.timedoutHtlcs(format, c.remoteCommit, c.remoteCommitPublished, closing1.commitments.remoteParams.dustLimit, confirmed.tx)
          case Some(_: Closing.MutualClose | _: Closing.RecoveryClose | _: Closing.RevokedClose) => Set.empty[UpdateAddHtlc] // We can not have pending HTLCs in these states
          case None => Set.empty[UpdateAddHtlc] // Keep waiting
        }

        // Update state and notify system about failed HTLCs
        val settledOutgoingHtlcIds = (overRiddenHtlcs ++ timedOutHtlcs).map(_.id)
        StoreBecomeSend(closing1.modify(_.commitments.postCloseOutgoingResolvedIds).using(_ ++ settledOutgoingHtlcIds), CLOSING)
        for (add <- overRiddenHtlcs ++ timedOutHtlcs) events localAddRejected InPrincipleNotSendable(localAdd = add)

        Helpers.chainFeePaid(confirmed.tx, closing1).foreach { chainFee =>
          // Record a chain tx fee we have paid if fee can be defined
          bag.addChannelTxFee(chainFee, confirmed.tx.txid)
        }

        // Finally remove a channel from db if it's closed (will disappear on next restart)
        val isClosed = Closing.isClosed(closing1, additionalConfirmedTxOpt = confirmed.tx.asSome)
        if (isClosed) closing1.commitments.updateOpt.map(_.shortChannelId).foreach(bag.rmHtlcInfos)
        if (isClosed) bag.delete(closing1.channelId)


      case (closing: DATA_CLOSING, cmd: CMD_FULFILL_HTLC, CLOSING) if !closing.commitments.alreadyReplied(cmd.theirAdd.id) =>
        // We get a preimage when channel is already closed, so we need to try to redeem payments on chain
        val (commits1, ourFulfillMsg) = closing.commitments.sendFulfill(cmd)
        val conf = LNParams.feeRatesInfo.onChainFeeConf

        val lcp1Opt = for (lcp <- closing.localCommitPublished) yield Closing.claimCurrentLocalCommitTxOutputs(commits1, lcp.commitTx, conf)
        val rcp1Opt = for (rcp <- closing.remoteCommitPublished) yield Closing.claimRemoteCommitTxOutputs(commits1, commits1.remoteCommit, rcp.commitTx, conf.feeEstimator)

        val rcp1NextOpt = for {
          rcp <- closing.nextRemoteCommitPublished
          nextRemoteCommit <- commits1.remoteNextCommitInfo.left.toOption.map(_.nextRemoteCommit)
        } yield Closing.claimRemoteCommitTxOutputs(commits1, nextRemoteCommit, rcp.commitTx, conf.feeEstimator)

        // Store an updated closing data so this case won't be executed again, publish our chain redeem txs and send preimage off-chain to give them one more chance to obtain it
        val closing1 = closing.copy(commitments = commits1, localCommitPublished = lcp1Opt, remoteCommitPublished = rcp1Opt, nextRemoteCommitPublished = rcp1NextOpt)
        StoreBecomeSend(closing1, CLOSING, ourFulfillMsg)
        rcp1NextOpt.foreach(doPublish)
        rcp1Opt.foreach(doPublish)
        lcp1Opt.foreach(doPublish)


      // RESTORING FROM STORED DATA

      case (null, data1: DATA_CLOSING, -1) =>
        data1.mutualClosePublished.foreach(doPublish)
        data1.localCommitPublished.foreach(doPublish)
        data1.remoteCommitPublished.foreach(doPublish)
        data1.revokedCommitPublished.foreach(doPublish)
        data1.nextRemoteCommitPublished.foreach(doPublish)
        data1.futureRemoteCommitPublished.foreach(doPublish)
        BECOME(data1, CLOSING)


      case (null, normalData: HasNormalCommitments, -1) =>
        val commitInput: Transactions.InputInfo = normalData.commitments.commitInput
        chainWallet.watcher ! WatchSpent(receiver, commitInput.outPoint.txid, commitInput.outPoint.index.toInt, commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
        chainWallet.watcher ! WatchConfirmed(receiver, commitInput.outPoint.txid, commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        BECOME(normalData, SLEEPING)


      case _ =>
    }

  def nextFeerate(norm: DATA_NORMAL, threshold: Double): Option[FeeratePerKw] =
    newFeerate(LNParams.feeRatesInfo, norm.commitments.localCommit.spec, threshold)

  private def maybeRevertUnsignedOutgoing(data1: HasNormalCommitments) =
    if (data1.commitments.localHasUnsignedOutgoingHtlcs || data1.commitments.remoteHasUnsignedOutgoingHtlcs) {
      val remoteProposed = data1.commitments.remoteChanges.proposed.collect { case remoteUpdate: UpdateAddHtlc => remoteUpdate }
      val localProposed = data1.commitments.localChanges.proposed.collect { case localUpdate: UpdateAddHtlc => localUpdate }
      val data2 = data1.modifyAll(_.commitments.remoteChanges.proposed, _.commitments.localChanges.proposed).setTo(Nil)
      val data3 = data2.modify(_.commitments.remoteNextHtlcId).using(_ - remoteProposed.size)
      val data4 = data3.modify(_.commitments.localNextHtlcId).using(_ - localProposed.size)
      (true, data4, localProposed)
    } else (false, data1, Nil)

  private def handleChannelForceClosing(prev: HasNormalCommitments)(turnIntoClosing: HasNormalCommitments => DATA_CLOSING): Unit = {
    val (_, closing1: DATA_CLOSING, localProposedAdds) = turnIntoClosing.andThen(maybeRevertUnsignedOutgoing)(prev)
    StoreBecomeSend(closing1, CLOSING)

    // Unsigned outgoing HTLCs should be failed right away on any force-closing
    for (add <- localProposedAdds) events localAddRejected ChannelNotAbleToSend(add)
    // In case if force-closing happens when we have a cross-signed mutual tx
    closing1.mutualClosePublished.foreach(doPublish)
  }

  private def startNegotiationsAsFunder(data1: DATA_NORMAL, local: Shutdown, remote: Shutdown): Unit = {
    val (closingTx, closingSigned) = Closing.makeFirstClosingTx(data1.commitments, local.scriptPubKey, remote.scriptPubKey, LNParams.feeRatesInfo.onChainFeeConf)
    val data2 = DATA_NEGOTIATING(data1.commitments, local, remote, List(ClosingTxProposed(closingTx.tx, closingSigned) :: Nil), bestUnpublishedClosingTxOpt = None)
    StoreBecomeSend(data2, OPEN, local, closingSigned)
  }

  private def maybeStartNegotiations(data1: DATA_NORMAL, remote: Shutdown): Unit = {
    val local = data1.localShutdown getOrElse Shutdown(data1.channelId, data1.commitments.localParams.defaultFinalScriptPubKey)
    if (data1.commitments.hasPendingHtlcsOrFeeUpdate) StoreBecomeSend(data1.copy(localShutdown = local.asSome, remoteShutdown = remote.asSome), OPEN, local)
    else if (!data1.commitments.localParams.isFunder) StoreBecomeSend(DATA_NEGOTIATING(data1.commitments, local, remote), OPEN, local)
    else startNegotiationsAsFunder(data1, local, remote)
  }

  private def handleMutualClose(closingTx: Transaction, data1: DATA_NEGOTIATING): Unit = {
    val data2 = data1.toClosed.copy(mutualClosePublished = closingTx :: Nil)
    BECOME(STORE(data2), CLOSING)
    doPublish(closingTx)
  }

  private def handleMutualClose(closingTx: Transaction, data1: DATA_CLOSING): Unit = {
    val data2 = data1.copy(mutualClosePublished = data1.mutualClosePublished :+ closingTx)
    BECOME(STORE(data2), CLOSING)
    doPublish(closingTx)
  }

  private def spendLocalCurrent(data1: HasNormalCommitments): Unit = data1 match {
    case alreadyClosing: DATA_CLOSING if alreadyClosing.futureRemoteCommitPublished.isDefined =>
    case _: DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT =>

    case _ =>
      val commitTx = data1.commitments.localCommit.publishableTxs.commitTx.tx
      val lcp = Closing.claimCurrentLocalCommitTxOutputs(data1.commitments, commitTx, LNParams.feeRatesInfo.onChainFeeConf)

      handleChannelForceClosing(data1) {
        case some: DATA_CLOSING => some.copy(localCommitPublished = lcp.asSome)
        case some: DATA_NEGOTIATING => some.toClosed.copy(localCommitPublished = lcp.asSome)
        case _ => DATA_CLOSING(data1.commitments, System.currentTimeMillis, localCommitPublished = lcp.asSome)
      }

      doPublish(lcp)
  }

  private def handleRemoteSpentCurrent(commitTx: Transaction, data1: HasNormalCommitments): Unit = {
    val rcp = Closing.claimRemoteCommitTxOutputs(data1.commitments, data1.commitments.remoteCommit, commitTx, LNParams.feeRatesInfo.onChainFeeConf.feeEstimator)

    handleChannelForceClosing(data1) {
      case some: DATA_CLOSING => some.copy(remoteCommitPublished = rcp.asSome)
      case some: DATA_NEGOTIATING => some.toClosed.copy(remoteCommitPublished = rcp.asSome)
      case some: DATA_WAIT_FOR_FUNDING_CONFIRMED => DATA_CLOSING(some.commitments, System.currentTimeMillis, remoteCommitPublished = rcp.asSome)
      case _ => DATA_CLOSING(data1.commitments, System.currentTimeMillis, remoteCommitPublished = rcp.asSome)
    }

    doPublish(rcp)
  }

  private def handleRemoteSpentNext(commitTx: Transaction, data1: HasNormalCommitments): Unit = {
    val nextRemoteCommit: RemoteCommit = data1.commitments.remoteNextCommitInfo.left.get.nextRemoteCommit
    val rcp = Closing.claimRemoteCommitTxOutputs(data1.commitments, nextRemoteCommit, commitTx, LNParams.feeRatesInfo.onChainFeeConf.feeEstimator)

    handleChannelForceClosing(data1) {
      case some: DATA_CLOSING => some.copy(nextRemoteCommitPublished = rcp.asSome)
      case some: DATA_NEGOTIATING => some.toClosed.copy(nextRemoteCommitPublished = rcp.asSome)
      case _ => DATA_CLOSING(data1.commitments, System.currentTimeMillis, nextRemoteCommitPublished = rcp.asSome)
    }

    doPublish(rcp)
  }

  private def handleRemoteSpentFuture(commitTx: Transaction, commits: NormalCommits): Unit = {
    val remoteCommitPublished = RemoteCommitPublished(commitTx, claimMainOutputTx = None, claimHtlcSuccessTxs = Nil, claimHtlcTimeoutTxs = Nil)
    val closing = DATA_CLOSING(commits, System.currentTimeMillis, futureRemoteCommitPublished = remoteCommitPublished.asSome)
    StoreBecomeSend(closing, CLOSING)
  }

  private def handleRemoteSpentOther(tx: Transaction, data1: HasNormalCommitments): Unit =
    Closing.claimRevokedRemoteCommitTxOutputs(data1.commitments, tx, bag, LNParams.feeRatesInfo.onChainFeeConf.feeEstimator) match {
      // This is most likely an old revoked state, but it might not be in some kind of exceptional circumstance (private keys leakage, old backup etc)

      case Some(revCp) =>
        handleChannelForceClosing(data1) {
          case some: DATA_CLOSING => some.copy(revokedCommitPublished = some.revokedCommitPublished :+ revCp)
          case _ => DATA_CLOSING(data1.commitments, System.currentTimeMillis, revokedCommitPublished = revCp :: Nil)
        }

        doPublish(revCp)

      case None =>
        // It is dagerous to publish our commit here (for example when we restore state from an old backup)
        // thanks to remote static key we get the rest of channel balance back anyway so it's not too bad
        handleRemoteSpentFuture(tx, data1.commitments)
    }

  // Publish handlers

  private def doPublish(closingTx: Transaction): Unit = {
    val replyEvent: BitcoinEvent = BITCOIN_TX_CONFIRMED(closingTx)
    chainWallet.watcher ! WatchConfirmed(receiver, closingTx, replyEvent, LNParams.minDepthBlocks)
    chainWallet.watcher ! PublishAsap(closingTx)
  }

  private def publishIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(PublishAsap).foreach(event => chainWallet.watcher ! event)

  // Watch utxos only we can spend to get basically resolved
  private def watchConfirmedIfNeeded(txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(BITCOIN_TX_CONFIRMED).foreach { replyEvent =>
      chainWallet.watcher ! WatchConfirmed(receiver, replyEvent.tx, replyEvent, LNParams.minDepthBlocks)
    }

  // Watch utxos that both we and peer can spend to get triggered (spent, but not confirmed yet)
  private def watchSpentIfNeeded(parentTx: Transaction, txes: Iterable[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty): Unit =
    txes.filterNot(Closing inputsAlreadySpent irrevocablySpent).map(_.txIn.head.outPoint.index.toInt).foreach { outPointIndex =>
      chainWallet.watcher ! WatchSpent(receiver, parentTx, outPointIndex, BITCOIN_OUTPUT_SPENT)
    }

  private def doPublish(lcp: LocalCommitPublished): Unit = {
    publishIfNeeded(List(lcp.commitTx) ++ lcp.claimMainDelayedOutputTx ++ lcp.htlcSuccessTxs ++ lcp.htlcTimeoutTxs ++ lcp.claimHtlcDelayedTxs, lcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(lcp.commitTx) ++ lcp.claimMainDelayedOutputTx ++ lcp.claimHtlcDelayedTxs, lcp.irrevocablySpent)
    watchSpentIfNeeded(lcp.commitTx, lcp.htlcSuccessTxs ++ lcp.htlcTimeoutTxs, lcp.irrevocablySpent)
  }

  private def doPublish(rcp: RemoteCommitPublished): Unit = {
    publishIfNeeded(rcp.claimMainOutputTx ++ rcp.claimHtlcSuccessTxs ++ rcp.claimHtlcTimeoutTxs, rcp.irrevocablySpent)
    watchSpentIfNeeded(rcp.commitTx, rcp.claimHtlcTimeoutTxs ++ rcp.claimHtlcSuccessTxs, rcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(rcp.commitTx) ++ rcp.claimMainOutputTx, rcp.irrevocablySpent)
  }

  private def doPublish(rcp: RevokedCommitPublished): Unit = {
    publishIfNeeded(rcp.claimMainOutputTx ++ rcp.mainPenaltyTx ++ rcp.htlcPenaltyTxs ++ rcp.claimHtlcDelayedPenaltyTxs, rcp.irrevocablySpent)
    watchSpentIfNeeded(rcp.commitTx, rcp.mainPenaltyTx ++ rcp.htlcPenaltyTxs, rcp.irrevocablySpent)
    watchConfirmedIfNeeded(List(rcp.commitTx) ++ rcp.claimMainOutputTx, rcp.irrevocablySpent)
  }
}
