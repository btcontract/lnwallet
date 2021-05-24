package immortan

import fr.acinq.eclair._
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.blockchain._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._

import akka.actor.{ActorRef, Props}
import fr.acinq.bitcoin.{ByteVector32, ScriptFlags, Transaction}
import fr.acinq.eclair.transactions.Transactions.TxOwner
import fr.acinq.eclair.channel.Helpers.Closing
import fr.acinq.eclair.payment.OutgoingPacket
import fr.acinq.bitcoin.Crypto.PrivateKey
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

abstract class ChannelNormal(bag: ChannelBag) extends Channel with Handlers { me =>
  val receiver: ActorRef = LNParams.system actorOf Props(new ActorEventsReceiver)
  val chainWallet: WalletExt

  def doProcess(change: Any): Unit =
    Tuple3(data, change, state) match {

      // OPENING PHASE: FUNDER FLOW

      case (null, init: INPUT_INIT_FUNDER, null) =>
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
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFunder.remoteInfo, wait.initFunder.channelVersion,
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
        val commits = NormalCommits(wait.channelVersion, wait.remoteInfo, wait.localParams, wait.remoteParams, wait.channelFlags,
          LocalCommit(index = 0L, wait.localSpec, publishableTxs), wait.remoteCommit, LocalChanges(Nil, Nil, Nil), RemoteChanges(Nil, Nil, Nil),
          localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(randomKey.publicKey), signedLocalCommitTx.input, ShaChain.init, wait.channelId)

        chainWallet.watcher ! WatchSpent(receiver, commits.commitInput.outPoint.txid, commits.commitInput.outPoint.index.toInt, commits.commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
        chainWallet.watcher ! WatchConfirmed(receiver, commits.commitInput.outPoint.txid, commits.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        // Persist a channel unconditionally, try to re-publish a funding tx on restart unconditionally (don't react to commit=false, we can't trust remote servers on this)
        StoreBecomeSend(DATA_WAIT_FOR_FUNDING_CONFIRMED(commits, wait.fundingTx.toSome, System.currentTimeMillis, Left(wait.lastSent), deferred = None), WAIT_FUNDING_DONE)
        chainWallet.wallet.commit(wait.fundingTx)

      // OPENING PHASE: FUNDEE FLOW

      case (null, init: INPUT_INIT_FUNDEE, null) =>
        val emptyUpfrontShutdown: TlvStream[AcceptChannelTlv] = TlvStream(ChannelTlv UpfrontShutdownScript ByteVector.empty)
        val accept = AcceptChannel(init.theirOpen.temporaryChannelId, init.localParams.dustLimit, init.localParams.maxHtlcValueInFlightMsat, init.localParams.channelReserve,
          init.localParams.htlcMinimum, LNParams.minDepthBlocks, init.localParams.toSelfDelay, init.localParams.maxAcceptedHtlcs, init.localParams.keys.fundingKey.publicKey,
          init.localParams.keys.revocationKey.publicKey, init.localParams.walletStaticPaymentBasepoint, init.localParams.keys.delayedPaymentKey.publicKey,
          init.localParams.keys.htlcKey.publicKey, init.localParams.keys.commitmentPoint(index = 0L), emptyUpfrontShutdown)

        val remoteParams = RemoteParams(init.theirOpen.dustLimitSatoshis, init.theirOpen.maxHtlcValueInFlightMsat,
          init.theirOpen.channelReserveSatoshis, init.theirOpen.htlcMinimumMsat, init.theirOpen.toSelfDelay, init.theirOpen.maxAcceptedHtlcs,
          init.theirOpen.fundingPubkey, init.theirOpen.revocationBasepoint, init.theirOpen.paymentBasepoint, init.theirOpen.delayedPaymentBasepoint,
          init.theirOpen.htlcBasepoint)

        Helpers.validateParamsFundee(LNParams.ourInit.features, init.theirOpen, LNParams.feeRatesInfo.onChainFeeConf)
        val data1 = DATA_WAIT_FOR_FUNDING_CREATED(init, remoteParams, accept)
        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(accept)


      case (wait: DATA_WAIT_FOR_FUNDING_CREATED, created: FundingCreated, WAIT_FOR_ACCEPT) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFundee.remoteInfo, wait.initFundee.channelVersion,
          wait.initFundee.localParams, wait.remoteParams, wait.initFundee.theirOpen.fundingSatoshis, wait.initFundee.theirOpen.pushMsat, wait.initFundee.theirOpen.feeratePerKw,
          created.fundingTxid, created.fundingOutputIndex, wait.initFundee.theirOpen.firstPerCommitmentPoint)

        val localSigOfLocalTx = Transactions.sign(localCommitTx, wait.initFundee.localParams.keys.fundingKey.privateKey, TxOwner.Local, wait.initFundee.channelVersion.commitmentFormat)
        val signedLocalCommitTx = Transactions.addSigs(localCommitTx, wait.initFundee.localParams.keys.fundingKey.publicKey, wait.remoteParams.fundingPubKey, localSigOfLocalTx, created.signature)
        val localSigOfRemoteTx = Transactions.sign(remoteCommitTx, wait.initFundee.localParams.keys.fundingKey.privateKey, TxOwner.Remote, wait.initFundee.channelVersion.commitmentFormat)
        val fundingSigned = FundingSigned(channelId = toLongId(created.fundingTxid, created.fundingOutputIndex), signature = localSigOfRemoteTx)

        // Make sure their supplied signature is correct before proceeding
        require(Transactions.checkSpendable(signedLocalCommitTx).isSuccess)

        val publishableTxs = PublishableTxs(signedLocalCommitTx, Nil)
        val remoteCommit = RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, remotePerCommitmentPoint = wait.initFundee.theirOpen.firstPerCommitmentPoint)
        val commits = NormalCommits(wait.initFundee.channelVersion, wait.initFundee.remoteInfo, wait.initFundee.localParams, wait.remoteParams, wait.initFundee.theirOpen.channelFlags,
          LocalCommit(index = 0L, localSpec, publishableTxs), remoteCommit, LocalChanges(Nil, Nil, Nil), RemoteChanges(Nil, Nil, Nil), localNextHtlcId = 0L, remoteNextHtlcId = 0L,
          remoteNextCommitInfo = Right(randomKey.publicKey), signedLocalCommitTx.input, ShaChain.init, fundingSigned.channelId)

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

        if (Try(correct).isFailure) StoreBecomeSend(DATA_CLOSING(wait.commitments, wait.fundingTx), CLOSING) else {
          val shortChannelId = ShortChannelId(event.blockHeight, event.txIndex, wait.commitments.commitInput.outPoint.index.toInt)
          val fundingLocked = FundingLocked(nextPerCommitmentPoint = wait.commitments.localParams.keys.commitmentPoint(1L), channelId = wait.channelId)
          StoreBecomeSend(DATA_WAIT_FOR_FUNDING_LOCKED(wait.commitments, shortChannelId, fundingLocked), WAIT_FUNDING_DONE, fundingLocked)
          wait.deferred.foreach(process)
        }


      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, locked: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their message, they will re-send if we get disconnected
        BECOME(wait.copy(deferred = locked.toSome), WAIT_FUNDING_DONE)


      case (wait: DATA_WAIT_FOR_FUNDING_LOCKED, locked: FundingLocked, WAIT_FUNDING_DONE) =>
        val commits1 = wait.commitments.modify(_.remoteNextCommitInfo) setTo Right(locked.nextPerCommitmentPoint)
        StoreBecomeSend(DATA_NORMAL(commits1, wait.shortChannelId), OPEN)

      // MAIN LOOP

      case (some: HasNormalCommitments, remoteInfo: RemoteNodeInfo, OPEN | SLEEPING) if some.commitments.remoteInfo.nodeId == remoteInfo.nodeId =>
        val data1 = some.modify(_.commitments.remoteInfo).setTo(remoteInfo)
        data = STORE(data1)


      case (norm: DATA_NORMAL, update: ChannelUpdate, OPEN | SLEEPING) if update.shortChannelId == norm.shortChannelId =>
        val data1 = norm.modify(_.commitments.updateOpt).setTo(update.toSome)
        data = STORE(data1)


      case (norm: DATA_NORMAL, CMD_CHECK_FEERATE, OPEN) =>
        // Current feerates are supposed to be updated before this command is received
        feeUpdateRequired(norm.commitments, LNParams.feeRatesInfo.current).foreach(process)


      case (norm: DATA_NORMAL, cmd: CMD_UPDATE_FEERATE, OPEN) if norm.commitments.localParams.isFunder =>
        // Unconditionally update feerates here, check whether this is needed was done at earlier stage
        val (commits1, reserve) = norm.commitments.sendFee(cmd.ourFeeRatesUpdate)

        if (reserve.toLong > 0L) {
          // Only send update if we can afford it
          BECOME(norm.copy(commitments = commits1), OPEN)
          SEND(cmd.ourFeeRatesUpdate)
          doProcess(CMD_SIGN)
        }


      // We may schedule shutdown while channel is offline
      case (norm: DATA_NORMAL, cmd: CMD_CLOSE, OPEN | SLEEPING) =>
        val localScriptPubKey = cmd.scriptPubKey.getOrElse(norm.commitments.localParams.defaultFinalScriptPubKey)
        val isValidFinalScriptPubkey = Helpers.Closing.isValidFinalScriptPubkey(localScriptPubKey)
        val hasLocalHasUnsignedOutgoingHtlcs = norm.commitments.localHasUnsignedOutgoingHtlcs
        val shutdown = Shutdown(norm.channelId, localScriptPubKey)

        if (!isValidFinalScriptPubkey) throw CMDException(new RuntimeException, cmd)
        else if (norm.localShutdown.isDefined) throw CMDException(new RuntimeException, cmd)
        else if (hasLocalHasUnsignedOutgoingHtlcs) throw CMDException(new RuntimeException, cmd)
        else StoreBecomeSend(norm.copy(localShutdown = shutdown.toSome), state, shutdown)


      case (norm: DATA_NORMAL, cmd: CMD_ADD_HTLC, _) =>
        if (SLEEPING == state) throw CMDException(ChannelOffline, cmd)
        if (OPEN != state) throw CMDException(new RuntimeException, cmd)
        if (norm.localShutdown.nonEmpty || norm.remoteShutdown.nonEmpty) throw CMDException(new RuntimeException, cmd)
        val (commits1, updateAddHtlcMsg) = norm.commitments.sendAdd(cmd, LNParams.blockCount.get, LNParams.feeRatesInfo.onChainFeeConf)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(updateAddHtlcMsg)
        doProcess(CMD_SIGN)


      case (_, cmd: CMD_ADD_HTLC, _) =>
        // Omit this channel in any other state
        throw CMDException(new RuntimeException, cmd)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (norm: DATA_NORMAL, cmd: CMD_FULFILL_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFulfillHtlc(norm.channelId, cmd.theirAdd.id, cmd.preimage)
        val commits1 = norm.commitments.addLocalProposal(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(msg)


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


      // We have nothing to sign so check for valid shutdown state, only consider when we have nothing in-flight
      case (norm: DATA_NORMAL, CMD_SIGN, OPEN) if norm.remoteShutdown.isDefined && !norm.commitments.localHasUnsignedOutgoingHtlcs =>
        val (data1, replies) = maybeStartNegotiations(norm, norm.remoteShutdown.get, LNParams.feeRatesInfo.onChainFeeConf)
        StoreBecomeSend(data1, OPEN, replies:_*)


      case (norm: DATA_NORMAL, add: UpdateAddHtlc, OPEN) =>
        val commits1 = norm.commitments.receiveAdd(add, LNParams.feeRatesInfo.onChainFeeConf)
        val theirAdd = UpdateAddHtlcExt(add, norm.commitments.remoteInfo)
        BECOME(norm.copy(commitments = commits1), OPEN)
        events.addReceived(theirAdd)


      case (norm: DATA_NORMAL, msg: UpdateFulfillHtlc, OPEN) =>
        val (commits1, ourAdd) = norm.commitments.receiveFulfill(msg)
        val fulfill = RemoteFulfill(ourAdd, msg.paymentPreimage)
        // First persist a new state, then call an event
        BECOME(norm.copy(commitments = commits1), OPEN)
        events.fulfillReceived(fulfill)


      case (norm: DATA_NORMAL, fail: UpdateFailHtlc, OPEN) =>
        val commits1 = norm.commitments.receiveFail(fail)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, malformed: UpdateFailMalformedHtlc, OPEN) =>
        val commits1 = norm.commitments.receiveFailMalformed(malformed)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, commitSig: CommitSig, OPEN) =>
        val (commits1, revocation) = norm.commitments.receiveCommit(commitSig)
        StoreBecomeSend(norm.copy(commitments = commits1), OPEN, revocation)
        // We may have fulfilled some incoming HTLCs, check feerate again
        process(CMD_CHECK_FEERATE)
        doProcess(CMD_SIGN)


      case (norm: DATA_NORMAL, revocation: RevokeAndAck, OPEN) =>
        val commits1 = norm.commitments.receiveRevocation(revocation)
        val lastRemoteRejects: Seq[RemoteReject] = norm.commitments.remoteChanges.signed.collect {
          case fail: UpdateFailHtlc => RemoteUpdateFail(fail, norm.commitments.remoteCommit.spec.findIncomingHtlcById(fail.id).get.add)
          case malform: UpdateFailMalformedHtlc => RemoteUpdateMalform(malform, norm.commitments.remoteCommit.spec.findIncomingHtlcById(malform.id).get.add)
        }

        StoreBecomeSend(norm.copy(commitments = commits1), OPEN)
        events.stateUpdated(lastRemoteRejects)


      case (norm: DATA_NORMAL, remoteFee: UpdateFee, OPEN) =>
        val commits1 = norm.commitments.receiveFee(remoteFee, LNParams.feeRatesInfo.onChainFeeConf)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, remote: Shutdown, OPEN) =>
        val isTheirFinalScriptPubkeyValid = Closing.isValidFinalScriptPubkey(remote.scriptPubKey)
        if (!isTheirFinalScriptPubkeyValid) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.remoteHasUnsignedOutgoingHtlcs) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.remoteHasUnsignedOutgoingUpdateFee) throw ChannelTransitionFail(norm.commitments.channelId)
        if (norm.commitments.localHasUnsignedOutgoingHtlcs) BECOME(norm.copy(remoteShutdown = remote.toSome), OPEN) else {
          val (data1, replies) = maybeStartNegotiations(norm, remote, LNParams.feeRatesInfo.onChainFeeConf)
          StoreBecomeSend(data1, OPEN, replies:_*)
        }

      // NEGOTIATIONS

      case (negs: DATA_NEGOTIATING, remote: ClosingSigned, OPEN) =>
        handleNegotiations(negs, remote, LNParams.feeRatesInfo.onChainFeeConf)

      // RESTORING FROM STORED DATA

      case (null, normalData: HasNormalCommitments, null) =>
        val commitInput: Transactions.InputInfo = normalData.commitments.commitInput
        chainWallet.watcher ! WatchSpent(receiver, commitInput.outPoint.txid, commitInput.outPoint.index.toInt, commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
        chainWallet.watcher ! WatchConfirmed(receiver, commitInput.outPoint.txid, commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)

        normalData match {
          case data1: DATA_CLOSING =>
            BECOME(data1, state1 = CLOSING)
            Helpers.Closing.isClosingTypeAlreadyKnown(data1) match {
              case Some(close: Helpers.Closing.MutualClose) => doPublish(close.tx)
              case Some(close: Helpers.Closing.LocalClose) => doPublish(close.localCommitPublished)
              case Some(close: Helpers.Closing.RemoteClose) => doPublish(close.remoteCommitPublished)
              case Some(close: Helpers.Closing.RecoveryClose) => doPublish(close.remoteCommitPublished)
              case Some(close: Helpers.Closing.RevokedClose) => doPublish(close.revokedCommitPublished)

              case None =>
                for (close <- data1.mutualClosePublished) doPublish(close)
                for (close <- data1.localCommitPublished) doPublish(close)
                for (close <- data1.remoteCommitPublished) doPublish(close)
                for (close <- data1.revokedCommitPublished) doPublish(close)
                for (close <- data1.nextRemoteCommitPublished) doPublish(close)
                for (close <- data1.futureRemoteCommitPublished) doPublish(close)
            }

          case data1 =>
            BECOME(data1, SLEEPING)
        }

      // OFFLINE IN PERSISTENT STATES

      case (_, CMD_SOCKET_OFFLINE, WAIT_FUNDING_DONE | OPEN) => BECOME(data, SLEEPING)

      // REESTABLISHMENT IN PERSISTENT STATES

      case (wait: DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT, CMD_SOCKET_ONLINE, SLEEPING) =>
        // There isn't much to do except asking them again to publish their current commitment by sending an error
        val error = Error(wait.channelId, "please publish your local commitment")
        BECOME(wait, CLOSING)
        SEND(error)


      case (data1: HasNormalCommitments, CMD_SOCKET_ONLINE, SLEEPING) =>
        val myCurrentPerCommitmentPoint = data1.commitments.localParams.keys.commitmentPoint(data1.commitments.localCommit.index)
        val yourLastPerCommitmentSecret = data1.commitments.remotePerCommitmentSecrets.lastIndex.flatMap(data1.commitments.remotePerCommitmentSecrets.getHash).getOrElse(ByteVector32.Zeroes)
        val reestablish = ChannelReestablish(data1.channelId, data1.commitments.localCommit.index + 1, data1.commitments.remoteCommit.index, PrivateKey(yourLastPerCommitmentSecret), myCurrentPerCommitmentPoint)
        SEND(reestablish)


      case (data1: DATA_CLOSING, _: ChannelReestablish, CLOSING) =>
        val error = Error(data1.channelId, s"funding tx has been spent")
        SEND(error)


      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, _: ChannelReestablish, SLEEPING) =>
        // We put back the watch (operation is idempotent) because corresponding event may have been already fired while we were in SLEEPING state
        chainWallet.watcher ! WatchConfirmed(receiver, wait.commitments.commitInput.outPoint.txid, wait.commitments.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        // Getting remote ChannelReestablish means our chain wallet is online (since we start connecting channels only after it becomes online), it makes sense to retry a funding broadcast here
        wait.fundingTx.foreach(chainWallet.wallet.commit)
        BECOME(wait, WAIT_FUNDING_DONE)


      case (wait: DATA_WAIT_FOR_FUNDING_LOCKED, _: ChannelReestablish, SLEEPING) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        SEND(wait.lastSent)


      case (closing: DATA_CLOSING, WatchEventSpent(BITCOIN_OUTPUT_SPENT, tx), CLOSING) =>
        // Peer has just used a preimage on chain to claim our outgoing payment's UTXO, payment is sent
        chainWallet.watcher ! WatchConfirmed(receiver, tx, LNParams.minDepthBlocks, BITCOIN_TX_CONFIRMED(tx))
        val remoteFulfills = Helpers.Closing.extractPreimages(closing.commitments.localCommit, tx).map(RemoteFulfill.tupled)

        val rev1 = closing.revokedCommitPublished.map { rev =>
          // This might be an old revoked state which is a violation of contract and allows us to take a whole channel balance right away
          val (rev1, txOpt) = Helpers.Closing.claimRevokedHtlcTxOutputs(closing.commitments, rev, tx, LNParams.feeRatesInfo.onChainFeeConf.feeEstimator)
          for (claimTx <- txOpt) chainWallet.watcher ! WatchSpent(receiver, tx, claimTx.txIn.filter(_.outPoint.txid == tx.txid).head.outPoint.index.toInt, BITCOIN_OUTPUT_SPENT)
          for (claimTx <- txOpt) chainWallet.watcher ! PublishAsap(claimTx)
          rev1
        }

        // First persist a new state, then call an event
        StoreBecomeSend(closing.copy(revokedCommitPublished = rev1), CLOSING)
        // Proceed as if we have normally received preimages off chain
        remoteFulfills.foreach(events.fulfillReceived)


      case (data1: DATA_NORMAL, reestablish: ChannelReestablish, SLEEPING) => handleNormalSync(data1, reestablish)
      case (data1: DATA_NEGOTIATING, _: ChannelReestablish, SLEEPING) => handleNegotiationsSync(data1, LNParams.feeRatesInfo.onChainFeeConf)
      case _ =>
    }

  def feeUpdateRequired(commits: NormalCommits, rates: CurrentFeerates): Option[CMD_UPDATE_FEERATE] = {
    val networkFeeratePerKw = LNParams.feeRatesInfo.onChainFeeConf.getCommitmentFeerate(commits.channelVersion, rates.toSome)
    val shouldUpdate = LNParams.feeRatesInfo.onChainFeeConf.shouldUpdateFee(commits.localCommit.spec.feeratePerKw, networkFeeratePerKw)
    if (commits.localParams.isFunder && shouldUpdate) CMD_UPDATE_FEERATE(commits.channelId, networkFeeratePerKw).toSome else None
  }
}
