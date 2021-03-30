package immortan

import fr.acinq.eclair._
import immortan.Channel._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.blockchain._
import com.softwaremill.quicklens._
import fr.acinq.eclair.transactions._

import scala.util.{Success, Try}
import akka.actor.{ActorRef, Props}
import fr.acinq.bitcoin.{ByteVector32, Script, ScriptFlags, Transaction}
import fr.acinq.eclair.transactions.Transactions.TxOwner
import fr.acinq.eclair.payment.OutgoingPacket
import fr.acinq.eclair.router.Announcements
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.eclair.crypto.ShaChain
import scodec.bits.ByteVector
import immortan.utils.Rx


object ChannelNormal {
  def make(initListeners: Set[ChannelListener], normalData: HasNormalCommitments, cw: WalletExt, bag: ChannelBag): ChannelNormal = new ChannelNormal(bag) {
    def SEND(messages: LightningMessage*): Unit = CommsTower.sendMany(messages, normalData.commitments.remoteInfo.nodeSpecificPair)
    def STORE(normalData1: PersistentChannelData): PersistentChannelData = bag.put(normalData1)
    var chainWallet: WalletExt = cw
    listeners = initListeners
    doProcess(normalData)
  }
}

abstract class ChannelNormal(bag: ChannelBag) extends Channel with Handlers { me =>
  val receiver: ActorRef = LNParams.system actorOf Props(new Receiver)
  var chainWallet: WalletExt

  def doProcess(change: Any): Unit =
    Tuple3(data, change, state) match {

      // OPENING PHASE: FUNDER FLOW

      case (null, init: INPUT_INIT_FUNDER, null) =>
        val channelKeyPath = init.remoteInfo.keyPath(init.localParams)
        val localFundingPubKey = init.remoteInfo.fundingPublicKey(init.localParams.fundingKeyPath).publicKey
        val emptyUpfrontShutdown: TlvStream[OpenChannelTlv] = TlvStream(ChannelTlv UpfrontShutdownScript ByteVector.empty)

        val basePoint = init.localParams.walletStaticPaymentBasepoint.getOrElse(init.remoteInfo.paymentPoint(channelKeyPath).publicKey)
        val open = OpenChannel(LNParams.chainHash, init.temporaryChannelId, init.fundingAmount, init.pushAmount, init.localParams.dustLimit, init.localParams.maxHtlcValueInFlightMsat,
          init.localParams.channelReserve, init.localParams.htlcMinimum, init.initialFeeratePerKw, init.localParams.toSelfDelay, init.localParams.maxAcceptedHtlcs, localFundingPubKey,
          init.remoteInfo.revocationPoint(channelKeyPath).publicKey, basePoint, init.remoteInfo.delayedPaymentPoint(channelKeyPath).publicKey,
          init.remoteInfo.htlcPoint(channelKeyPath).publicKey, init.remoteInfo.commitmentPoint(channelKeyPath, index = 0L),
          init.channelFlags, emptyUpfrontShutdown)

        val data1 = DATA_WAIT_FOR_ACCEPT_CHANNEL(init, open)
        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(open)


      case (wait: DATA_WAIT_FOR_ACCEPT_CHANNEL, accept: AcceptChannel, WAIT_FOR_ACCEPT) =>
        val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat, accept.channelReserveSatoshis, accept.htlcMinimumMsat, accept.toSelfDelay,
          accept.maxAcceptedHtlcs, accept.fundingPubkey, accept.revocationBasepoint, accept.paymentBasepoint, accept.delayedPaymentBasepoint, accept.htlcBasepoint)

        Helpers.validateParamsFunder(wait.lastSent, accept)

        val multisig = Script pay2wsh Scripts.multiSig2of2(wait.lastSent.fundingPubkey, remoteParams.fundingPubKey)
        chainWallet.wallet.makeFundingTx(Script.write(multisig), wait.initFunder.fundingAmount, wait.initFunder.fundingTxFeeratePerKw).foreach(process)
        BECOME(DATA_WAIT_FOR_FUNDING_INTERNAL(wait.initFunder, remoteParams, accept.firstPerCommitmentPoint, wait.lastSent), WAIT_FOR_ACCEPT)


      case (wait: DATA_WAIT_FOR_FUNDING_INTERNAL, MakeFundingTxResponse(fundingTx, fundingTxOutputIndex, fundingTxFee), WAIT_FOR_ACCEPT) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFunder.remoteInfo, wait.initFunder.channelVersion,
          wait.initFunder.temporaryChannelId, wait.initFunder.localParams, wait.remoteParams, wait.initFunder.fundingAmount, wait.initFunder.pushAmount,
          wait.initFunder.initialFeeratePerKw, fundingTx.hash, fundingTxOutputIndex, wait.remoteFirstPerCommitmentPoint).right.get

        require(fundingTx.txOut(fundingTxOutputIndex).publicKeyScript == localCommitTx.input.txOut.publicKeyScript)

        val extendedFundingPubKey = wait.initFunder.remoteInfo.fundingPublicKey(wait.initFunder.localParams.fundingKeyPath)
        val localSigOfRemoteTx = wait.initFunder.remoteInfo.sign(remoteCommitTx, extendedFundingPubKey, TxOwner.Remote, wait.initFunder.channelVersion.commitmentFormat)
        val fundingCreated = FundingCreated(wait.initFunder.temporaryChannelId, fundingTxid = fundingTx.hash, fundingTxOutputIndex, localSigOfRemoteTx)

        val data1 = DATA_WAIT_FOR_FUNDING_SIGNED(wait.initFunder.remoteInfo, channelId = toLongId(fundingTx.hash, fundingTxOutputIndex), wait.initFunder.localParams,
          wait.remoteParams, fundingTx, fundingTxFee, localSpec, localCommitTx, RemoteCommit(index = 0L, remoteSpec, remoteCommitTx.tx.txid, wait.remoteFirstPerCommitmentPoint),
          wait.lastSent.channelFlags, wait.initFunder.channelVersion, fundingCreated)

        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(fundingCreated)


      case (wait: DATA_WAIT_FOR_FUNDING_SIGNED, signed: FundingSigned, WAIT_FOR_ACCEPT) =>
        val fundingPubKey = wait.remoteInfo.fundingPublicKey(wait.localParams.fundingKeyPath)
        val localSigOfLocalTx = wait.remoteInfo.sign(wait.localCommitTx, fundingPubKey, TxOwner.Local, wait.channelVersion.commitmentFormat)
        val signedLocalCommitTx = Transactions.addSigs(wait.localCommitTx, fundingPubKey.publicKey, wait.remoteParams.fundingPubKey, localSigOfLocalTx, signed.signature)

        if (Transactions.checkSpendable(signedLocalCommitTx).isFailure) {
          chainWallet.wallet.rollback(wait.fundingTx)
          throw new RuntimeException
        } else {
          val publishableTxs = PublishableTxs(signedLocalCommitTx, Nil)
          val commits = NormalCommits(wait.channelVersion, wait.remoteInfo, wait.localParams, wait.remoteParams, wait.channelFlags, LocalCommit(index = 0L, wait.localSpec, publishableTxs),
            wait.remoteCommit, LocalChanges(Nil, Nil, Nil), RemoteChanges(Nil, Nil, Nil), localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(randomKey.publicKey),
            signedLocalCommitTx.input, ShaChain.init, wait.channelId)

          chainWallet.watcher ! WatchSpent(receiver, commits.commitInput.outPoint.txid, commits.commitInput.outPoint.index.toInt, commits.commitInput.txOut.publicKeyScript, BITCOIN_FUNDING_SPENT)
          chainWallet.watcher ! WatchConfirmed(receiver, commits.commitInput.outPoint.txid, commits.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
          StoreBecomeSend(DATA_WAIT_FOR_FUNDING_CONFIRMED(commits, Some(wait.fundingTx), System.currentTimeMillis, Left(wait.lastSent), deferred = None), WAIT_FUNDING_DONE)

          chainWallet.wallet.commit(wait.fundingTx) onComplete {
            case Success(false) => process(BITCOIN_FUNDING_PUBLISH_FAILED)
            case _ => // It's unknown whether tx has been published, do nothing
          }
        }

      // OPENING PHASE: FUNDEE FLOW

      case (null, init: INPUT_INIT_FUNDEE, null) =>
        val channelKeyPath = init.remoteInfo.keyPath(init.localParams)
        val localFundingPubKey = init.remoteInfo.fundingPublicKey(init.localParams.fundingKeyPath).publicKey
        val emptyUpfrontShutdown: TlvStream[AcceptChannelTlv] = TlvStream(ChannelTlv UpfrontShutdownScript ByteVector.empty)

        Helpers.validateParamsFundee(LNParams.ourInit.features, init.theirOpen, init.remoteInfo.nodeId, LNParams.feeRatesInfo.onChainFeeConf)

        val basePoint = init.localParams.walletStaticPaymentBasepoint.getOrElse(init.remoteInfo.paymentPoint(channelKeyPath).publicKey)
        val accept = AcceptChannel(init.theirOpen.temporaryChannelId, init.localParams.dustLimit, init.localParams.maxHtlcValueInFlightMsat, init.localParams.channelReserve, init.localParams.htlcMinimum,
          LNParams.minDepthBlocks, init.localParams.toSelfDelay, init.localParams.maxAcceptedHtlcs, localFundingPubKey, init.remoteInfo.revocationPoint(channelKeyPath).publicKey, basePoint,
          init.remoteInfo.delayedPaymentPoint(channelKeyPath).publicKey, init.remoteInfo.htlcPoint(channelKeyPath).publicKey,
          init.remoteInfo.commitmentPoint(channelKeyPath, index = 0L), emptyUpfrontShutdown)

        val remoteParams = RemoteParams(init.theirOpen.dustLimitSatoshis, init.theirOpen.maxHtlcValueInFlightMsat, init.theirOpen.channelReserveSatoshis,
          init.theirOpen.htlcMinimumMsat, init.theirOpen.toSelfDelay, init.theirOpen.maxAcceptedHtlcs, init.theirOpen.fundingPubkey, init.theirOpen.revocationBasepoint,
          init.theirOpen.paymentBasepoint, init.theirOpen.delayedPaymentBasepoint, init.theirOpen.htlcBasepoint)

        val data1 = DATA_WAIT_FOR_FUNDING_CREATED(init, remoteParams, accept)
        BECOME(data1, WAIT_FOR_ACCEPT)
        SEND(accept)


      case (wait: DATA_WAIT_FOR_FUNDING_CREATED, created: FundingCreated, WAIT_FOR_ACCEPT) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Helpers.Funding.makeFirstCommitTxs(wait.initFundee.remoteInfo, wait.initFundee.channelVersion,
          wait.initFundee.theirOpen.temporaryChannelId, wait.initFundee.localParams, wait.remoteParams, wait.initFundee.theirOpen.fundingSatoshis,
          wait.initFundee.theirOpen.pushMsat, wait.initFundee.theirOpen.feeratePerKw, created.fundingTxid, created.fundingOutputIndex,
          wait.initFundee.theirOpen.firstPerCommitmentPoint).right.get

        val fundingPubKey = wait.initFundee.remoteInfo.fundingPublicKey(wait.initFundee.localParams.fundingKeyPath)
        val localSigOfLocalTx = wait.initFundee.remoteInfo.sign(localCommitTx, fundingPubKey, TxOwner.Local, wait.initFundee.channelVersion.commitmentFormat)
        val signedLocalCommitTx = Transactions.addSigs(localCommitTx, fundingPubKey.publicKey, wait.remoteParams.fundingPubKey, localSigOfLocalTx, created.signature)

        if (Transactions.checkSpendable(signedLocalCommitTx).isFailure) throw new RuntimeException

        val localSigOfRemoteTx = wait.initFundee.remoteInfo.sign(remoteCommitTx, fundingPubKey, TxOwner.Remote, wait.initFundee.channelVersion.commitmentFormat)
        val fundingSigned = FundingSigned(channelId = toLongId(created.fundingTxid, created.fundingOutputIndex), signature = localSigOfRemoteTx)

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
        // Remote peer may send a tx which is unrelated to our agreed upon channel funding, that is, we won't be able to spend our commit tx, check this right away
        def correct: Unit = Transaction.correctlySpends(wait.commitments.localCommit.publishableTxs.commitTx.tx, Seq(event.tx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

        if (Try(correct).isFailure && wait.lastSent.isRight) BECOME(wait, CLOSING) else {
          val shortChannelId = ShortChannelId(event.blockHeight, event.txIndex, wait.commitments.commitInput.outPoint.index.toInt)
          val nextPerCommitmentPoint = wait.commitments.remoteInfo.commitmentPoint(wait.commitments.channelKeyPath, index = 1L)
          val fundingLocked = FundingLocked(wait.channelId, nextPerCommitmentPoint)

          val data1 = DATA_WAIT_FOR_FUNDING_LOCKED(wait.commitments, shortChannelId, fundingLocked)
          StoreBecomeSend(data1, WAIT_FUNDING_DONE, fundingLocked)
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
        data = me STORE some.modify(_.commitments.remoteInfo).setTo(remoteInfo)


      case (norm: DATA_NORMAL, update: ChannelUpdate, OPEN | SLEEPING) if update.shortChannelId == norm.shortChannelId && Announcements.checkSig(update)(norm.commitments.remoteInfo.nodeId) =>
        data = me STORE norm.modify(_.commitments.updateOpt).setTo(update.toSome)


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


      case (norm: DATA_NORMAL, cmd: CMD_ADD_HTLC, OPEN) if norm.localShutdown.isEmpty && norm.remoteShutdown.isEmpty =>
        val (commits1, updateAddHtlcMsg) = norm.commitments.sendAdd(cmd, LNParams.blockCount.get, LNParams.feeRatesInfo.onChainFeeConf)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(updateAddHtlcMsg)
        doProcess(CMD_SIGN)


      case (_: DATA_NORMAL, cmd: CMD_ADD_HTLC, SLEEPING) =>
        // Instruct payment master to not omit this channel yet
        throw CMDException(ChannelOffline, cmd)


      case (_, cmd: CMD_ADD_HTLC, _) =>
        // Instruct payment master to omit this channel
        throw CMDException(new RuntimeException, cmd)


      case (norm: DATA_NORMAL, cmd: CMD_FULFILL_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFulfillHtlc(norm.channelId, cmd.theirAdd.id, cmd.preimage)
        val commits1 = norm.commitments.addLocalProposal(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(msg)


      case (norm: DATA_NORMAL, cmd: CMD_FAIL_HTLC, OPEN) if !norm.commitments.alreadyReplied(cmd.theirAdd.id) =>
        val msg = OutgoingPacket.buildHtlcFailure(cmd, theirAdd = cmd.theirAdd)
        val commits1 = norm.commitments.addLocalProposal(msg)
        BECOME(norm.copy(commitments = commits1), OPEN)
        SEND(msg)


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
        val fulfill = RemoteFulfill(msg.paymentPreimage, ourAdd)
        BECOME(norm.copy(commitments = commits1), OPEN)
        events.fulfillReceived(fulfill)


      case (norm: DATA_NORMAL, fail: UpdateFailHtlc, OPEN) =>
        val (commits1, _) = norm.commitments.receiveFail(fail)
        BECOME(norm.copy(commitments = commits1), OPEN)


      case (norm: DATA_NORMAL, malformed: UpdateFailMalformedHtlc, OPEN) =>
        val (commits1, _) = norm.commitments.receiveFailMalformed(malformed)
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
        val (data1, replies) = handleRemoteShutdown(norm, remote, LNParams.feeRatesInfo.onChainFeeConf)
        StoreBecomeSend(data1, OPEN, replies:_*)

      // NEGOTIATIONS

      case (negs: DATA_NEGOTIATING, remote: ClosingSigned, OPEN) => handleNegotiations(negs, remote, LNParams.feeRatesInfo.onChainFeeConf)

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

                // if commitment number is zero, we also need to make sure that the funding tx has been published
                if (data1.commitments.localCommit.index == 0 && data1.commitments.remoteCommit.index == 0)
                  chainWallet.watcher ! GetTxWithMeta(commitInput.outPoint.txid)
            }

          case data1: DATA_WAIT_FOR_FUNDING_CONFIRMED =>
            chainWallet.watcher ! GetTxWithMeta(commitInput.outPoint.txid)
            BECOME(data1, SLEEPING)

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
        val myCurrentPerCommitmentPoint = data1.commitments.remoteInfo.commitmentPoint(data1.commitments.channelKeyPath, data1.commitments.localCommit.index)
        val yourLastPerCommitmentSecret = data1.commitments.remotePerCommitmentSecrets.lastIndex.flatMap(data1.commitments.remotePerCommitmentSecrets.getHash).getOrElse(ByteVector32.Zeroes)
        val reestablish = ChannelReestablish(data1.channelId, data1.commitments.localCommit.index + 1, data1.commitments.remoteCommit.index, PrivateKey(yourLastPerCommitmentSecret), myCurrentPerCommitmentPoint)
        SEND(reestablish)


      case (data1: DATA_CLOSING, _: ChannelReestablish, CLOSING) =>
        val error = Error(data1.channelId, s"funding tx has been spent")
        SEND(error)


      case (wait: DATA_WAIT_FOR_FUNDING_CONFIRMED, _: ChannelReestablish, SLEEPING) =>
        // We put back the watch (operation is idempotent) because corresponding event may have been already fired while we were in SLEEPING
        chainWallet.watcher ! WatchConfirmed(receiver, wait.commitments.commitInput.outPoint.txid, wait.commitments.commitInput.txOut.publicKeyScript, LNParams.minDepthBlocks, BITCOIN_FUNDING_DEPTHOK)
        BECOME(wait, OPEN)


      case (wait: DATA_WAIT_FOR_FUNDING_LOCKED, _: ChannelReestablish, SLEEPING) =>
        SEND(wait.lastSent)
        BECOME(wait, OPEN)


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
