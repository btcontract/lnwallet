package com.lightning.walletapp.ln

import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.ln.ChanErrorCodes._

import fr.acinq.eclair.UInt64
import scodec.bits.ByteVector
import java.util.concurrent.Executors
import fr.acinq.bitcoin.Protocol.Zeroes
import com.lightning.walletapp.ChannelManager
import fr.acinq.bitcoin.ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.LNDirectionalMessage
import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.lightning.walletapp.ln.Helpers.{Closing, Funding}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{Point, Scalar}
import fr.acinq.bitcoin.{Satoshi, Transaction}
import scala.util.{Failure, Success, Try}


object Channel {
  val WAIT_FOR_INIT = "WAIT-FOR-INIT"
  val WAIT_FOR_ACCEPT = "WAIT-FOR-ACCEPT"
  val WAIT_FUNDING_SIGNED = "WAIT-FUNDING-SIGNED"

  // All states below are persisted
  val WAIT_FUNDING_DONE = "WAIT-FUNDING-DONE"
  val NEGOTIATIONS = "NEGOTIATIONS"
  val SLEEPING = "SLEEPING"
  val OPEN = "OPEN"

  // No tears, only dreams now
  val SUSPENDED = "SUSPENDED"
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def isOpeningOrOperational(chan: Channel) = isOperational(chan) || isOpening(chan)
  def isOpening(chan: Channel) = chan.data.isInstanceOf[WaitFundingDoneData]

  def isOperational(chan: Channel) = chan.data match {
    case NormalData(_, _, None, None, None) => true
    case hc: HostedCommits => hc.getError.isEmpty
    case _ => false
  }

  def channelAndHop(chan: Channel) = for {
    // Only proceed if channel has (1) commits
    // and (2) remote update supplied by peer

    commits <- chan.getCommits
    chanUpdate <- commits.updateOpt
    nodeId = chan.data.announce.nodeId
    path = Vector(chanUpdate toHop nodeId)
  } yield chan -> path
}

abstract class Channel(val isHosted: Boolean) extends StateMachine[ChannelData] { me =>
  def process(change: Any) = Future(me doProcess change) onFailure { case failure => events onException me -> failure }

  def getCommits: Option[Commitments] = data match {
    case normal: HasNormalCommits => Some(normal.commitments)
    case hosted: HostedCommits => Some(hosted)
    case _ => None
  }

  def isUpdatable(upd: ChannelUpdate) =
    getCommits.flatMap(_.updateOpt) forall { oldUpdate =>
      val isDifferentShortId = oldUpdate.shortChannelId != upd.shortChannelId
      val isOldUpdateRefresh = !isDifferentShortId && oldUpdate.timestamp < upd.timestamp
      isDifferentShortId || isOldUpdateRefresh
    }

  def RESOLVE(prevUpds: Traversable[LightningMessage], cs: Commitments) = {
    // New state is settled, we resolve remote updates from previous state which can be done once
    // by doing it this way we ensure it is not possible to resolve the same payment differently twice

    val resolves = prevUpds collect { case remoteAdd: UpdateAddHtlc =>
      // We don't want to receive a payment into a channel we have originally sent it from in an attempt to rebalance
      val isLoop = cs.localSpec.htlcs.exists(htlc => !htlc.incoming && htlc.add.paymentHash == remoteAdd.paymentHash)
      resolveHtlc(LNParams.keys.makeFakeKey(remoteAdd.paymentHash), remoteAdd, LNParams.bag, isLoop)
    }

    for (cmd <- resolves) me doProcess cmd
    // Inform if there were any commands
    resolves.isEmpty
  }

  def SEND(lightningMessage: LightningMessage) = for {
    work <- ConnectionManager.workers.get(data.announce.nodeId)
  } work.handler process lightningMessage

  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition must always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def UPDATA(d1: ChannelData) = BECOME(d1, state)
  def STORE[T <: ChannelData](data: T): T

  def inFlightHtlcs: Set[Htlc]
  def estCanReceiveMsat: Long
  def estCanSendMsat: Long
  def refundableMsat: Long

  var waitingUpdate: Boolean = true
  var permanentOffline: Boolean = true
  var listeners: Set[ChannelListener] = _
  val events: ChannelListener = new ChannelListener {
    override def onProcessSuccess = { case ps => for (lst <- listeners if lst.onProcessSuccess isDefinedAt ps) lst onProcessSuccess ps }
    override def onException = { case failure => for (lst <- listeners if lst.onException isDefinedAt failure) lst onException failure }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }
    override def unknownHostedHtlcsDetected(hc: HostedCommits) = for (lst <- listeners) lst unknownHostedHtlcsDetected hc
    override def fulfillReceived(upd: UpdateFulfillHtlc) = for (lst <- listeners) lst fulfillReceived upd
    override def outPaymentAccepted(rd: RoutingData) = for (lst <- listeners) lst outPaymentAccepted rd
    override def onSettled(cs: Commitments) = for (lst <- listeners) lst.onSettled(cs)
  }
}

abstract class NormalChannel extends Channel(isHosted = false) { me =>
  def fundTxId = data match { case hnc: HasNormalCommits => hnc.commitments.commitInput.outPoint.txid case _ => ByteVector.empty }
  def inFlightHtlcs: Set[Htlc] = data match { case hnc: HasNormalCommits => hnc.commitments.reducedRemoteState.spec.htlcs case _ => Set.empty }
  def estCanSendMsat = data match { case hnc: HasNormalCommits => hnc.commitments.nextDummyReduced.canSendMsat + LNParams.minCapacityMsat case _ => 0L }
  def estCanReceiveMsat = data match { case hnc: HasNormalCommits => hnc.commitments.nextDummyReduced.canReceiveMsat case _ => 0L }
  def refundableMsat = data match { case hnc: HasNormalCommits => hnc.commitments.localCommit.spec.toLocalMsat case _ => 0L }

  def CLOSEANDWATCH(close: ClosingData): Unit
  def ASKREFUNDPEER(some: HasNormalCommits, point: Point): Unit
  def GETREV(cs: NormalCommits, tx: Transaction): Option[RevokedCommitPublished]
  def REV(cs: NormalCommits, rev: RevokeAndAck): Unit

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd: CMDOpenChannel, WAIT_FOR_INIT) =>
        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash, cmd.tempChanId,
          cmd.fundingSat, cmd.pushMsat, cmd.localParams.dustLimit.amount, cmd.localParams.maxHtlcValueInFlightMsat,
          cmd.localParams.channelReserveSat, LNParams.minPaymentMsat, cmd.initialFeeratePerKw, cmd.localParams.toSelfDelay,
          cmd.localParams.maxAcceptedHtlcs, cmd.localParams.fundingPrivKey.publicKey, cmd.localParams.revocationBasepoint,
          cmd.localParams.paymentBasepoint, cmd.localParams.delayedPaymentBasepoint, cmd.localParams.htlcBasepoint,
          Generators.perCommitPoint(cmd.localParams.shaSeed, index = 0L), cmd.channelFlags)


      case (InitData(announce), Tuple2(localParams: LocalParams, open: OpenChannel), WAIT_FOR_INIT) =>
        if (LNParams.chainHash != open.chainHash) throw new LightningException("They have provided a wrong chain hash")
        if (open.channelFlags.isPublic) throw new LightningException("They are offering a public channel and we only support private ones")
        if (open.pushMsat > 1000L * open.fundingSatoshis) throw new LightningException("They are trying to push more than proposed capacity")
        if (open.dustLimitSatoshis > open.channelReserveSatoshis) throw new LightningException("Their dust limit exceeds their channel reserve")
        if (open.feeratePerKw < LNParams.minFeeratePerKw) throw new LightningException("Their proposed opening on-chain fee is too small")
        if (open.toSelfDelay > LNParams.maxToSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (open.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (open.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (open.pushMsat < 0) throw new LightningException("Their pushMsat is negative")

        val toLocalMsat \ toRemoteMsat = (open.pushMsat, open.fundingSatoshis * 1000L - open.pushMsat)
        if (toLocalMsat <= open.channelReserveSatoshis * 1000L && toRemoteMsat <= open.channelReserveSatoshis * 1000L)
          throw new LightningException("Both toLocal and toRemote amounts are less than total channel reserve")

        if (open.fundingSatoshis / open.channelReserveSatoshis < LNParams.channelReserveToFundingRatio / 5)
          throw new LightningException("Their imposed channel reserve is too high relative to capacity")

        val remoteParams = AcceptChannel(open.temporaryChannelId, open.dustLimitSatoshis, open.maxHtlcValueInFlightMsat,
          open.channelReserveSatoshis, open.htlcMinimumMsat, minimumDepth = 6, open.toSelfDelay, open.maxAcceptedHtlcs,
          open.fundingPubkey, open.revocationBasepoint, open.paymentBasepoint, open.delayedPaymentBasepoint,
          open.htlcBasepoint, open.firstPerCommitmentPoint)

        val firstPerCommitPoint = Generators.perCommitPoint(localParams.shaSeed, index = 0L)
        BECOME(WaitFundingCreatedRemote(announce, localParams, remoteParams, open), WAIT_FOR_ACCEPT) SEND AcceptChannel(open.temporaryChannelId, localParams.dustLimit.amount,
          localParams.maxHtlcValueInFlightMsat, localParams.channelReserveSat, LNParams.minPaymentMsat, LNParams.minDepth, localParams.toSelfDelay, localParams.maxAcceptedHtlcs,
          localParams.fundingPrivKey.publicKey, localParams.revocationBasepoint, localParams.paymentBasepoint, localParams.delayedPaymentBasepoint,
          localParams.htlcBasepoint, firstPerCommitPoint)


      case (WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT) if accept.temporaryChannelId == cmd.tempChanId =>
        if (accept.dustLimitSatoshis > cmd.localParams.channelReserveSat) throw new LightningException("Our channel reserve is less than their dust")
        if (UInt64(100000000L) > accept.maxHtlcValueInFlightMsat) throw new LightningException("Their maxHtlcValueInFlightMsat is too low")
        if (accept.channelReserveSatoshis > cmd.fundingSat / 10) throw new LightningException("Their proposed reserve is too high")
        if (accept.toSelfDelay > LNParams.maxToSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (accept.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (accept.htlcMinimumMsat > 546000L) throw new LightningException("Their htlcMinimumMsat is too high")
        if (accept.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (accept.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")
        if (accept.minimumDepth > 6L) throw new LightningException("Their minimumDepth is too high")

        val fundTx = cmd.batch.replaceDummyAndSign(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstCommitTxs(cmd.localParams, cmd.fundingSat,
          cmd.pushMsat, cmd.initialFeeratePerKw, accept, fundTx.hash, cmd.batch.fundOutIdx, accept.firstPerCommitmentPoint)

        val longId = Tools.toLongId(fundTx.hash, cmd.batch.fundOutIdx)
        val localSigOfRemoteTx = Scripts.sign(cmd.localParams.fundingPrivKey)(remoteCommitTx)
        val fundingCreated = FundingCreated(cmd.tempChanId, fundTx.hash, cmd.batch.fundOutIdx, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(index = 0L, remoteSpec, Some(remoteCommitTx.tx), accept.firstPerCommitmentPoint)
        val core = WaitFundingSignedCore(cmd.localParams, longId, Some(cmd.channelFlags), accept, localSpec, firstRemoteCommit)
        BECOME(WaitFundingSignedData(announce, core, localCommitTx, fundTx), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have proposed us a channel, we have agreed to their terms and now they have created a funding tx which we should check
      case (WaitFundingCreatedRemote(announce, localParams, accept, open), FundingCreated(_, txid, outIndex, remoteSig), WAIT_FOR_ACCEPT) =>

        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
          Funding.makeFirstCommitTxs(localParams, open.fundingSatoshis, open.pushMsat,
            open.feeratePerKw, accept, txid, outIndex, open.firstPerCommitmentPoint)

        val signedLocalCommitTx = Scripts.addSigs(localCommitTx, localParams.fundingPrivKey.publicKey,
          accept.fundingPubkey, Scripts.sign(localParams.fundingPrivKey)(localCommitTx), remoteSig)

        if (Scripts.checkValid(signedLocalCommitTx).isSuccess) {
          val localSigOfRemoteTx = Scripts.sign(localParams.fundingPrivKey)(remoteCommitTx)
          val fundingSigned = FundingSigned(Tools.toLongId(txid, outIndex), localSigOfRemoteTx)
          val rc = RemoteCommit(index = 0L, remoteSpec, Some(remoteCommitTx.tx), open.firstPerCommitmentPoint)
          val core = WaitFundingSignedCore(localParams, fundingSigned.channelId, Some(open.channelFlags), accept, localSpec, rc)
          BECOME(WaitBroadcastRemoteData(announce, core, core makeCommitments signedLocalCommitTx), WAIT_FUNDING_DONE) SEND fundingSigned
          // Once this happens WaitBroadcastRemoteData will be saved in channel listener
        } else throw new LightningException


      // LOCAL FUNDER FLOW


      // They have signed our first commit, we can broadcast a local funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        val localSignature = Scripts.sign(wait.core.localParams.fundingPrivKey)(wait.localCommitTx)
        val localKey \ remoteKey = wait.core.localParams.fundingPrivKey.publicKey -> wait.core.remoteParams.fundingPubkey
        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, localKey, remoteKey, localSignature, remote.signature)
        val wait1 = WaitFundingDoneData(wait.announce, None, None, wait.fundingTx, wait.core makeCommitments signedLocalCommitTx)
        if (Scripts.checkValid(signedLocalCommitTx).isSuccess) BECOME(wait1, WAIT_FUNDING_DONE) else throw new LightningException


      // BECOMING OPEN


      // We have agreed to proposed incoming channel and they have published a funding tx
      case (wait: WaitBroadcastRemoteData, CMDSpent(fundTx), WAIT_FUNDING_DONE | SLEEPING)
        // GUARD: this is actually a funding tx and we can correctly spend from it
        if fundTxId == fundTx.txid && wait.fundingError.isEmpty =>

        val data1 = me STORE decideOnFundeeTx(wait, fundTx)
        val isZeroConfSpendablePush = wait.commitments.channelFlags.exists(_.isZeroConfSpendablePush)
        if (isZeroConfSpendablePush) me UPDATA data1 doProcess CMDConfirmed(fundTx) else me UPDATA data1


      // We have agreed to proposed incoming channel and see a confirmed a funding tx right away
      case (wait: WaitBroadcastRemoteData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE | SLEEPING)
        // GUARD: this is actually a funding tx and we can correctly spend from it
        if fundTxId == fundTx.txid && wait.fundingError.isEmpty =>
        val data1 = me STORE decideOnFundeeTx(wait, fundTx)
        me UPDATA data1 doProcess CMDConfirmed(fundTx)


      case (wait: WaitBroadcastRemoteData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // For turbo chans, their fundingLocked may arrive faster than onchain event
        // no need to store their fundingLocked because it gets re-sent on reconnect
        me UPDATA wait.copy(their = their.some)


      case (wait: WaitFundingDoneData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their fundingLocked because it gets re-sent on reconnect
        if (wait.our.isEmpty) me UPDATA wait.copy(their = their.some)
        else becomeOpen(wait.copy(their = their.some), their)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE | SLEEPING) if fundTxId == fundTx.txid =>
        // Transaction has been confirmed, save our FundingLocked and inform peer if we are currently online
        val ourFirstFundingLocked = makeFirstFundingLocked(wait)
        val wait1 = wait.copy(our = ourFirstFundingLocked.some)
        if (wait1.their.isEmpty) me UPDATA STORE(wait1)
        else becomeOpen(wait1, wait1.their.get)
        me SEND ourFirstFundingLocked


      // OPEN MODE


      // GUARD: due to timestamp filter the first update they send must be for our channel
      case (norm: NormalData, upd: ChannelUpdate, OPEN | SLEEPING) if waitingUpdate && !upd.isHosted =>
        if (me isUpdatable upd) data = me STORE norm.modify(_.commitments.updateOpt).setTo(upd.some)
        waitingUpdate = false


      case (norm: NormalData, addHtlc: UpdateAddHtlc, OPEN) =>
        // Got new incoming HTLC, put it to changes without storing for now
        me UPDATA norm.copy(commitments = norm.commitments receiveAdd addHtlc)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFulfill fulfill)
        events fulfillReceived fulfill


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFail fail)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we have sent to them earlier
        me UPDATA norm.copy(commitments = norm.commitments receiveFailMalformed fail)


      case (norm: NormalData, routingData: RoutingData, OPEN)
        if isOperational(me) && !ChannelManager.activeInFlightHashes.contains(routingData.pr.paymentHash) =>
        // We can send a new HTLC when channel is both operational and online
        val c1 \ updateAddHtlc = norm.commitments sendAdd routingData
        me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
        events outPaymentAccepted routingData
        me doProcess CMDProceed


      case (norm: NormalData, cmd: CMDFulfillHtlc, OPEN) =>
        val c1 \ updateFulfillHtlc = norm.commitments sendFulfill cmd
        me UPDATA norm.copy(commitments = c1) SEND updateFulfillHtlc


      case (norm: NormalData, cmd: CMDFailHtlc, OPEN) =>
        val c1 \ updateFailHtlc = norm.commitments sendFail cmd
        me UPDATA norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm: NormalData, cmd: CMDFailMalformedHtlc, OPEN) =>
        val c1 \ updateFailMalformedHtlс = norm.commitments sendFailMalformed cmd
        me UPDATA norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      case (norm: NormalData, CMDProceed, OPEN)
        // Only if we have a point and something to sign
        if norm.commitments.remoteNextCommitInfo.isRight &&
          (norm.commitments.localChanges.proposed.nonEmpty ||
            norm.commitments.remoteChanges.acked.nonEmpty) =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = norm.commitments sendCommit nextRemotePoint
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND commitSig


      case (norm: NormalData, sig: CommitSig, OPEN) =>
        // We received a commit sig from them, can update local commit
        val c1 \ revokeAndAck = norm.commitments receiveCommit sig
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND revokeAndAck
        // Clear remote commit first
        me doProcess CMDProceed
        events onSettled c1


      case (norm: NormalData, rev: RevokeAndAck, OPEN) =>
        me UPDATA norm.copy(commitments = norm.commitments receiveRevocation rev)
        val noResolves = RESOLVE(norm.commitments.remoteChanges.signed, norm.commitments)
        if (noResolves) me STORE data

        me doProcess CMDProceed
        REV(norm.commitments, rev)


      case (norm: NormalData, fee: UpdateFee, OPEN) if !norm.commitments.localParams.isFunder =>
        // It is their duty to update fees when we are a fundee, this will be persisted later
        me UPDATA norm.copy(commitments = norm.commitments receiveFee fee)


      case (norm: NormalData, CMDFeerate(satPerKw), OPEN) if norm.commitments.localParams.isFunder =>
        val shouldUpdate = LNParams.shouldUpdateFee(satPerKw, norm.commitments.localSpec.feeratePerKw)
        if (shouldUpdate) norm.commitments sendFee satPerKw foreach { case c1 \ feeUpdateMessage =>
          // We send a fee update if current chan unspendable reserve + commitTx fee can afford it
          // otherwise we fail silently in hope that fee will drop or we will receive a payment
          me UPDATA norm.copy(commitments = c1) SEND feeUpdateMessage
          me doProcess CMDProceed
        }


      // SHUTDOWN in WAIT_FUNDING_DONE


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), WAIT_FUNDING_DONE) =>
        val finalScriptPubKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        val ourShutdown = Shutdown(wait.commitments.channelId, scriptPubKey = finalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, None, ourShutdown.some)
        for (msg <- makeFirstFundingLocked(wait) :: ourShutdown :: Nil) me SEND msg
        BECOME(me STORE norm, OPEN)


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), SLEEPING) =>
        val finalScriptPubKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        val ourShutdown = Shutdown(wait.commitments.channelId, scriptPubKey = finalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, None, ourShutdown.some)
        BECOME(me STORE norm, SLEEPING)


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE) =>
        val our = Shutdown(wait.commitments.channelId, wait.commitments.localParams.defaultFinalScriptPubKey)
        val norm = NormalData(wait.announce, wait.commitments, None, our.some, remote.some)
        BECOME(me STORE norm, OPEN) SEND our
        me doProcess CMDProceed


      // SHUTDOWN in OPEN


      case (norm @ NormalData(announce, commitments, txOpt, our, their), CMDShutdown(script), OPEN | SLEEPING) =>
        if (commitments.localHasUnsignedOutgoing || our.isDefined || their.isDefined) startLocalClose(norm) else {
          val our = Shutdown(commitments.channelId, script getOrElse commitments.localParams.defaultFinalScriptPubKey)
          val norm1 = me STORE NormalData(announce, commitments, txOpt, our.some, their)
          me UPDATA norm1 SEND our
        }


      // Either they initiate a shutdown or respond to the one we have sent
      // should sign our unsigned outgoing HTLCs if present and then proceed with shutdown
      case (NormalData(announce, commitments, txOpt, our, None), remote: Shutdown, OPEN) =>
        val norm = NormalData(announce, commitments, txOpt, our, remote.some)
        if (commitments.remoteHasUnsignedOutgoing) startLocalClose(norm)
        else me UPDATA norm doProcess CMDProceed


      // We have nothing to sign so check for valid shutdown state, only consider this if we have nothing in-flight
      case (NormalData(announce, commitments, txOpt, our, their), CMDProceed, OPEN) if inFlightHtlcs.isEmpty =>

        our -> their match {
          case Some(ourSig) \ Some(theirSig) =>
            if (commitments.localParams.isFunder) {
              // Got both shutdowns without HTLCs in-flight so can send a first closing since we are the funder
              val firstProposed = Closing.makeFirstClosing(commitments, ourSig.scriptPubKey, theirSig.scriptPubKey)
              val neg = NegotiationsData(announce, commitments, ourSig, theirSig, firstProposed :: Nil)
              BECOME(me STORE neg, NEGOTIATIONS) SEND firstProposed.localClosingSigned
            } else {
              // Got both shutdowns without HTLCs in-flight so wait for funder's proposal
              val neg = NegotiationsData(announce, commitments, ourSig, theirSig, Nil)
              BECOME(me STORE neg, NEGOTIATIONS)
            }

          case None \ Some(theirSig) =>
            // We have previously received their Shutdown so can respond, then CMDProceed to enter NEGOTIATIONS
            val our = Shutdown(commitments.channelId, commitments.localParams.defaultFinalScriptPubKey)
            val norm1 = me STORE NormalData(announce, commitments, txOpt, our.some, theirSig.some)
            me UPDATA norm1 SEND our
            me doProcess CMDProceed

          case Some(ourSig) \ None =>
            // Was initiated in SLEEPING
            me SEND ourSig

          // Not ready
          case _ =>
        }


      // SYNC and REFUNDING MODE


      case (ref: RefundingData, cr: ChannelReestablish, REFUNDING) =>
        cr.myCurrentPerCommitmentPoint -> ref.remoteLatestPoint match {
          case _ \ Some(ourSavedPoint) => ASKREFUNDPEER(ref, ourSavedPoint)
          case Some(theirPoint) \ _ => ASKREFUNDPEER(ref, theirPoint)
          case _ => // They don't support data-loss-protect
        }


      case (some: HasNormalCommits, cr: ChannelReestablish, SLEEPING)
        // GUARD: their nextRemoteRevocationNumber is unexpectedly too far away
        if some.commitments.localCommit.index < cr.nextRemoteRevocationNumber && cr.myCurrentPerCommitmentPoint.isDefined =>
        val secret = Generators.perCommitSecret(some.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) ASKREFUNDPEER(some, cr.myCurrentPerCommitmentPoint.get)
        else throw new LightningException


      case (norm: NormalData, cr: ChannelReestablish, SLEEPING) =>
        // Resend our FundingLocked if a channel is fresh since they might not receive it before reconnect
        val reSendFundingLocked = cr.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0
        if (reSendFundingLocked) me SEND makeFirstFundingLocked(norm)

        // First we clean up unacknowledged updates
        val localDelta = norm.commitments.localChanges.proposed collect { case _: UpdateAddHtlc => true }
        val remoteDelta = norm.commitments.remoteChanges.proposed collect { case _: UpdateAddHtlc => true }
        val c1 = norm.commitments.modifyAll(_.localChanges.proposed, _.remoteChanges.proposed).setTo(Vector.empty)
          .modify(_.remoteNextHtlcId).using(_ - remoteDelta.size).modify(_.localNextHtlcId).using(_ - localDelta.size)

        def maybeResendOurRevocationMessage = if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
          val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
          val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
          me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
        } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw new LightningException

        c1.remoteNextCommitInfo match {
          // We had sent a new sig and were waiting for their revocation
          // they didn't receive the new sig because disconnection happened
          // we resend the same updates and sig, also be careful about revocation
          case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>
            val revocationWasSentLast = c1.localCommit.index > wait.localCommitIndexSnapshot

            if (!revocationWasSentLast) maybeResendOurRevocationMessage
            for (msg <- c1.localChanges.signed :+ wait.sent) me SEND msg
            if (revocationWasSentLast) maybeResendOurRevocationMessage

          // We had sent a new sig and were waiting for their revocation, they had received
          // the new sig but their revocation was lost during the disconnection, they'll resend us the revocation
          case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendOurRevocationMessage
          case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendOurRevocationMessage
          case _ => throw new LightningException("Local sync error")
        }

        val d1 = norm.copy(commitments = c1)
        BECOME(d1, OPEN) doProcess CMDProceed


      // We're exiting a sync state while funding tx is still not provided
      case (remote: WaitBroadcastRemoteData, _: ChannelReestablish, SLEEPING) =>
        // Need to check whether a funding is present in a listener
        BECOME(remote, WAIT_FUNDING_DONE)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, _: ChannelReestablish, SLEEPING) =>
        for (fundingLocked <- wait.our) me SEND fundingLocked
        BECOME(wait, WAIT_FUNDING_DONE)


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, _: ChannelReestablish, SLEEPING) =>
        // Last closing signed may be empty if we are not a funder of this channel
        val lastClosingSignedOpt = neg.localProposals.headOption.map(_.localClosingSigned)
        for (msg <- neg.localShutdown +: lastClosingSignedOpt.toVector) me SEND msg
        BECOME(neg, NEGOTIATIONS)


      // SYNC: ONLINE/SLEEPING


      case (some: HasNormalCommits, CMDSocketOnline, SLEEPING) =>
        // According to BOLD a first message on connection should be reestablish
        // will specifically NOT work in REFUNDING to not let them know beforehand
        me SEND makeReestablish(some, some.commitments.localCommit.index + 1)
        permanentOffline = false


      case (some: HasNormalCommits, newAnn: NodeAnnouncement, _)
        if state != OPEN && some.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Node was SLEEPING for a long time, update but do not trigger listeners for this change
        data = me STORE some.modify(_.announce).setTo(newAnn)


      case (wait: WaitBroadcastRemoteData, CMDSocketOffline, WAIT_FUNDING_DONE) => BECOME(wait, SLEEPING)
      case (wait: WaitFundingDoneData, CMDSocketOffline, WAIT_FUNDING_DONE) => BECOME(wait, SLEEPING)
      case (negs: NegotiationsData, CMDSocketOffline, NEGOTIATIONS) => BECOME(negs, SLEEPING)
      case (norm: NormalData, CMDSocketOffline, OPEN) => BECOME(norm, SLEEPING)


      // NEGOTIATIONS MODE


      case (neg @ NegotiationsData(_, commitments, localShutdown, remoteShutdown, localProposals, _),
      ClosingSigned(_, remoteClosingFee, remoteClosingSig), NEGOTIATIONS) =>

        val lastLocalFee = localProposals.headOption.map(_.localClosingSigned.feeSatoshis) getOrElse {
          // If we are fundee and we were waiting for them to send their first proposal, we don't have a lastLocalClosingFee
          val closing = Closing.makeFirstClosing(commitments, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
          closing.localClosingSigned.feeSatoshis
        }

        val ClosingTxProposed(closing, closingSigned) =
          Closing.makeClosing(commitments, Satoshi(remoteClosingFee),
            localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)

        val signedClose =
          Scripts.addSigs(closing, commitments.localParams.fundingPrivKey.publicKey,
            commitments.remoteParams.fundingPubkey, closingSigned.signature, remoteClosingSig)

        Scripts.checkValid(txWithInputInfo = signedClose) match {
          case Success(okClose) if remoteClosingFee == lastLocalFee =>
            // Our current and their proposed fees are equal for this tx
            startMutualClose(neg, okClose.tx)

          case Success(okClose) =>
            val nextCloseFee = Satoshi(lastLocalFee + remoteClosingFee) / 4 * 2
            val nextProposed = Closing.makeClosing(commitments, nextCloseFee, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
            if (remoteClosingFee == nextCloseFee.amount) startMutualClose(neg, okClose.tx) SEND nextProposed.localClosingSigned else {
              val d1 = me STORE neg.copy(lastSignedTx = Some(okClose), localProposals = nextProposed +: neg.localProposals)
              me UPDATA d1 SEND nextProposed.localClosingSigned
            }

          case _ =>
            startLocalClose(neg)
            // Nothing left to do here so at least inform user
            throw new LightningException("Remote shutdown signature check failed")
        }


      case (negs: NegotiationsData, _: CMDShutdown, NEGOTIATIONS | SLEEPING) =>
        // Disregard custom scriptPubKey and always refund to local wallet
        startLocalClose(negs)


      // HANDLE FUNDING SPENT


      case (RefundingData(announce, Some(remoteLatestPoint), commitments), CMDSpent(spendTx), REFUNDING)
        // GUARD: we have got a remote commit which we asked them to spend and we have their point
        if spendTx.txIn.exists(input => commitments.commitInput.outPoint == input.outPoint) =>
        val rcp = Closing.claimRemoteMainOutput(commitments, remoteLatestPoint, spendTx)
        val d1 = ClosingData(announce, commitments, refundRemoteCommit = rcp :: Nil)
        BECOME(me STORE d1, CLOSING)


      case (cd: ClosingData, CMDSpent(htlcTx), CLOSING)
        // This may be one of our own 1st tier transactions
        // or they may broadcast their 1st tier, catch all of them
        if cd.revokedCommit.exists(_ spendsFromRevoked htlcTx) =>

        for {
          revCommitPublished <- cd.revokedCommit if revCommitPublished spendsFromRevoked htlcTx
          perCommitmentSecret <- Helpers.Closing.extractCommitmentSecret(cd.commitments, revCommitPublished.commitTx)
          punishTx <- Closing.claimRevokedHtlcTxOutputs(commitments = cd.commitments, htlcTx, perCommitmentSecret)
          punishTxj = com.lightning.walletapp.lnutils.ImplicitConversions.bitcoinLibTx2bitcoinjTx(punishTx)
        } com.lightning.walletapp.Utils.app.kit blockSend punishTxj


      case (some: HasNormalCommits, CMDSpent(tx), _)
        // GUARD: tx which spends our funding is broadcasted, must react
        if tx.txIn.exists(input => some.commitments.commitInput.outPoint == input.outPoint) =>
        val nextRemoteCommitLeft = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(GETREV(some.commitments, tx), nextRemoteCommitLeft, some) match {
          case (_, _, close: ClosingData) if close.refundRemoteCommit.nonEmpty => Tools log s"Existing refund $tx"
          case (_, _, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log s"Existing mutual $tx"
          case (_, _, close: ClosingData) if close.localCommit.exists(_.commitTx.txid == tx.txid) => Tools log s"Existing local $tx"
          case (_, _, close: ClosingData) if close.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(close, tx)
          case (_, _, negs: NegotiationsData) if negs.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(negs, tx)
          case (Some(claim), _, closingData: ClosingData) => me CLOSEANDWATCH closingData.modify(_.revokedCommit).using(claim +: _)
          case (Some(claim), _, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil)
          case (_, Left(nextRemote), _) if nextRemote.txOpt.exists(_.txid == tx.txid) => startRemoteNextClose(some, nextRemote)
          case _ if some.commitments.remoteCommit.txOpt.exists(_.txid == tx.txid) => startRemoteCurrentClose(some)
          case _ if some.commitments.localCommit.commitTx.tx.txid == tx.txid => startLocalClose(some)

          case (_, _, norm: NormalData) =>
            // Example: old snapshot is used two times in a row
            val d1 = norm.modify(_.unknownSpend) setTo Some(tx)
            me UPDATA STORE(d1)

          case _ =>
            // Nothing left to do here so at least inform user
            throw new LightningException("Unknown spend detected")
        }


      // HANDLE INITIALIZATION


      case (null, ref: RefundingData, null) => super.become(ref, REFUNDING)
      case (null, close: ClosingData, null) => super.become(close, CLOSING)
      case (null, init: InitData, null) => super.become(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => super.become(wait, SLEEPING)
      case (null, wait: WaitBroadcastRemoteData, null) => super.become(wait, SLEEPING)
      case (null, negs: NegotiationsData, null) => super.become(negs, SLEEPING)
      case (null, norm: NormalData, null) => super.become(norm, SLEEPING)
      case _ =>
    }

    // Change has been processed without failures
    events onProcessSuccess Tuple3(me, data, change)
  }

  def makeReestablish(some: HasNormalCommits, nextLocalCommitmentNumber: Long) = {
    val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
    val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).map(ByteVector.view)
    val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
    val myCurrentPerCommitmentPointOpt = Some(myCurrentPerCommitmentPoint)

    ChannelReestablish(some.commitments.channelId, nextLocalCommitmentNumber, some.commitments.remoteCommit.index,
      yourLastPerCommitmentSecret.map(Scalar.apply) orElse Some(Zeroes), myCurrentPerCommitmentPointOpt)
  }

  def makeFirstFundingLocked(some: HasNormalCommits) = {
    val first = Generators.perCommitPoint(some.commitments.localParams.shaSeed, 1L)
    FundingLocked(some.commitments.channelId, nextPerCommitmentPoint = first)
  }

  def decideOnFundeeTx(wait: WaitBroadcastRemoteData, fundTx: Transaction) = Try {
    val ourFirstCommitmentTransaction: Transaction = wait.commitments.localCommit.commitTx.tx
    Transaction.correctlySpends(ourFirstCommitmentTransaction, Seq(fundTx), STANDARD_SCRIPT_VERIFY_FLAGS)
  } match {
    case Failure(reason) => wait.modify(_.fundingError) setTo Some(reason.getMessage)
    case _ => WaitFundingDoneData(wait.announce, None, wait.their, fundTx, wait.commitments)
  }

  private def becomeOpen(wait: WaitFundingDoneData, their: FundingLocked) = {
    val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), OPEN)
  }

  private def startMutualClose(some: HasNormalCommits, tx: Transaction) = some match {
    case closingData: ClosingData => BECOME(me STORE closingData.copy(mutualClose = tx +: closingData.mutualClose), CLOSING)
    case neg: NegotiationsData => BECOME(me STORE ClosingData(neg.announce, neg.commitments, neg.localProposals, tx :: Nil), CLOSING)
    case _ => BECOME(me STORE ClosingData(some.announce, some.commitments, Nil, tx :: Nil), CLOSING)
  }

  def startLocalClose(some: HasNormalCommits): Unit =
  // Something went wrong and we decided to spend our CURRENT commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, LNParams.bag) -> some match {
      case (_, neg: NegotiationsData) if neg.lastSignedTx.isDefined => startMutualClose(neg, neg.lastSignedTx.get.tx)
      case (localClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(localCommit = localClaim :: Nil)
      case (localClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, localCommit = localClaim :: Nil)
    }

  private def startRemoteCurrentClose(some: HasNormalCommits) =
  // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(remoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, remoteCommit = remoteClaim :: Nil)
    }

  private def startRemoteNextClose(some: HasNormalCommits, nextRemoteCommit: RemoteCommit) =
  // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(nextRemoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, nextRemoteCommit = remoteClaim :: Nil)
    }
}

abstract class HostedChannel extends Channel(isHosted = true) { me =>
  def estCanSendMsat = data match { case hc: HostedCommits => hc.localSpec.toLocalMsat case _ => 0L }
  def estCanReceiveMsat = data match { case hc: HostedCommits => hc.localSpec.toRemoteMsat case _ => 0L }
  def inFlightHtlcs: Set[Htlc] = data match { case hc: HostedCommits => hc.currentAndNextInFlight case _ => Set.empty }
  def isBlockDayOutOfSync(blockDay: Long): Boolean = math.abs(blockDay - LNParams.broadcaster.currentBlockDay) > 1
  def refundableMsat = estCanSendMsat

  private var isChainHeightKnown: Boolean = false
  private var isSocketConnected: Boolean = false

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (wait: WaitRemoteHostedReply, CMDSocketOnline, WAIT_FOR_INIT) =>
        if (isChainHeightKnown) BECOME(wait, WAIT_FOR_ACCEPT) SEND wait.invokeMsg
        isSocketConnected = true
        permanentOffline = false


      case (wait: WaitRemoteHostedReply, CMDChainTipKnown, WAIT_FOR_INIT) =>
        if (isSocketConnected) BECOME(wait, WAIT_FOR_ACCEPT) SEND wait.invokeMsg
        isChainHeightKnown = true


      case (WaitRemoteHostedReply(announce, refundScriptPubKey, _), init: InitHostedChannel, WAIT_FOR_ACCEPT) =>
        if (init.liabilityDeadlineBlockdays < LNParams.minHostedLiabilityBlockdays) throw new LightningException("Their liability deadline is too low")
        if (init.initialClientBalanceMsat > init.channelCapacityMsat) throw new LightningException("Their init balance for us is larger than capacity")
        if (init.minimalOnchainRefundAmountSatoshis > LNParams.minHostedOnChainRefundSat) throw new LightningException("Their min refund is too high")
        if (init.channelCapacityMsat < LNParams.minHostedOnChainRefundSat) throw new LightningException("Their proposed channel capacity is too low")
        if (UInt64(100000000L) > init.maxHtlcValueInFlightMsat) throw new LightningException("Their max value in-flight is too low")
        if (init.htlcMinimumMsat > 546000L) throw new LightningException("Their minimal payment size is too high")
        if (init.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")

        val localHalfSignedHC =
          restoreCommits(LastCrossSignedState(refundScriptPubKey, init, LNParams.broadcaster.currentBlockDay, init.initialClientBalanceMsat,
            init.channelCapacityMsat - init.initialClientBalanceMsat, localUpdates = 0L, remoteUpdates = 0L, incomingHtlcs = Nil, outgoingHtlcs = Nil,
            localSigOfRemote = ByteVector.empty, remoteSigOfLocal = ByteVector.empty).withLocalSigOfRemote(LNParams.keys.extendedNodeKey.privateKey), announce)

        val localSU = localHalfSignedHC.lastCrossSignedState.stateUpdate(isTerminal = true)
        me UPDATA WaitRemoteHostedStateUpdate(announce, localHalfSignedHC) SEND localSU


      case (WaitRemoteHostedStateUpdate(announce, hc), remoteSU: StateUpdate, WAIT_FOR_ACCEPT) =>
        val localCompleteLCSS = hc.lastCrossSignedState.copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS)
        val isRightRemoteUpdateNumber = hc.lastCrossSignedState.remoteUpdates == remoteSU.localUpdates
        val isRightLocalUpdateNumber = hc.lastCrossSignedState.localUpdates == remoteSU.remoteUpdates
        val isRemoteSigOk = localCompleteLCSS.verifyRemoteSig(hc.announce.nodeId)

        if (me isBlockDayOutOfSync remoteSU.blockDay) throw new LightningException("Their blockday is wrong")
        if (!isRightRemoteUpdateNumber) throw new LightningException("Their remote update number is wrong")
        if (!isRightLocalUpdateNumber) throw new LightningException("Their local update number is wrong")
        if (!isRemoteSigOk) throw new LightningException("Their signature is wrong")
        BECOME(me STORE hc.copy(lastCrossSignedState = localCompleteLCSS), OPEN)


      case (wait: WaitRemoteHostedReply, remoteLCSS: LastCrossSignedState, WAIT_FOR_ACCEPT) =>
        // We have expected InitHostedChannel but got LastCrossSignedState so this channel exists already
        // make sure our signature match and if so then become OPEN using host supplied state data
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(LNParams.keys.extendedNodeKey.publicKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(wait.announce.nodeId)
        val hc = restoreCommits(remoteLCSS.reverse, wait.announce)

        if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc, ERR_HOSTED_WRONG_LOCAL_SIG)
        else syncFromRestored(hc)


      // CHANNEL IS ESTABLISHED


      case (hc: HostedCommits, addHtlc: UpdateAddHtlc, OPEN) =>
        // They have sent us an incoming payment, do not store yet
        me UPDATA hc.receiveAdd(addHtlc)


      case (hc: HostedCommits, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFulfill(fulfill)
        events fulfillReceived fulfill


      case (hc: HostedCommits, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFail(fail)


      case (hc: HostedCommits, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC, do not store yet
        me UPDATA hc.receiveFailMalformed(fail)


      case (hc: HostedCommits, routingData: RoutingData, OPEN)
        if !ChannelManager.activeInFlightHashes.contains(routingData.pr.paymentHash)=>
        val hostedCommits1 \ updateAddHtlc = hc.sendAdd(routingData)
        me UPDATA hostedCommits1 SEND updateAddHtlc
        events outPaymentAccepted routingData
        me doProcess CMDProceed


      case (hc: HostedCommits, CMDProceed, OPEN) =>
        // Store not yet cross-signed updates because once local signature is sent they may have a new complete state
        val nextHC = hc.nextLocalUnsignedLCSS(LNParams.broadcaster.currentBlockDay).withLocalSigOfRemote(LNParams.keys.extendedNodeKey.privateKey)
        // Tell them not to resolve updates yet even though they may have a cross-signed state because they may also be sending changes
        me UPDATA STORE(hc) SEND nextHC.stateUpdate(isTerminal = false)


      case (hc: HostedCommits, StateUpdate(blockDay, _, remoteUpdates, localSigOfRemoteLCSS, isTerminal), OPEN)
        // GUARD: only proceed if signture is defferent because they will send a few non-terminal duplicates
        if hc.lastCrossSignedState.remoteSigOfLocal != localSigOfRemoteLCSS =>

        val lcss1 = hc.nextLocalUnsignedLCSS(blockDay).copy(remoteSigOfLocal = localSigOfRemoteLCSS).withLocalSigOfRemote(LNParams.keys.extendedNodeKey.privateKey)
        val hc1 = hc.copy(lastCrossSignedState = lcss1, localSpec = hc.nextLocalSpec, futureUpdates = Vector.empty)
        val isRemoteSigOk = lcss1.verifyRemoteSig(hc.announce.nodeId)

        if (remoteUpdates < lcss1.localUpdates) me SEND lcss1.stateUpdate(isTerminal = false)
        else if (me isBlockDayOutOfSync blockDay) localSuspend(hc, ERR_HOSTED_WRONG_BLOCKDAY)
        else if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isTerminal) me SEND lcss1.stateUpdate(isTerminal = true)
        else {
          me UPDATA hc1 SEND lcss1.stateUpdate(isTerminal = true)
          val noResolves = RESOLVE(prevUpds = hc.nextRemoteUpdates, hc)
          if (noResolves) me STORE hc1 else me doProcess CMDProceed
          events onSettled hc1
        }


      case (hc: HostedCommits, cmd: CMDFulfillHtlc, OPEN) =>
        val fulfill = UpdateFulfillHtlc(hc.channelId, cmd.add.id, cmd.preimage)
        val notFound = CommitmentSpec.findHtlcById(hc.localSpec, cmd.add.id, isIncoming = true).isEmpty
        if (notFound) throw new LightningException else me UPDATA hc.addProposal(fulfill.local) SEND fulfill


      case (hc: HostedCommits, cmd: CMDFailHtlc, OPEN) =>
        val fail = UpdateFailHtlc(hc.channelId, cmd.id, cmd.reason)
        val notFound = CommitmentSpec.findHtlcById(hc.localSpec, cmd.id, isIncoming = true).isEmpty
        if (notFound) throw new LightningException else me UPDATA hc.addProposal(fail.local) SEND fail


      case (hc: HostedCommits, cmd: CMDFailMalformedHtlc, OPEN) =>
        val fail = UpdateFailMalformedHtlc(hc.channelId, cmd.id, cmd.onionHash, cmd.code)
        val notFound = CommitmentSpec.findHtlcById(hc.localSpec, cmd.id, isIncoming = true).isEmpty
        if (notFound) throw new LightningException else me UPDATA hc.addProposal(fail.local) SEND fail


      case (hc: HostedCommits, CMDSocketOnline, SLEEPING | SUSPENDED) =>
        if (isChainHeightKnown) me SEND hc.getError.getOrElse(hc.invokeMsg)
        isSocketConnected = true
        permanentOffline = false


      case (hc: HostedCommits, CMDChainTipKnown, SLEEPING | SUSPENDED) =>
        if (isSocketConnected) me SEND hc.getError.getOrElse(hc.invokeMsg)
        isChainHeightKnown = true


      case (hc: HostedCommits, CMDSocketOffline, OPEN) =>
        isSocketConnected = false
        BECOME(hc, SLEEPING)


      case (hc: HostedCommits, init: InitHostedChannel, SLEEPING) =>
        // Remote peer has lost this channel, they should re-sync from our LCSS
        syncAndResend(hc, hc.futureUpdates, hc.lastCrossSignedState, hc.localSpec)


      // Technically they can send a remote LCSS before us explicitly asking for it first
      // this may be a problem if chain height is not yet known and we have incoming HTLCs to resolve
      case (hc: HostedCommits, remoteLCSS: LastCrossSignedState, SLEEPING) if isChainHeightKnown =>
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(LNParams.keys.extendedNodeKey.publicKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(hc.announce.nodeId)
        val weAreAhead = hc.lastCrossSignedState.isAhead(remoteLCSS)
        val weAreEven = hc.lastCrossSignedState.isEven(remoteLCSS)

        if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc, ERR_HOSTED_WRONG_LOCAL_SIG)
        else if (weAreAhead || weAreEven) {
          // They have our current or previous state, resend but keep our state
          syncAndResend(hc, hc.futureUpdates, hc.lastCrossSignedState, hc.localSpec)
        } else hc.findState(remoteLCSS).headOption match {

          case Some(hc1) =>
            val leftOvers = hc.futureUpdates.diff(hc1.futureUpdates)
            // They have our future state, settle on it and resend local leftovers
            val hc2 = syncAndResend(hc1, leftOvers, remoteLCSS.reverse, hc1.nextLocalSpec)
            events onSettled hc2

          case None =>
            // We are far behind, restore state and resolve incoming adds
            val hc1 = restoreCommits(remoteLCSS.reverse, hc.announce)
            events unknownHostedHtlcsDetected hc
            syncFromRestored(hc1)
        }


      case (hc: HostedCommits, newAnn: NodeAnnouncement, _)
        if state != OPEN && hc.announce.nodeId == newAnn.nodeId && Announcements.checkSig(newAnn) =>
        // Node was SLEEPING for a long time, update but do not trigger listeners for this change
        data = me STORE hc.copy(announce = newAnn)


      case (hc: HostedCommits, upd: ChannelUpdate, OPEN | SLEEPING) if waitingUpdate && upd.isHosted =>
        if (me isUpdatable upd) data = me STORE hc.copy(updateOpt = upd.some)
        waitingUpdate = false


      case (hc: HostedCommits, remoteError: Error, WAIT_FOR_ACCEPT | OPEN | SLEEPING) =>
        val hc1 = hc.modify(_.remoteError) setTo Some(remoteError)
        events unknownHostedHtlcsDetected hc
        BECOME(me STORE hc1, SUSPENDED)


      // User has accepted a proposed remote override, now make sure all provided parameters check out
      case (hc: HostedCommits, CMDHostedStateOverride(remoteOverride), SUSPENDED) if isSocketConnected =>
        val localBalance = hc.newLocalBalanceMsat(remoteOverride)

        val restoredLCSS =
          hc.lastCrossSignedState.copy(incomingHtlcs = Nil, outgoingHtlcs = Nil,
            localBalanceMsat = localBalance, remoteBalanceMsat = remoteOverride.localBalanceMsat,
            localUpdates = remoteOverride.remoteUpdates, remoteUpdates = remoteOverride.localUpdates,
            blockDay = remoteOverride.blockDay, remoteSigOfLocal = remoteOverride.localSigOfRemoteLCSS)
            .withLocalSigOfRemote(LNParams.keys.extendedNodeKey.privateKey)

        if (localBalance < 0) throw new LightningException("Provided updated local balance is larger than capacity")
        if (remoteOverride.localUpdates < hc.lastCrossSignedState.remoteUpdates) throw new LightningException("Provided local update number from remote host is wrong")
        if (remoteOverride.remoteUpdates < hc.lastCrossSignedState.localUpdates) throw new LightningException("Provided remote update number from remote host is wrong")
        if (remoteOverride.blockDay < hc.lastCrossSignedState.blockDay) throw new LightningException("Provided override blockday from remote host is not acceptable")
        require(restoredLCSS.verifyRemoteSig(hc.announce.nodeId), "Provided override signature from remote host is wrong")
        BECOME(me STORE restoreCommits(restoredLCSS, hc.announce), OPEN) SEND restoredLCSS.stateUpdate(isTerminal = true)
        // They may send a new StateUpdate right after overriding
        waitingUpdate = true


      case (null, wait: WaitRemoteHostedReply, null) => super.become(wait, WAIT_FOR_INIT)
      case (null, hc: HostedCommits, null) if hc.getError.isDefined => super.become(hc, SUSPENDED)
      case (null, hc: HostedCommits, null) => super.become(hc, SLEEPING)
      case _ =>
    }

    // Change has been processed without failures
    events onProcessSuccess Tuple3(me, data, change)
  }

  def syncAndResend(hc: HostedCommits, leftovers: Vector[LNDirectionalMessage], lcss: LastCrossSignedState, spec: CommitmentSpec) = {
    // Forget about remote, re-send our LCSS and all non-cross-signed local updates, finally sign if local leftovers are indeed present
    val hc1 = hc.copy(futureUpdates = leftovers.filter { case _ \ isLocal => isLocal }, lastCrossSignedState = lcss, localSpec = spec)

    BECOME(hc1, OPEN) SEND hc1.lastCrossSignedState
    for (update <- hc1.nextLocalUpdates) me SEND update
    if (hc1.nextLocalUpdates.nonEmpty) me doProcess CMDProceed
    hc1
  }

  def syncFromRestored(restoredHC: HostedCommits) = {
    // We may have incoming cross-signed HTLCs: resolve them right away
    val adds = for (Htlc(true, add) <- restoredHC.localSpec.htlcs) yield add
    BECOME(restoredHC, OPEN) SEND restoredHC.lastCrossSignedState

    val noResolves = RESOLVE(adds, restoredHC)
    if (noResolves) me STORE restoredHC
    else me doProcess CMDProceed
  }

  def restoreCommits(localLCSS: LastCrossSignedState, ann: NodeAnnouncement) = {
    val inHtlcs = for (updateAddHtlc <- localLCSS.incomingHtlcs) yield Htlc(incoming = true, updateAddHtlc)
    val outHtlcs = for (updateAddHtlc <- localLCSS.outgoingHtlcs) yield Htlc(incoming = false, updateAddHtlc)
    val localSpec = CommitmentSpec(feeratePerKw = 0L, localLCSS.localBalanceMsat, localLCSS.remoteBalanceMsat, htlcs = (inHtlcs ++ outHtlcs).toSet)
    HostedCommits(ann, localLCSS, futureUpdates = Vector.empty, localSpec, updateOpt = None, localError = None, remoteError = None, System.currentTimeMillis)
  }

  def localSuspend(hc: HostedCommits, errCode: ByteVector) = {
    val localError = Error(channelId = hc.channelId, data = errCode)
    val hc1 = hc.modify(_.localError) setTo Some(localError)
    BECOME(me STORE hc1, SUSPENDED) SEND localError
    events unknownHostedHtlcsDetected hc
  }

  def startUp = {
    me process CMDChainTipKnown
    me process CMDSocketOnline
  }
}

trait ChannelListener {
  type Malfunction = (Channel, Throwable)
  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onProcessSuccess: PartialFunction[Incoming, Unit] = none
  def onException: PartialFunction[Malfunction, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none

  def unknownHostedHtlcsDetected(hc: HostedCommits): Unit = none
  def fulfillReceived(upd: UpdateFulfillHtlc): Unit = none
  def outPaymentAccepted(rd: RoutingData): Unit = none
  def onSettled(cs: Commitments): Unit = none
}