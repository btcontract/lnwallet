package immortan

import fr.acinq.eclair._
import immortan.Channel._
import immortan.ErrorCodes._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.payment.OutgoingPacket
import fr.acinq.bitcoin.ByteVector64
import fr.acinq.bitcoin.SatoshiLong
import scodec.bits.ByteVector


object ChannelHosted {
  def make(initListeners: Set[ChannelListener], hostedData: HostedCommits, bag: ChannelBag): ChannelHosted = new ChannelHosted {
    def SEND(msgs: LightningMessage*): Unit = CommsTower.sendMany(msgs.map(LightningMessageCodecs.prepareNormal), hostedData.remoteInfo.nodeSpecificPair)
    def STORE(hostedData: PersistentChannelData): PersistentChannelData = bag.put(hostedData)
    listeners = initListeners
    doProcess(hostedData)
  }
}

abstract class ChannelHosted extends Channel { me =>
  def isBlockDayOutOfSync(currentBlockDay: Long): Boolean =
    math.abs(currentBlockDay - LNParams.currentBlockDay) > 1

  def doProcess(change: Any): Unit =
    Tuple3(data, change, state) match {
      case (wait: WaitRemoteHostedReply, CMD_SOCKET_ONLINE, WAIT_FOR_INIT) =>
        me SEND InvokeHostedChannel(LNParams.chainHash, wait.refundScriptPubKey, wait.secret)
        BECOME(wait, WAIT_FOR_ACCEPT)


      case (WaitRemoteHostedReply(remoteInfo, refundScriptPubKey, _), init: InitHostedChannel, WAIT_FOR_ACCEPT) =>
        if (init.liabilityDeadlineBlockdays < LNParams.minHostedLiabilityBlockdays) throw new RuntimeException("Their liability deadline is too low")
        if (init.initialClientBalanceMsat > init.channelCapacityMsat) throw new RuntimeException("Their init balance for us is larger than capacity")
        if (init.minimalOnchainRefundAmountSatoshis > LNParams.minHostedOnChainRefund) throw new RuntimeException("Their min refund is too high")
        if (init.channelCapacityMsat < LNParams.minHostedOnChainRefund) throw new RuntimeException("Their proposed channel capacity is too low")
        if (UInt64(100000000L) > init.maxHtlcValueInFlightMsat) throw new RuntimeException("Their max value in-flight is too low")
        if (init.htlcMinimumMsat > 546000L.msat) throw new RuntimeException("Their minimal payment size is too high")
        if (init.maxAcceptedHtlcs < 1) throw new RuntimeException("They can accept too few payments")

        val localHalfSignedHC =
          restoreCommits(LastCrossSignedState(isHost = false, refundScriptPubKey, init, LNParams.currentBlockDay, init.initialClientBalanceMsat,
            init.channelCapacityMsat - init.initialClientBalanceMsat, localUpdates = 0L, remoteUpdates = 0L, incomingHtlcs = Nil, outgoingHtlcs = Nil,
            localSigOfRemote = ByteVector64.Zeroes, remoteSigOfLocal = ByteVector64.Zeroes).withLocalSigOfRemote(remoteInfo.nodeSpecificPrivKey), remoteInfo)

        BECOME(WaitRemoteHostedStateUpdate(remoteInfo, localHalfSignedHC), WAIT_FOR_ACCEPT)
        SEND(localHalfSignedHC.lastCrossSignedState.stateUpdate)


      case (WaitRemoteHostedStateUpdate(_, localHalfSignedHC), remoteSU: StateUpdate, WAIT_FOR_ACCEPT) =>
        val localCompleteLCSS = localHalfSignedHC.lastCrossSignedState.copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS)
        val isRightRemoteUpdateNumber = localHalfSignedHC.lastCrossSignedState.remoteUpdates == remoteSU.localUpdates
        val isRightLocalUpdateNumber = localHalfSignedHC.lastCrossSignedState.localUpdates == remoteSU.remoteUpdates
        val isRemoteSigOk = localCompleteLCSS.verifyRemoteSig(localHalfSignedHC.remoteInfo.nodeId)
        val isBlockDayWrong = isBlockDayOutOfSync(remoteSU.blockDay)

        if (isBlockDayWrong) throw new RuntimeException("Their blockday is wrong")
        if (!isRemoteSigOk) throw new RuntimeException("Their signature is wrong")
        if (!isRightRemoteUpdateNumber) throw new RuntimeException("Their remote update number is wrong")
        if (!isRightLocalUpdateNumber) throw new RuntimeException("Their local update number is wrong")
        BECOME(me STORE localHalfSignedHC.copy(lastCrossSignedState = localCompleteLCSS), OPEN)


      case (wait: WaitRemoteHostedReply, remoteLCSS: LastCrossSignedState, WAIT_FOR_ACCEPT) =>
        // We have expected InitHostedChannel but got LastCrossSignedState so this channel exists already
        // make sure our signature match and if so then become OPEN using host supplied state data
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(wait.remoteInfo.nodeSpecificPubKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(wait.remoteInfo.nodeId)
        val hc = restoreCommits(remoteLCSS.reverse, wait.remoteInfo)

        if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc, ERR_HOSTED_WRONG_LOCAL_SIG)
        else {
          StoreBecomeSend(hc, OPEN, hc.lastCrossSignedState)
          // We may have incoming HTLCs to fail or fulfill
          events.stateUpdated(Nil)
        }

      // CHANNEL IS ESTABLISHED

      case (hc: HostedCommits, add: UpdateAddHtlc, OPEN) =>
        val theirAdd = UpdateAddHtlcExt(add, hc.remoteInfo)
        BECOME(hc.receiveAdd(add), OPEN)
        events.addReceived(theirAdd)


      // Relaxed constraints for receiveng preimages over HCs
      case (hc: HostedCommits, msg: UpdateFulfillHtlc, OPEN | SUSPENDED) =>
        val ourAdd = hc.nextLocalSpec.findOutgoingHtlcById(msg.id).get.add
        val filfill = RemoteFulfill(msg.paymentPreimage, ourAdd)
        BECOME(hc.addRemoteProposal(msg), state)
        events.fulfillReceived(filfill)


      case (hc: HostedCommits, fail: UpdateFailHtlc, OPEN) =>
        require(hc.localSpec.findOutgoingHtlcById(fail.id).isDefined)
        BECOME(hc.addRemoteProposal(fail), OPEN)


      case (hc: HostedCommits, malform: UpdateFailMalformedHtlc, OPEN) =>
        require(0 == (malform.failureCode & FailureMessageCodecs.BADONION), "wrong bad onion code")
        require(hc.localSpec.findOutgoingHtlcById(malform.id).isDefined)
        BECOME(hc.addRemoteProposal(malform), OPEN)


      case (hc: HostedCommits, CMD_SIGN, OPEN) if hc.nextLocalUpdates.nonEmpty || hc.resizeProposal.isDefined =>
        val nextLocalLCSS = hc.resizeProposal.map(hc.withResize).getOrElse(hc).nextLocalUnsignedLCSS(LNParams.currentBlockDay)
        SEND(nextLocalLCSS.withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey).stateUpdate)


      // CMD_SIGN will be sent by ChannelMaster
      case (hc: HostedCommits, remoteSU: StateUpdate, OPEN) if hc.lastCrossSignedState.remoteSigOfLocal != remoteSU.localSigOfRemoteLCSS =>
        // First attempt a normal state update, then a resized one if signature check fails and we have a pending resize proposal
        attemptStateUpdate(remoteSU, hc)


      case (hc: HostedCommits, cmd: CMD_ADD_HTLC, OPEN) =>
        val (hc1, msg) = hc.sendAdd(cmd, LNParams.blockCount.get)
        StoreBecomeSend(hc1, OPEN, msg)
        doProcess(CMD_SIGN)


      case (_: HostedCommits, cmd: CMD_ADD_HTLC, SLEEPING) =>
        // Instruct payment master to not omit this channel yet
        throw CMDException(ChannelOffline, cmd)


      case (_: HostedCommits, cmd: CMD_ADD_HTLC, _) =>
        // Instruct payment master to omit this channel
        throw CMDException(new RuntimeException, cmd)


      case (hc: HostedCommits, cmd: CMD_FULFILL_HTLC, OPEN) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFulfillHtlc(hc.channelId, cmd.theirAdd.id, cmd.preimage)
        StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


      case (hc: HostedCommits, cmd: CMD_FAIL_HTLC, OPEN) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = OutgoingPacket.buildHtlcFailure(cmd, theirAdd = cmd.theirAdd)
        StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


      case (hc: HostedCommits, cmd: CMD_FAIL_MALFORMED_HTLC, OPEN) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFailMalformedHtlc(hc.channelId, cmd.theirAdd.id, cmd.onionHash, cmd.failureCode)
        StoreBecomeSend(hc.addLocalProposal(msg), OPEN, msg)


      case (hc: HostedCommits, CMD_SOCKET_ONLINE, SLEEPING | SUSPENDED) =>
        val invokeMsg = InvokeHostedChannel(LNParams.chainHash, hc.lastCrossSignedState.refundScriptPubKey, ByteVector.empty)
        SEND(hc.getError getOrElse invokeMsg)


      case (hc: HostedCommits, CMD_SOCKET_OFFLINE, OPEN) => BECOME(hc, SLEEPING)


      case (hc: HostedCommits, _: InitHostedChannel, SLEEPING) =>
        // Peer has lost this channel, they may re-sync from our LCSS
        SEND(hc.lastCrossSignedState)


      // CMD_SIGN will be sent by ChannelMaster
      case (hc: HostedCommits, remoteLCSS: LastCrossSignedState, SLEEPING) =>
        val localLCSS: LastCrossSignedState = hc.lastCrossSignedState // In any case our LCSS is the current one
        val hc1 = hc.resizeProposal.filter(_ isRemoteResized remoteLCSS).map(hc.withResize).getOrElse(hc) // But they may have a resized one
        val weAreEven = localLCSS.remoteUpdates == remoteLCSS.localUpdates && localLCSS.localUpdates == remoteLCSS.remoteUpdates
        val weAreAhead = localLCSS.remoteUpdates > remoteLCSS.localUpdates || localLCSS.localUpdates > remoteLCSS.remoteUpdates
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(hc.remoteInfo.nodeSpecificPubKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(hc.remoteInfo.nodeId)

        if (!isRemoteSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_LOCAL_SIG)
        else if (weAreAhead || weAreEven) {
          SEND(List(localLCSS) ++ hc1.resizeProposal ++ hc1.nextLocalUpdates:_*)
          BECOME(hc1.copy(nextRemoteUpdates = Nil), OPEN)
        } else {
          val localUpdatesAcked = remoteLCSS.remoteUpdates - hc1.lastCrossSignedState.localUpdates
          val remoteUpdatesAcked = remoteLCSS.localUpdates - hc1.lastCrossSignedState.remoteUpdates

          val remoteUpdatesAccounted = hc1.nextRemoteUpdates take remoteUpdatesAcked.toInt
          val localUpdatesAccounted = hc1.nextLocalUpdates take localUpdatesAcked.toInt
          val localUpdatesLeftover = hc1.nextLocalUpdates drop localUpdatesAcked.toInt

          val hc2 = hc1.copy(nextLocalUpdates = localUpdatesAccounted, nextRemoteUpdates = remoteUpdatesAccounted)
          val syncedLCSS = hc2.nextLocalUnsignedLCSS(remoteLCSS.blockDay).copy(localSigOfRemote = remoteLCSS.remoteSigOfLocal, remoteSigOfLocal = remoteLCSS.localSigOfRemote)
          val syncedCommits = hc2.copy(lastCrossSignedState = syncedLCSS, localSpec = hc2.nextLocalSpec, nextLocalUpdates = localUpdatesLeftover, nextRemoteUpdates = Nil)
          if (syncedLCSS.reverse != remoteLCSS) StoreBecomeSend(restoreCommits(remoteLCSS.reverse, hc2.remoteInfo), OPEN, remoteLCSS.reverse) // We are too far behind, restore from their data
          else StoreBecomeSend(syncedCommits, OPEN, List(syncedLCSS) ++ hc2.resizeProposal ++ localUpdatesLeftover:_*) // We are behind but our own future cross-signed state is reachable
        }


      case (hc: HostedCommits, upd: ChannelUpdate, OPEN | SLEEPING) if hc.updateOpt.forall(_.timestamp < upd.timestamp) =>
        val shortIdMatches = hostedShortChanId(hc.remoteInfo.nodeSpecificPubKey.value, hc.remoteInfo.nodeId.value) == upd.shortChannelId
        if (shortIdMatches) data = me STORE hc.copy(updateOpt = upd.toSome)


      case (hc: HostedCommits, cmd: HC_CMD_RESIZE, OPEN | SLEEPING) if hc.resizeProposal.isEmpty =>
        val capacitySat = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat.truncateToSatoshi
        val resize = ResizeChannel(capacitySat + cmd.delta).sign(hc.remoteInfo.nodeSpecificPrivKey)
        StoreBecomeSend(hc.copy(resizeProposal = resize.toSome), state, resize)
        doProcess(CMD_SIGN)


      case (hc: HostedCommits, resize: ResizeChannel, OPEN | SLEEPING) if hc.resizeProposal.isEmpty =>
        // Can happen if we have sent a resize earlier, but then lost channel data and restored from their
        val isLocalSigOk = resize.verifyClientSig(hc.remoteInfo.nodeSpecificPubKey)
        if (isLocalSigOk) me STORE hc.copy(resizeProposal = resize.toSome)
        else localSuspend(hc, ERR_HOSTED_INVALID_RESIZE)


      case (hc: HostedCommits, remoteError: Error, WAIT_FOR_ACCEPT | OPEN | SLEEPING) if hc.remoteError.isEmpty =>
        BECOME(me STORE hc.copy(remoteError = remoteError.toSome), SUSPENDED)


      case (hc: HostedCommits, CMD_HOSTED_STATE_OVERRIDE(remoteSO), SUSPENDED) =>
        // User has manually accepted a proposed remote override, now make sure all remote-provided parameters check out
        val localBalance: MilliSatoshi = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat - remoteSO.localBalanceMsat

        val completeLocalLCSS =
          hc.lastCrossSignedState.copy(incomingHtlcs = Nil, outgoingHtlcs = Nil, localBalanceMsat = localBalance, remoteBalanceMsat = remoteSO.localBalanceMsat,
            localUpdates = remoteSO.remoteUpdates, remoteUpdates = remoteSO.localUpdates, blockDay = remoteSO.blockDay, remoteSigOfLocal = remoteSO.localSigOfRemoteLCSS)
            .withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)

        if (localBalance < 0L.msat) throw new RuntimeException("Provided updated local balance is larger than capacity")
        if (remoteSO.localUpdates < hc.lastCrossSignedState.remoteUpdates) throw new RuntimeException("Provided local update number from remote host is wrong")
        if (remoteSO.remoteUpdates < hc.lastCrossSignedState.localUpdates) throw new RuntimeException("Provided remote update number from remote host is wrong")
        if (remoteSO.blockDay < hc.lastCrossSignedState.blockDay) throw new RuntimeException("Provided override blockday from remote host is not acceptable")
        require(completeLocalLCSS.verifyRemoteSig(hc.remoteInfo.nodeId), "Provided override signature from remote host is wrong")
        StoreBecomeSend(restoreCommits(completeLocalLCSS, hc.remoteInfo), OPEN, completeLocalLCSS.stateUpdate)

      case (null, wait: WaitRemoteHostedReply, null) => super.become(wait, WAIT_FOR_INIT)
      case (null, hc: HostedCommits, null) if hc.getError.isDefined => super.become(hc, SUSPENDED)
      case (null, hc: HostedCommits, null) => super.become(hc, SLEEPING)
      case _ =>
    }

  def restoreCommits(localLCSS: LastCrossSignedState, remoteInfo: RemoteNodeInfo): HostedCommits = {
    val inFlightHtlcs = localLCSS.incomingHtlcs.map(IncomingHtlc) ++ localLCSS.outgoingHtlcs.map(OutgoingHtlc)
    val localSpec = CommitmentSpec(FeeratePerKw(0L.sat), localLCSS.localBalanceMsat, localLCSS.remoteBalanceMsat, htlcs = inFlightHtlcs.toSet)
    HostedCommits(remoteInfo, localLCSS, nextLocalUpdates = Nil, nextRemoteUpdates = Nil, localSpec, updateOpt = None, localError = None, remoteError = None)
  }

  def localSuspend(hc: HostedCommits, errCode: String): Unit = {
    val localError = Error(hc.channelId, ByteVector fromValidHex errCode)
    val hc1 = if (hc.localError.isDefined) hc else hc.copy(localError = localError.toSome)
    StoreBecomeSend(hc1, SUSPENDED, localError)
  }

  def attemptStateUpdate(remoteSU: StateUpdate, hc: HostedCommits): Unit = {
    val lcss1 = hc.nextLocalUnsignedLCSS(remoteSU.blockDay).copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS).withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)
    val hc1 = hc.copy(lastCrossSignedState = lcss1, localSpec = hc.nextLocalSpec, nextLocalUpdates = Nil, nextRemoteUpdates = Nil)
    val isRemoteSigOk = lcss1.verifyRemoteSig(hc.remoteInfo.nodeId)
    val isBlockDayWrong = isBlockDayOutOfSync(remoteSU.blockDay)

    if (isBlockDayWrong) {
      localSuspend(hc, ERR_HOSTED_WRONG_BLOCKDAY)
    } else if (remoteSU.remoteUpdates < lcss1.localUpdates) {
      // Persist unsigned remote updates to use them on re-sync
      doProcess(CMD_SIGN)
      me STORE hc
    } else if (!isRemoteSigOk) {
      hc.resizeProposal.map(hc.withResize) match {
        case Some(resizedHC) => attemptStateUpdate(remoteSU, resizedHC)
        case None => localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
      }
    } else {
      val lastRemoteRejects: Seq[RemoteReject] = hc.nextRemoteUpdates.collect {
        case fail: UpdateFailHtlc => RemoteUpdateFail(fail, hc.localSpec.findOutgoingHtlcById(fail.id).get.add)
        case malform: UpdateFailMalformedHtlc => RemoteUpdateMalform(malform, hc.localSpec.findOutgoingHtlcById(malform.id).get.add)
      }

      StoreBecomeSend(hc1, OPEN, lcss1.stateUpdate)
      events.stateUpdated(lastRemoteRejects)
    }
  }
}