package immortan

import fr.acinq.eclair._
import immortan.Channel._
import immortan.ErrorCodes._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.transactions._

import akka.actor.{ActorRef, Props}
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
  val receiver: ActorRef = LNParams.system actorOf Props(new ActorEventsReceiver)
  def isOutOfSync(blockDay: Long): Boolean = math.abs(blockDay - LNParams.currentBlockDay) > 1

  def doProcess(change: Any): Unit =
    Tuple3(data, change, state) match {
      case (wait: WaitRemoteHostedReply, CMD_SOCKET_ONLINE, WAIT_FOR_INIT) =>
        me SEND InvokeHostedChannel(LNParams.chainHash, wait.refundScriptPubKey, wait.secret)
        BECOME(wait, WAIT_FOR_ACCEPT)


      case (WaitRemoteHostedReply(remoteInfo, refundScriptPubKey, _), init: InitHostedChannel, WAIT_FOR_ACCEPT) =>
        if (init.initialClientBalanceMsat > init.channelCapacityMsat) throw new RuntimeException("Their init balance for us is larger than capacity")
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
        val isBlockDayWrong = isOutOfSync(remoteSU.blockDay)

        if (isBlockDayWrong) throw new RuntimeException("Their blockday is wrong")
        if (!isRemoteSigOk) throw new RuntimeException("Their signature is wrong")
        if (!isRightRemoteUpdateNumber) throw new RuntimeException("Their remote update number is wrong")
        if (!isRightLocalUpdateNumber) throw new RuntimeException("Their local update number is wrong")
        StoreBecomeSend(localHalfSignedHC.copy(lastCrossSignedState = localCompleteLCSS), OPEN)


      case (wait: WaitRemoteHostedReply, remoteLCSS: LastCrossSignedState, WAIT_FOR_ACCEPT) =>
        // We have expected InitHostedChannel but got LastCrossSignedState so this channel exists already
        // make sure our signature match and if so then become OPEN using host supplied state data
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(wait.remoteInfo.nodeSpecificPubKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(wait.remoteInfo.nodeId)
        val hc = restoreCommits(remoteLCSS.reverse, wait.remoteInfo)

        if (!isRemoteSigOk) localSuspend(hc, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc, ERR_HOSTED_WRONG_LOCAL_SIG)
        else {
          // We may have incoming HTLCs to fail or fulfill
          StoreBecomeSend(hc, OPEN, hc.lastCrossSignedState)
          events.stateUpdated(rejects = Nil)
        }

      // CHANNEL IS ESTABLISHED

      case (hc: HostedCommits, add: UpdateAddHtlc, OPEN) =>
        val theirAdd = UpdateAddHtlcExt(add, hc.remoteInfo)
        BECOME(hc.receiveAdd(add), OPEN)
        events.addReceived(theirAdd)


      // Relaxed constraints for receiveng preimages over HCs
      case (hc: HostedCommits, fulfill: UpdateFulfillHtlc, OPEN | SLEEPING) =>
        val (hc1, ourAdd: UpdateAddHtlc) = hc.receiveFulfill(fulfill)
        val filfill = RemoteFulfill(ourAdd, fulfill.paymentPreimage)
        BECOME(data1 = hc1, state1 = state)
        events.fulfillReceived(filfill)


      case (hc: HostedCommits, fail: UpdateFailHtlc, OPEN) =>
        BECOME(hc.receiveFail(fail), OPEN)


      case (hc: HostedCommits, malform: UpdateFailMalformedHtlc, OPEN) =>
        BECOME(hc.receiveFailMalformed(malform), OPEN)


      case (hc: HostedCommits, CMD_SIGN, OPEN) if hc.nextLocalUpdates.nonEmpty || hc.resizeProposal.isDefined =>
        val nextLocalLCSS = hc.resizeProposal.map(hc.withResize).getOrElse(hc).nextLocalUnsignedLCSS(LNParams.currentBlockDay)
        SEND(nextLocalLCSS.withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey).stateUpdate)


      case (hc: HostedCommits, remoteSU: StateUpdate, OPEN) if remoteSU.localSigOfRemoteLCSS != hc.lastCrossSignedState.remoteSigOfLocal =>
        // First attempt a normal state update, then a resized one if signature check fails and we have a pending resize proposal
        attemptStateUpdate(remoteSU, hc)


      case (hc: HostedCommits, cmd: CMD_ADD_HTLC, _) =>
        if (SLEEPING == state) throw CMDException(ChannelOffline, cmd)
        if (OPEN != state) throw CMDException(new RuntimeException, cmd)
        if (hc.getError.isDefined) throw CMDException(new RuntimeException, cmd)
        val (hc1, msg) = hc.sendAdd(cmd, LNParams.blockCount.get)
        StoreBecomeSend(hc1, OPEN, msg)
        process(CMD_SIGN)


      case (_, cmd: CMD_ADD_HTLC, _) =>
        // Omit this channel in any other state
        throw CMDException(new RuntimeException, cmd)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (hc: HostedCommits, cmd: CMD_FULFILL_HTLC, OPEN | SLEEPING) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFulfillHtlc(hc.channelId, cmd.theirAdd.id, cmd.preimage)
        StoreBecomeSend(hc.addLocalProposal(msg), state, msg)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (hc: HostedCommits, cmd: CMD_FAIL_HTLC, OPEN) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = OutgoingPacket.buildHtlcFailure(cmd, theirAdd = cmd.theirAdd)
        StoreBecomeSend(hc.addLocalProposal(msg), state, msg)


      // CMD_SIGN will be sent from ChannelMaster strictly after outgoing FSM sends this command
      case (hc: HostedCommits, cmd: CMD_FAIL_MALFORMED_HTLC, OPEN) if !hc.alreadyReplied(cmd.theirAdd.id) =>
        val msg = UpdateFailMalformedHtlc(hc.channelId, cmd.theirAdd.id, cmd.onionHash, cmd.failureCode)
        StoreBecomeSend(hc.addLocalProposal(msg), state, msg)


      case (hc: HostedCommits, CMD_SOCKET_ONLINE, OPEN | SLEEPING) =>
        val invokeMsg = InvokeHostedChannel(LNParams.chainHash, hc.lastCrossSignedState.refundScriptPubKey, ByteVector.empty)
        SEND(hc.getError getOrElse invokeMsg)


      case (hc: HostedCommits, CMD_SOCKET_OFFLINE, OPEN) => BECOME(hc, SLEEPING)


      case (hc: HostedCommits, _: InitHostedChannel, SLEEPING) => SEND(hc.lastCrossSignedState)


      case (hc: HostedCommits, remoteLCSS: LastCrossSignedState, SLEEPING) =>
        val localLCSS: LastCrossSignedState = hc.lastCrossSignedState // In any case our LCSS is the current one
        val hc1 = hc.resizeProposal.filter(_ isRemoteResized remoteLCSS).map(hc.withResize).getOrElse(hc) // But they may have a resized one
        val weAreEven = localLCSS.remoteUpdates == remoteLCSS.localUpdates && localLCSS.localUpdates == remoteLCSS.remoteUpdates
        val weAreAhead = localLCSS.remoteUpdates > remoteLCSS.localUpdates || localLCSS.localUpdates > remoteLCSS.remoteUpdates
        val isLocalSigOk = remoteLCSS.verifyRemoteSig(hc1.remoteInfo.nodeSpecificPubKey)
        val isRemoteSigOk = remoteLCSS.reverse.verifyRemoteSig(hc1.remoteInfo.nodeId)

        if (!isRemoteSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_REMOTE_SIG)
        else if (!isLocalSigOk) localSuspend(hc1, ERR_HOSTED_WRONG_LOCAL_SIG)
        else if (weAreAhead || weAreEven) {
          SEND(List(localLCSS) ++ hc1.resizeProposal ++ hc1.nextLocalUpdates:_*)
          // Forget about their unsigned updates, they are expected to resend
          BECOME(hc1.copy(nextRemoteUpdates = Nil), OPEN)
          process(CMD_SIGN)
        } else {
          val localUpdatesAcked = remoteLCSS.remoteUpdates - hc1.lastCrossSignedState.localUpdates
          val remoteUpdatesAcked = remoteLCSS.localUpdates - hc1.lastCrossSignedState.remoteUpdates

          val remoteUpdatesAccounted = hc1.nextRemoteUpdates take remoteUpdatesAcked.toInt
          val localUpdatesAccounted = hc1.nextLocalUpdates take localUpdatesAcked.toInt
          val localUpdatesLeftover = hc1.nextLocalUpdates drop localUpdatesAcked.toInt

          val hc2 = hc1.copy(nextLocalUpdates = localUpdatesAccounted, nextRemoteUpdates = remoteUpdatesAccounted)
          val syncedLCSS = hc2.nextLocalUnsignedLCSS(remoteLCSS.blockDay).copy(localSigOfRemote = remoteLCSS.remoteSigOfLocal, remoteSigOfLocal = remoteLCSS.localSigOfRemote)

          if (syncedLCSS.reverse == remoteLCSS) {
            // We have fallen behind a bit but have all the data required to successfully synchronize such that an updated state is reached
            val hc3 = hc2.copy(lastCrossSignedState = syncedLCSS, localSpec = hc2.nextLocalSpec, nextLocalUpdates = localUpdatesLeftover, nextRemoteUpdates = Nil)
            StoreBecomeSend(hc3, OPEN, List(syncedLCSS) ++ hc2.resizeProposal ++ localUpdatesLeftover:_*)
            process(CMD_SIGN)
          } else {
            // We are too far behind, restore from their data
            val hc3 = restoreCommits(remoteLCSS.reverse, hc2.remoteInfo)
            StoreBecomeSend(data1 = hc3, state1 = OPEN, remoteLCSS.reverse)
            events.stateUpdated(fakeFailedOurAdds(hc1, hc3).toList)
          }
        }


      case (hc: HostedCommits, upd: ChannelUpdate, OPEN | SLEEPING) if hc.updateOpt.forall(_.timestamp < upd.timestamp) =>
        val shortIdMatches = hostedShortChanId(hc.remoteInfo.nodeSpecificPubKey.value, hc.remoteInfo.nodeId.value) == upd.shortChannelId
        if (shortIdMatches) data = me STORE hc.copy(updateOpt = upd.toSome)


      case (hc: HostedCommits, cmd: HC_CMD_RESIZE, OPEN | SLEEPING) if hc.resizeProposal.isEmpty =>
        val capacitySat = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat.truncateToSatoshi
        val resize = ResizeChannel(capacitySat + cmd.delta).sign(hc.remoteInfo.nodeSpecificPrivKey)
        StoreBecomeSend(hc.copy(resizeProposal = resize.toSome), state, resize)
        process(CMD_SIGN)


      case (hc: HostedCommits, resize: ResizeChannel, OPEN | SLEEPING) if hc.resizeProposal.isEmpty =>
        // Can happen if we have sent a resize earlier, but then lost channel data and restored from their
        val isLocalSigOk: Boolean = resize.verifyClientSig(hc.remoteInfo.nodeSpecificPubKey)
        if (isLocalSigOk) StoreBecomeSend(hc.copy(resizeProposal = resize.toSome), state)
        else localSuspend(hc, ERR_HOSTED_INVALID_RESIZE)


      case (hc: HostedCommits, remoteError: Error, WAIT_FOR_ACCEPT | OPEN) if hc.remoteError.isEmpty =>
        StoreBecomeSend(hc.copy(remoteError = remoteError.toSome), OPEN)


      case (hc: HostedCommits, remoteSO: StateOverride, OPEN | SLEEPING) if hc.getError.isDefined =>
        StoreBecomeSend(hc.copy(overrideProposal = remoteSO.toSome), state)


      case (hc: HostedCommits, cmd @ CMD_HOSTED_STATE_OVERRIDE(remoteSO), OPEN | SLEEPING) if hc.getError.isDefined =>
        val overriddenLocalBalance = hc.lastCrossSignedState.initHostedChannel.channelCapacityMsat - remoteSO.localBalanceMsat
        val completeLocalLCSS = hc.lastCrossSignedState.copy(incomingHtlcs = Nil, outgoingHtlcs = Nil, localBalanceMsat = overriddenLocalBalance,
          remoteBalanceMsat = remoteSO.localBalanceMsat, localUpdates = remoteSO.remoteUpdates, remoteUpdates = remoteSO.localUpdates, blockDay = remoteSO.blockDay,
          remoteSigOfLocal = remoteSO.localSigOfRemoteLCSS).withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)

        val isRemoteSigOk = completeLocalLCSS.verifyRemoteSig(hc.remoteInfo.nodeId)
        val hc1 = restoreCommits(completeLocalLCSS, hc.remoteInfo)

        if (completeLocalLCSS.localBalanceMsat < 0L.msat) throw CMDException(new RuntimeException("Provided updated local balance is larger than capacity"), cmd)
        if (remoteSO.localUpdates < hc.lastCrossSignedState.remoteUpdates) throw CMDException(new RuntimeException("Provided local update number from remote host is wrong"), cmd)
        if (remoteSO.remoteUpdates < hc.lastCrossSignedState.localUpdates) throw CMDException(new RuntimeException("Provided remote update number from remote host is wrong"), cmd)
        if (remoteSO.blockDay < hc.lastCrossSignedState.blockDay) throw CMDException(new RuntimeException("Provided override blockday from remote host is not acceptable"), cmd)
        if (!isRemoteSigOk) throw CMDException(new RuntimeException("Provided override signature from remote host is wrong"), cmd)
        StoreBecomeSend(hc1, OPEN, completeLocalLCSS.stateUpdate)
        events.stateUpdated(fakeFailedOurAdds(hc, hc1).toList)


      case (null, wait: WaitRemoteHostedReply, null) => super.become(wait, WAIT_FOR_INIT)
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
    StoreBecomeSend(hc1, state, localError)
  }

  def fakeFailedOurAdds(hc: HostedCommits, hc1: HostedCommits): Set[RemoteUpdateFail] =
    // When overriding or synchronizing with remote state from future we may have local outgoing adds which need to be failed in outgoing FSM
    for (ourAdd <- hc.allOutgoing -- hc1.allOutgoing) yield RemoteUpdateFail(UpdateFailHtlc(hc.channelId, ourAdd.id, reason = ByteVector.empty), ourAdd)

  def attemptStateUpdate(remoteSU: StateUpdate, hc: HostedCommits): Unit = {
    val lcss1 = hc.nextLocalUnsignedLCSS(remoteSU.blockDay).copy(remoteSigOfLocal = remoteSU.localSigOfRemoteLCSS).withLocalSigOfRemote(hc.remoteInfo.nodeSpecificPrivKey)
    val hc1 = hc.copy(lastCrossSignedState = lcss1, localSpec = hc.nextLocalSpec, nextLocalUpdates = Nil, nextRemoteUpdates = Nil)
    val isRemoteSigOk = lcss1.verifyRemoteSig(hc.remoteInfo.nodeId)
    val isBlockDayWrong = isOutOfSync(remoteSU.blockDay)

    if (isBlockDayWrong) {
      localSuspend(hc, ERR_HOSTED_WRONG_BLOCKDAY)
    } else if (remoteSU.remoteUpdates < lcss1.localUpdates) {
      // Persist unsigned remote updates to use them on re-sync
      // we do not update runtime data because ours is newer one
      process(CMD_SIGN)
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

      // First persist a new state, then call an event
      StoreBecomeSend(hc1, OPEN, lcss1.stateUpdate)
      events.stateUpdated(lastRemoteRejects)
    }
  }
}
