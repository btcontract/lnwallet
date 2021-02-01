package com.lightning.walletapp.lnutils

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.helper.{AES, RichCursor}
import fr.acinq.bitcoin.{Crypto, MilliSatoshi, Transaction}
import com.lightning.walletapp.lnutils.olympus.{CerberusAct, OlympusWrap}
import com.lightning.walletapp.{ChannelManager, LNUrl, PayLinkInfo, PayRequest}
import com.lightning.walletapp.ln.wire.LightningMessageCodecs.cerberusPayloadCodec
import com.lightning.walletapp.ln.wire.LightningMessageCodecs
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRoute
import com.lightning.walletapp.ln.crypto.Sphinx.PublicKeyVec
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.walletapp.Utils.app
import scodec.bits.ByteVector


object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  var acceptedPayments = Map.empty[ByteVector, RoutingData]
  var unsentPayments = Map.empty[ByteVector, RoutingData]
  var newRoutesOrGiveUp: RoutingData => Unit = _
  var failOnUI: RoutingData => Unit = _

  def addPendingPayment(rd: RoutingData) = {
    // Add payment to unsentPayments and try to resolve it later
    unsentPayments = unsentPayments.updated(rd.pr.paymentHash, rd)
    me insertOrUpdateOutgoingPayment rd
    resolvePending
    uiNotify
  }

  def resolvePending =
    if (ChannelManager.currentBlocksLeft.isDefined)
      // When uncapable chan becomes online: persists, waits for capable channel
      // When no routes found or any other error happens: gets removed in failOnUI
      // When accepted by channel: gets removed in outPaymentAccepted
      unsentPayments.values foreach fetchAndSend

  def extractPreimage(candidateTx: Transaction) = {
    val fulfills = candidateTx.txIn.map(_.witness.stack) collect {
      case Seq(_, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
      case Seq(_, _, _, pre, _) if pre.size == 32 => UpdateFulfillHtlc(NOCHANID, 0L, pre)
    }

    fulfills foreach updOkOutgoing
    if (fulfills.nonEmpty) uiNotify
  }

  def fetchAndSend(rd: RoutingData) = ChannelManager.fetchRoutes(rd).foreach(ChannelManager.sendEither(_, failOnUI), anyError => me failOnUI rd)
  def updOkIncoming(m: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, m.amountMsat, System.currentTimeMillis, m.channelId, m.paymentHash)
  def updOkOutgoing(m: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, m.paymentPreimage, m.channelId, m.paymentHash)
  def getPaymentInfo(hash: ByteVector) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def updStatus(status: Int, hash: ByteVector) = db.change(PaymentTable.updStatusSql, status, hash)
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(rawQuery: String) = db.search(PaymentTable.searchSql, rawQuery)
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) =
    PaymentInfo(rawPr = rc string PaymentTable.pr, hash = rc string PaymentTable.hash, preimage = rc string PaymentTable.preimage,
      incoming = rc int PaymentTable.incoming, status = rc int PaymentTable.status, stamp = rc long PaymentTable.stamp,
      description = rc string PaymentTable.description, firstMsat = rc long PaymentTable.firstMsat,
      lastMsat = rc long PaymentTable.lastMsat, lastExpiry = rc long PaymentTable.lastExpiry)

  def insertOrUpdateOutgoingPayment(rd: RoutingData) = db txWrap {
    db.change(PaymentTable.updLastParamsOutgoingSql, rd.firstMsat, rd.lastMsat, rd.lastExpiry, rd.pr.paymentHash)
    db.change(PaymentTable.newSql, rd.pr.toJson, NOIMAGE, 0 /* outgoing payment */, WAITING, System.currentTimeMillis,
      rd.description.toJson, rd.pr.paymentHash, rd.firstMsat, rd.lastMsat, rd.lastExpiry, NOCHANID)
  }

  def recordRoutingDataWithPr(extraRoutes: Vector[PaymentRoute], amount: MilliSatoshi,
                              preimage: ByteVector, description: String): RoutingData = {

    val paymentHash = Crypto.sha256(preimage)
    val fakeKey = LNParams.keys.makeFakeKey(paymentHash)

    val pr = PaymentRequest(chainHash, Some(amount), paymentHash, fakeKey, description, None, extraRoutes)
    val rd = app.emptyRD(pr, firstMsat = amount.toLong, useCache = true)

    db.change(PaymentTable.newVirtualSql, rd.queryText, pr.paymentHash)
    db.change(PaymentTable.newSql, pr.toJson, preimage, 1 /* incoming payment */, WAITING, System.currentTimeMillis,
      PaymentDescription(None, description).toJson, pr.paymentHash, amount.toLong, 0L /* lastMsat with fees */,
      0L /* lastExpiry, may later be updated for reflexive payments */, NOCHANID)

    uiNotify
    rd
  }

  def markFailedPayments = db txWrap {
    db.change(PaymentTable.updFailWaitingSql, System.currentTimeMillis - PaymentRequest.expiryTag.seconds * 1000L)
    for (activeInFlightHash <- ChannelManager.activeInFlightHashes) updStatus(WAITING, activeInFlightHash)
  }

  override def unknownHostedHtlcsDetected(hc: HostedCommits) = {
    // Hosted peer is far ahead, we can't know what happened to in-flight payments
    for (htlc <- hc.currentAndNextInFlight) updStatus(FROZEN, htlc.add.paymentHash)
    // Don't enclose this in a transaction becuase we notify UI right away
    uiNotify
  }

  override def outPaymentAccepted(rd: RoutingData) = {
    acceptedPayments = acceptedPayments.updated(rd.pr.paymentHash, rd)
    unsentPayments = unsentPayments - rd.pr.paymentHash
    // Update once again with actual fees data
    me insertOrUpdateOutgoingPayment rd
  }

  override def fulfillReceived(ok: UpdateFulfillHtlc) = db txWrap {
    // Save preimage right away, don't wait for their next commitSig
    me updOkOutgoing ok

    acceptedPayments get ok.paymentHash foreach { rd =>
      val isFeeLow = !isFeeBreach(rd.usedRoute, 1000000000L, percent = 500L)
      db.change(PaymentTable.newVirtualSql, rd.queryText, rd.pr.paymentHash)
      if (rd.usedRoute.nonEmpty && isFeeLow) RouteWrap cacheSubRoutes rd
    }
  }

  override def onSettled(cs: Commitments) = {
    // Mark failed and fulfilled, upload backups

    db txWrap {
      for (updateAddHtlc <- cs.localSpec.fulfilledIncoming) updOkIncoming(updateAddHtlc)
      for (Htlc(false, add) <- cs.localSpec.malformed) updStatus(FAILURE, add.paymentHash)
      for (Htlc(false, add) \ failReason <- cs.localSpec.failed) {

        val rdOpt = acceptedPayments get add.paymentHash
        rdOpt map parseFailureCutRoutes(failReason) match {
          // Try to use reamining routes or fetch new ones if empty
          // but account for possibility of rd not being in place

          case Some(Some(rd1) \ excludes) =>
            for (badEntity <- excludes) BadEntityWrap.putEntity.tupled(badEntity)
            ChannelManager.sendEither(useFirstRoute(rd1.routes, rd1), newRoutesOrGiveUp)

          case _ =>
            // May happen after app has been restarted
            // also when someone sends an unparsable error
            updStatus(FAILURE, add.paymentHash)
        }
      }
    }

    uiNotify
    if (cs.localSpec.fulfilledIncoming.nonEmpty) {
      val vulnerableStates = ChannelManager.all.flatMap(getVulnerableRevVec).toMap
      getCerberusActs(vulnerableStates).foreach(olympusWrap.tellClouds)
    }

    if (cs.localSpec.fulfilled.nonEmpty) {
      com.lightning.walletapp.Vibrator.vibrate
      // This could be a memo-resolving payment
      olympusWrap tellClouds OlympusWrap.CMDStart
    }
  }

  def getVulnerableRevVec(chan: Channel) = chan.getCommits match {
    case Some(normalCommits: NormalCommits) if isOperational(chan) =>
      // Find previous states where amount is lower by more than 10000 SAT
      val amountThreshold = normalCommits.remoteCommit.spec.toRemoteMsat - 10000000L
      val cursor = db.select(RevokedInfoTable.selectLocalSql, normalCommits.channelId, amountThreshold)
      def toTxidAndInfo(rc: RichCursor) = Tuple2(rc string RevokedInfoTable.txId, rc string RevokedInfoTable.info)
      RichCursor(cursor) vec toTxidAndInfo

    case _ =>
      // Hosted channels
      // Closing channges
      Vector.empty
  }

  def getCerberusActs(infos: Map[String, String] = Map.empty) = {
    // Remove currently pending infos and limit max number of uploads
    val notPendingInfos = infos -- olympusWrap.pendingWatchTxIds take 100

    val encrypted = for {
      txid \ revInfo <- notPendingInfos
      txidBytes = ByteVector.fromValidHex(txid).toArray
      revInfoBytes = ByteVector.fromValidHex(revInfo).toArray
      enc = AES.encBytes(revInfoBytes, txidBytes)
    } yield txid -> enc

    for {
      pack <- encrypted grouped 20
      txids \ zygotePayloads = pack.unzip
      halfTxIds = for (txid <- txids) yield txid take 16
      cp = CerberusPayload(zygotePayloads.toVector, halfTxIds.toVector)
      bin = LightningMessageCodecs.serialize(cerberusPayloadCodec encode cp)
    } yield CerberusAct(bin, Nil, "cerberus/watch", txids.toVector)
  }

  override def onProcessSuccess = {
    case (_: NormalChannel, wbr: WaitBroadcastRemoteData, CMDChainTipKnown) if wbr.isLost =>
      // We don't allow manual deletion here as funding may just not be locally visible yet
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript wbr)
      db.change(ChannelTable.killSql, wbr.commitments.channelId)

    case (chan: NormalChannel, norm @ NormalData(_, _, Some(spendTx), _, _), CMDChainTipKnown) =>
      // Must be careful here: unknown spend might be a future commit so only publish local commit if that spend if deeply buried
      // this way a published local commit can not possibly trigger a punishment and will be failed right away with channel becoming CLOSED
      ChannelManager getStatus spendTx.txid match { case depth \ false if depth > blocksPerDay => chan startLocalClose norm case _ => }

    case (_: NormalChannel, close: ClosingData, CMDChainTipKnown) if close.canBeRemoved =>
      // Either a lot of time has passed or ALL closing transactions have enough confirmations
      app.kit.wallet.removeWatchedScripts(app.kit closingPubKeyScripts close)
      app.kit.wallet.removeWatchedScripts(app.kit fundingPubScript close)
      db.change(RevokedInfoTable.killSql, close.commitments.channelId)
      db.change(ChannelTable.killSql, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, _, SLEEPING, OPEN) =>
      // We may have payments waiting
      resolvePending

    case (_, _, WAIT_FUNDING_DONE, OPEN) =>
      // We may have a channel upload act waiting
      olympusWrap tellClouds OlympusWrap.CMDStart
  }
}

object ChannelWrap {
  def doPut(chanId: ByteVector, data: String) = db txWrap {
    // Insert and then update because of INSERT IGNORE effects
    db.change(ChannelTable.newSql, chanId, data)
    db.change(ChannelTable.updSql, data, chanId)
  }

  def put(data: ChannelData) = data match {
    case refund: RefundingData if refund.remoteLatestPoint.isEmpty => Tools log "skipping empty refund"
    case hasNorm: HasNormalCommits => doPut(hasNorm.commitments.channelId, '1' + hasNorm.toJson.toString)
    case hostedCommits: HostedCommits => doPut(hostedCommits.channelId, '2' + hostedCommits.toJson.toString)
    case otherwise => throw new RuntimeException(s"Can not presist this channel data type: $otherwise")
  }

  def doGet(database: LNOpenHelper) =
    RichCursor(database select ChannelTable.selectAllSql).vec(_ string ChannelTable.data) map {
      case rawChanData if '1' == rawChanData.head => to[HasNormalCommits](rawChanData substring 1)
      case rawChanData if '2' == rawChanData.head => to[HostedCommits](rawChanData substring 1)
      case otherwise => throw new RuntimeException(s"Can not deserialize: $otherwise")
    }
}

object RouteWrap {
  def cacheSubRoutes(rd: RoutingData) = {
    // This will only work if we have at least one hop: must check if route vector is empty!
    // then merge each of generated subroutes with a respected routing node or recipient node key
    val subs = (rd.usedRoute drop 1).scanLeft(rd.usedRoute take 1) { case rs \ hop => rs :+ hop }

    for (_ \ node \ path <- rd.onion.sharedSecrets drop 1 zip subs) {
      val pathJson \ nodeString = path.toJson.toString -> node.toString
      db.change(RouteTable.newSql, pathJson, nodeString)
      db.change(RouteTable.updSql, pathJson, nodeString)
    }
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    // Cached routes never expire, but local channels might be closed or excluded
    // so make sure we still have a matching channel for retrieved cached route

    val cursor = db.select(RouteTable.selectSql, targetId)
    val routeTry = RichCursor(cursor).headTry(_ string RouteTable.path) map to[PaymentRoute]
    val validRouteTry = for (rt <- routeTry if from contains rt.head.nodeId) yield Obs just Vector(rt)

    db.change(RouteTable.killSql, targetId)
    // Remove cached route in case if it starts hanging our payments
    // this route will be put back again if payment was a successful one
    validRouteTry getOrElse BadEntityWrap.findRoutes(from, targetId, rd)
  }
}

object BadEntityWrap {
  val putEntity = (entity: String, span: Long, msat: Long) => {
    db.change(BadEntityTable.newSql, entity, System.currentTimeMillis + span, msat)
    db.change(BadEntityTable.updSql, System.currentTimeMillis + span, msat, entity)
  }

  def findRoutes(from: PublicKeyVec, targetId: PublicKey, rd: RoutingData) = {
    val cursor = db.select(BadEntityTable.selectSql, System.currentTimeMillis, rd.firstMsat)
    // Both shortChannelId and nodeId are recorded in a same table for performance reasons, discerned by length
    val badNodes \ badChans = RichCursor(cursor).set(_ string BadEntityTable.resId).partition(_.length > 60)

    val targetStr = targetId.toString
    val fromAsStr = from.map(_.toString).toSet
    val finalBadNodes = badNodes - targetStr -- fromAsStr // Remove source and sink nodes because they may have been blacklisted earlier
    val finalBadChans = badChans.map(_.toLong) ++ rd.expensiveScids // Add not yet blacklisted but overly expensive shortChannelIds
    olympusWrap findRoutes OutRequest(rd.firstMsat / 1000L, finalBadNodes, finalBadChans, fromAsStr, targetStr)
  }
}

object PayMarketWrap {
  def rm(lnUrl: LNUrl) = db.change(PayMarketTable.killSql, lnUrl.request)
  def saveLink(lnUrl: LNUrl, payReq: PayRequest, msat: MilliSatoshi, hash: String) = db txWrap {
    val thumbnailImageString64 = payReq.metaDataImageBase64s.headOption.getOrElse(default = new String)
    db.change(PayMarketTable.updInfoSql, payReq.metaDataTextPlain, msat.toLong, System.currentTimeMillis, hash, thumbnailImageString64, lnUrl.request)
    db.change(PayMarketTable.newSql, lnUrl.request, payReq.metaDataTextPlain, msat.toLong, System.currentTimeMillis, hash, thumbnailImageString64)
    db.change(PayMarketTable.newVirtualSql, s"${lnUrl.uri.getHost} ${payReq.metaDataTextPlain}", lnUrl.request)
  }

  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PayMarketTable.table, null)
  def byQuery(rawQuery: String) = db.search(PayMarketTable.searchSql, rawQuery)
  def byRecent = db select PayMarketTable.selectRecentSql

  def toLinkInfo(rc: RichCursor) =
    PayLinkInfo(image64 = rc string PayMarketTable.image, lnurl = LNUrl(rc string PayMarketTable.lnurl),
      text = rc string PayMarketTable.text, lastMsat = MilliSatoshi(rc long PayMarketTable.lastMsat),
      hash = rc string PayMarketTable.hash, lastDate = rc long PayMarketTable.lastDate)
}