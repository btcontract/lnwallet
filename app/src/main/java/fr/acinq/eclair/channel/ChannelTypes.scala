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

import scodec.bits._
import fr.acinq.eclair._
import fr.acinq.bitcoin._
import fr.acinq.eclair.wire._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import scodec.bits.{BitVector, ByteVector}
import immortan.{LNParams, RemoteNodeInfo}
import fr.acinq.eclair.blockchain.MakeFundingTxResponse
import fr.acinq.eclair.crypto.Sphinx.PacketAndSecrets
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.crypto.Generators
import fr.acinq.eclair.transactions.{CommitmentSpec, Transactions}
import fr.acinq.eclair.wire.Onion.FinalPayload
import fr.acinq.eclair.payment.IncomingPacket
import immortan.crypto.Tools


// Fatal by deafult
case class FeerateTooSmall(channelId: ByteVector32, remoteFeeratePerKw: FeeratePerKw) extends RuntimeException {
  override def toString: String = s"FeerateTooSmall, remoteFeeratePerKw=$remoteFeeratePerKw"
}

case class DustLimitTooSmall(channelId: ByteVector32, dustLimit: Satoshi, min: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitTooSmall, dustLimit=$dustLimit, min=$min"
}

case class DustLimitTooLarge(channelId: ByteVector32, dustLimit: Satoshi, max: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitTooLarge, dustLimit=$dustLimit, max=$max"
}

case class InvalidMaxAcceptedHtlcs(channelId: ByteVector32, maxAcceptedHtlcs: Int, max: Int) extends RuntimeException {
  override def toString: String = s"InvalidMaxAcceptedHtlcs, maxAcceptedHtlcs=$maxAcceptedHtlcs, max=$max"
}

case class InvalidChainHash(channelId: ByteVector32, local: ByteVector32, remote: ByteVector32) extends RuntimeException {
  override def toString: String = s"InvalidChainHash, local=$local, remote=$remote"
}

case class InvalidPushAmount(channelId: ByteVector32, pushAmount: MilliSatoshi, max: MilliSatoshi) extends RuntimeException {
  override def toString: String = s"InvalidPushAmount, pushAmount=$pushAmount, max=$max"
}

case class ToSelfDelayTooHigh(channelId: ByteVector32, toSelfDelay: CltvExpiryDelta, max: CltvExpiryDelta) extends RuntimeException {
  override def toString: String = s"ToSelfDelayTooHigh, toSelfDelay=$toSelfDelay, max=$max"
}

case class InvalidFundingAmount(channelId: ByteVector32, fundingAmount: Satoshi, min: Satoshi, max: Satoshi) extends RuntimeException {
  override def toString: String = s"InvalidFundingAmount, fundingAmount=$fundingAmount, min=$min, max=$max"
}

case class DustLimitAboveOurChannelReserve(channelId: ByteVector32, dustLimit: Satoshi, channelReserve: Satoshi) extends RuntimeException {
  override def toString: String = s"DustLimitAboveOurChannelReserve, dustLimit=$dustLimit, channelReserve=$channelReserve"
}

case class ChannelReserveBelowOurDustLimit(channelId: ByteVector32, channelReserve: Satoshi, dustLimit: Satoshi) extends RuntimeException {
  override def toString: String = s"ChannelReserveBelowOurDustLimit, channelReserve=$channelReserve, dustLimit=$dustLimit"
}

case class ChannelReserveNotMet(channelId: ByteVector32, toLocal: MilliSatoshi, toRemote: MilliSatoshi, reserve: Satoshi) extends RuntimeException {
  override def toString: String = s"ChannelReserveNotMet, toLocal=$toLocal, toRemote=$toRemote, reserve=$reserve"
}

case class FeerateTooDifferent(channelId: ByteVector32, localFeeratePerKw: FeeratePerKw, remoteFeeratePerKw: FeeratePerKw) extends RuntimeException {
  override def toString: String = s"FeerateTooDifferent, localFeeratePerKw=$localFeeratePerKw, remoteFeeratePerKw=$remoteFeeratePerKw"
}

case class ChannelReserveTooHigh(channelId: ByteVector32, reserveToFundingRatio: Double, maxReserveToFundingRatio: Double) extends RuntimeException {
  override def toString: String = s"DustLimitTooSmall, reserveToFundingRatio=$reserveToFundingRatio, maxReserveToFundingRatio=$maxReserveToFundingRatio"
}

case class ChannelTransitionFail(channelId: ByteVector32) extends RuntimeException {
  override def toString: String = s"ChannelTransitionFail"
}

// Non-fatal by default
case object ChannelOffline extends RuntimeException
case object InPrincipleNotSendable extends RuntimeException
case class CMDException(error: RuntimeException, cmd: Command) extends RuntimeException


case class INPUT_INIT_FUNDER(remoteInfo: RemoteNodeInfo, temporaryChannelId: ByteVector32, fakeFunding: MakeFundingTxResponse, pushAmount: MilliSatoshi,
                             fundingFeeratePerKw: FeeratePerKw, initialFeeratePerKw: FeeratePerKw, localParams: LocalParams, remoteInit: Init,
                             channelFlags: Byte, channelVersion: ChannelVersion)

case class INPUT_INIT_FUNDEE(remoteInfo: RemoteNodeInfo, localParams: LocalParams, remoteInit: Init, channelVersion: ChannelVersion, theirOpen: OpenChannel)

sealed trait BitcoinEvent
case class BITCOIN_PARENT_TX_CONFIRMED(childTx: Transaction) extends BitcoinEvent
case class BITCOIN_TX_CONFIRMED(tx: Transaction) extends BitcoinEvent
case object BITCOIN_FUNDING_DEPTHOK extends BitcoinEvent
case object BITCOIN_FUNDING_SPENT extends BitcoinEvent
case object BITCOIN_OUTPUT_SPENT extends BitcoinEvent


sealed trait Command

sealed trait IncomingResolution

sealed trait ReasonableResolution extends IncomingResolution {
  val fullTag: FullPaymentTag // Payment type and grouping data (paymentHash x paymentSecret x type)
  val secret: PrivateKey // Node secret whose pubKey is seen by peer (might be peer-specific or invoice-specific)
  val add: UpdateAddHtlc
}

case class ReasonableTrampoline(packet: IncomingPacket.NodeRelayPacket, secret: PrivateKey) extends ReasonableResolution {
  val fullTag: FullPaymentTag = FullPaymentTag(packet.add.paymentHash, packet.outerPayload.paymentSecret.get, PaymentTagTlv.TRAMPLOINE_ROUTED)
  val add: UpdateAddHtlc = packet.add
}

case class ReasonableLocal(packet: IncomingPacket.FinalPacket, secret: PrivateKey) extends ReasonableResolution {
  val fullTag: FullPaymentTag = FullPaymentTag(packet.add.paymentHash, packet.payload.paymentSecret.get, PaymentTagTlv.FINAL_INCOMING)
  val add: UpdateAddHtlc = packet.add
}

sealed trait FinalResolution extends IncomingResolution { val theirAdd: UpdateAddHtlc }

case class CMD_FAIL_HTLC(reason: Either[ByteVector, FailureMessage], nodeSecret: PrivateKey, theirAdd: UpdateAddHtlc) extends Command with FinalResolution

case class CMD_FAIL_MALFORMED_HTLC(onionHash: ByteVector32, failureCode: Int, theirAdd: UpdateAddHtlc) extends Command with FinalResolution

case class CMD_FULFILL_HTLC(preimage: ByteVector32, theirAdd: UpdateAddHtlc) extends Command with FinalResolution

case class CMD_ADD_HTLC(fullTag: FullPaymentTag, firstAmount: MilliSatoshi, cltvExpiry: CltvExpiry, packetAndSecrets: PacketAndSecrets, payload: FinalPayload) extends Command {
  final val partId: ByteVector = packetAndSecrets.packet.publicKey

  lazy val encryptedTag: ByteVector = {
    // Important: LNParams.format must be defined
    val shortTag = ShortPaymentTag(fullTag.paymentSecret, fullTag.tag)
    val plainBytes = PaymentTagTlv.shortPaymentTagCodec.encode(shortTag).require.toByteVector
    Tools.chaChaEncrypt(LNParams.secret.keys.ourNodePrivateKey.value, randomBytes(12), plainBytes)
  }
}

case object CMD_CHECK_FEERATE extends Command
case class CMD_UPDATE_FEERATE(channelId: ByteVector32, feeratePerKw: FeeratePerKw) extends Command {
  val ourFeeRatesUpdate: UpdateFee = UpdateFee(channelId, feeratePerKw)
}

case class CMD_HOSTED_STATE_OVERRIDE(so: StateOverride) extends Command
case class HC_CMD_RESIZE(delta: Satoshi) extends Command
case object CMD_SOCKET_OFFLINE extends Command
case object CMD_SOCKET_ONLINE extends Command
case object CMD_SIGN extends Command

sealed trait CloseCommand extends Command
case object CMD_FORCECLOSE extends CloseCommand
final case class CMD_CLOSE(scriptPubKey: Option[ByteVector] = None) extends CloseCommand


trait ChannelData

trait PersistentChannelData extends ChannelData {
  def channelId: ByteVector32
}

sealed trait HasNormalCommitments extends PersistentChannelData {
  override def channelId: ByteVector32 = commitments.channelId
  def commitments: NormalCommits
}

case class ClosingTxProposed(unsignedTx: Transaction, localClosingSigned: ClosingSigned)

sealed trait ForceCloseCommitPublished {
  lazy val irrevocablySpentTxIds: Set[ByteVector32] = irrevocablySpent.values.toSet
  lazy val isCommitConfirmed: Boolean = irrevocablySpentTxIds contains commitTx.txid

  val irrevocablySpent: Map[OutPoint, ByteVector32]
  val delayedRefundsLeft: Seq[Transaction]
  val commitTx: Transaction
}

case class LocalCommitPublished(commitTx: Transaction, claimMainDelayedOutputTx: Option[Transaction], htlcSuccessTxs: List[Transaction], htlcTimeoutTxs: List[Transaction],
                                claimHtlcDelayedTxs: List[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = (claimMainDelayedOutputTx.toList ++ claimHtlcDelayedTxs).filterNot(delayTx => irrevocablySpentTxIds contains delayTx.txid)
}

case class RemoteCommitPublished(commitTx: Transaction, claimMainOutputTx: Option[Transaction], claimHtlcSuccessTxs: List[Transaction],
                                 claimHtlcTimeoutTxs: List[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = claimHtlcTimeoutTxs.filterNot(delayTx => irrevocablySpentTxIds contains delayTx.txid)

  lazy val paymentLeftoverRefunds: Seq[Transaction] = claimHtlcSuccessTxs ++ claimHtlcTimeoutTxs

  lazy val balanceLeftoverRefunds: Seq[Transaction] = commitTx +: claimMainOutputTx.toList
}

case class RevokedCommitPublished(commitTx: Transaction, claimMainOutputTx: Option[Transaction], mainPenaltyTx: Option[Transaction], htlcPenaltyTxs: List[Transaction],
                                  claimHtlcDelayedPenaltyTxs: List[Transaction], irrevocablySpent: Map[OutPoint, ByteVector32] = Map.empty) extends ForceCloseCommitPublished {

  lazy val delayedRefundsLeft: Seq[Transaction] = claimHtlcDelayedPenaltyTxs.filterNot(delayTx => irrevocablySpentTxIds contains delayTx.txid)

  lazy val penaltyTxs: Seq[Transaction] = claimMainOutputTx.toList ++ mainPenaltyTx.toList ++ htlcPenaltyTxs ++ claimHtlcDelayedPenaltyTxs
}

final case class DATA_WAIT_FOR_OPEN_CHANNEL(initFundee: INPUT_INIT_FUNDEE) extends ChannelData

final case class DATA_WAIT_FOR_ACCEPT_CHANNEL(initFunder: INPUT_INIT_FUNDER, lastSent: OpenChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_INTERNAL(initFunder: INPUT_INIT_FUNDER, remoteParams: RemoteParams, remoteFirstPerCommitmentPoint: PublicKey, lastSent: OpenChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_CREATED(initFundee: INPUT_INIT_FUNDEE, remoteParams: RemoteParams, lastSent: AcceptChannel) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_SIGNED(remoteInfo: RemoteNodeInfo, channelId: ByteVector32, localParams: LocalParams, remoteParams: RemoteParams, fundingTx: Transaction,
                                              fundingTxFee: Satoshi, localSpec: CommitmentSpec, localCommitTx: CommitTx, remoteCommit: RemoteCommit, channelFlags: Byte,
                                              channelVersion: ChannelVersion, lastSent: FundingCreated) extends ChannelData

final case class DATA_WAIT_FOR_FUNDING_CONFIRMED(commitments: NormalCommits, fundingTx: Option[Transaction],
                                                 waitingSince: Long, lastSent: Either[FundingCreated, FundingSigned],
                                                 deferred: Option[FundingLocked] = None) extends ChannelData with HasNormalCommitments

final case class DATA_WAIT_FOR_FUNDING_LOCKED(commitments: NormalCommits, shortChannelId: ShortChannelId, lastSent: FundingLocked) extends ChannelData with HasNormalCommitments

final case class DATA_NORMAL(commitments: NormalCommits, shortChannelId: ShortChannelId, localShutdown: Option[Shutdown] = None,
                             remoteShutdown: Option[Shutdown] = None) extends ChannelData with HasNormalCommitments

final case class DATA_NEGOTIATING(commitments: NormalCommits, localShutdown: Shutdown, remoteShutdown: Shutdown, closingTxProposed: List[List[ClosingTxProposed]],
                                  bestUnpublishedClosingTxOpt: Option[Transaction] = None) extends ChannelData with HasNormalCommitments

final case class DATA_CLOSING(commitments: NormalCommits, fundingTx: Option[Transaction], waitingSince: Long = System.currentTimeMillis, mutualCloseProposed: List[Transaction] = Nil,
                              mutualClosePublished: List[Transaction] = Nil, localCommitPublished: Option[LocalCommitPublished] = None, remoteCommitPublished: Option[RemoteCommitPublished] = None,
                              nextRemoteCommitPublished: Option[RemoteCommitPublished] = None, futureRemoteCommitPublished: Option[RemoteCommitPublished] = None,
                              revokedCommitPublished: List[RevokedCommitPublished] = Nil) extends ChannelData with HasNormalCommitments {

  lazy val balanceLeftoverRefunds: Seq[Transaction] = {
    // It's OK to use a set of all possible payment leftovers because it will be compared against an incoming tx
    mutualCloseProposed ++ mutualClosePublished ++ localCommitPublished.toList.flatMap(_.claimMainDelayedOutputTx) ++
      remoteCommitPublished.toList.flatMap(_.balanceLeftoverRefunds) ++ nextRemoteCommitPublished.toList.flatMap(_.balanceLeftoverRefunds) ++
      futureRemoteCommitPublished.toList.flatMap(_.balanceLeftoverRefunds)
  }

  lazy val paymentLeftoverRefunds: Seq[Transaction] = {
    // It's OK to use a set of all possible payment leftovers because it will be compared against an incoming tx
    localCommitPublished.toList.flatMap(_.claimHtlcDelayedTxs) ++ remoteCommitPublished.toList.flatMap(_.paymentLeftoverRefunds) ++
      nextRemoteCommitPublished.toList.flatMap(_.paymentLeftoverRefunds) ++ futureRemoteCommitPublished.toList.flatMap(_.paymentLeftoverRefunds)
  }

  lazy val forceCloseCommitPublished: Option[ForceCloseCommitPublished] = {
    // We must select a single candidate here because its delayed refunds will be displayed to user, so we can't show a total sum of all possible refunds
    val candidates = localCommitPublished ++ remoteCommitPublished ++ nextRemoteCommitPublished ++ futureRemoteCommitPublished ++ revokedCommitPublished
    candidates.find(_.isCommitConfirmed).orElse(candidates.headOption)
  }
}

final case class DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT(commitments: NormalCommits, remoteChannelReestablish: ChannelReestablish) extends ChannelData with HasNormalCommitments

object ChannelKeys {
  def fromPath(master: ExtendedPrivateKey, path: KeyPath): ChannelKeys = {
    val fundingKey = derivePrivateKey(chain = path.path :+ hardened(0L), parent = master)
    val revocationKey = derivePrivateKey(chain = path.path :+ hardened(1L), parent = master)
    val paymentKey = derivePrivateKey(chain = path.path :+ hardened(2L), parent = master)
    val delayedKey = derivePrivateKey(chain = path.path :+ hardened(3L), parent = master)
    val htlcKey = derivePrivateKey(chain = path.path :+ hardened(4L), parent = master)
    val shaBase = derivePrivateKey(chain = path.path :+ hardened(5L), parent = master)

    val shaSeed = Crypto.sha256(shaBase.privateKey.value :+ 1.toByte)
    ChannelKeys(path, shaSeed, fundingKey, revocationKey, paymentKey, delayedKey, htlcKey)
  }

  def newKeyPath(isFunder: Boolean): KeyPath = {
    def nextHop: Long = secureRandom.nextInt & 0xFFFFFFFFL
    val lastHop = if (isFunder) hardened(1) else hardened(0)
    val path = Seq(nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, lastHop)
    KeyPath(path)
  }
}

case class ChannelKeys(path: KeyPath, shaSeed: ByteVector32, fundingKey: ExtendedPrivateKey, revocationKey: ExtendedPrivateKey, paymentKey: ExtendedPrivateKey, delayedPaymentKey: ExtendedPrivateKey, htlcKey: ExtendedPrivateKey) {
  def sign(tx: TransactionWithInputInfo, key: PrivateKey, remoteSecret: PrivateKey, txOwner: TxOwner, format: CommitmentFormat): ByteVector64 = Transactions.sign(tx, Generators.revocationPrivKey(key, remoteSecret), txOwner, format)
  def sign(tx: TransactionWithInputInfo, key: PrivateKey, remotePoint: PublicKey, txOwner: TxOwner, format: CommitmentFormat): ByteVector64 = Transactions.sign(tx, Generators.derivePrivKey(key, remotePoint), txOwner, format)
  def commitmentSecret(index: Long): PrivateKey = Generators.perCommitSecret(shaSeed, index)
  def commitmentPoint(index: Long): PublicKey = Generators.perCommitPoint(shaSeed, index)
}

final case class LocalParams(keys: ChannelKeys, dustLimit: Satoshi, maxHtlcValueInFlightMsat: UInt64, channelReserve: Satoshi, htlcMinimum: MilliSatoshi,
                             toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int, isFunder: Boolean, defaultFinalScriptPubKey: ByteVector, walletStaticPaymentBasepoint: PublicKey)

final case class RemoteParams(dustLimit: Satoshi, maxHtlcValueInFlightMsat: UInt64, channelReserve: Satoshi, htlcMinimum: MilliSatoshi, toSelfDelay: CltvExpiryDelta, maxAcceptedHtlcs: Int,
                              fundingPubKey: PublicKey, revocationBasepoint: PublicKey, paymentBasepoint: PublicKey, delayedPaymentBasepoint: PublicKey, htlcBasepoint: PublicKey)

case class ChannelVersion(bits: BitVector) {
  val commitmentFormat: CommitmentFormat = DefaultCommitmentFormat
  def | (other: ChannelVersion): ChannelVersion = ChannelVersion(bits | other.bits)
}

object ChannelVersion {
  val LENGTH_BITS: Int = 4 * 8
  private val USE_PUBKEY_KEYPATH_BIT = 0
  private val USE_STATIC_REMOTEKEY_BIT = 1

  val ZEROES: ChannelVersion = ChannelVersion(bin"00000000000000000000000000000000")

  val STANDARD: ChannelVersion = ZEROES | fromBit(USE_PUBKEY_KEYPATH_BIT)

  val STATIC_REMOTEKEY: ChannelVersion = STANDARD | fromBit(USE_STATIC_REMOTEKEY_BIT)

  def fromBit(bit: Int): ChannelVersion = ChannelVersion(BitVector.low(LENGTH_BITS).set(bit).reverse)
}

object HostedChannelVersion {
  val RESIZABLE: ChannelVersion = ChannelVersion.STANDARD | ChannelVersion.fromBit(1)
}
