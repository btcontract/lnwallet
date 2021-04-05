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

package fr.acinq.eclair.wire

import scodec.codecs._
import fr.acinq.eclair.channel._
import fr.acinq.eclair.transactions._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.transactions.Transactions._
import fr.acinq.eclair.wire.LightningMessageCodecs._
import fr.acinq.eclair.crypto.ShaChain

import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPrivateKey, KeyPath}
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Transaction, TxOut}
import immortan.{HostedCommits, RemoteNodeInfo}
import scodec.{Attempt, Codec}

/**
 * Created by PM on 02/06/2017.
 */
object ChannelCodecs {

  /**
   * All LN protocol message must be stored as length-delimited, because they may have arbitrary trailing data
   */
  def lengthDelimited[T](codec: Codec[T]): Codec[T] = variableSizeBytesLong(varintoverflow, codec)

  val keyPathCodec: Codec[KeyPath] = ("path" | listOfN(uint16, uint32)).xmap[KeyPath](l => new KeyPath(l), keyPath => keyPath.path.toList).as[KeyPath]

  val extendedPrivateKeyCodec: Codec[ExtendedPrivateKey] = {
    ("secretkeybytes" | bytes32) ::
      ("chaincode" | bytes32) ::
      ("depth" | uint16) ::
      ("path" | keyPathCodec) ::
      ("parent" | int64)
  }.as[ExtendedPrivateKey]

  val channelVersionCodec: Codec[ChannelVersion] = bits(ChannelVersion.LENGTH_BITS).as[ChannelVersion]

  val outPointCodec: Codec[OutPoint] = lengthDelimited(bytes.xmap(d => OutPoint.read(d.toArray), d => OutPoint.write(d)))

  val txOutCodec: Codec[TxOut] = lengthDelimited(bytes.xmap(d => TxOut.read(d.toArray), d => TxOut.write(d)))

  val txCodec: Codec[Transaction] = lengthDelimited(bytes.xmap(d => Transaction.read(d.toArray), d => Transaction.write(d)))

  /**
   * byte-aligned boolean codec
   */
  val bool8: Codec[Boolean] = bool(8)

  val htlcCodec: Codec[DirectedHtlc] = discriminated[DirectedHtlc].by(bool8)
    .typecase(true, lengthDelimited(updateAddHtlcCodec).as[IncomingHtlc])
    .typecase(false, lengthDelimited(updateAddHtlcCodec).as[OutgoingHtlc])

  val commitmentSpecCodec: Codec[CommitmentSpec] = {
    ("feeratePerKw" | feeratePerKw) ::
      ("toLocal" | millisatoshi) ::
      ("toRemote" | millisatoshi) ::
      ("htlcs" | setCodec(htlcCodec))
  }.as[CommitmentSpec]

  val inputInfoCodec: Codec[InputInfo] = (
    ("outPoint" | outPointCodec) ::
      ("txOut" | txOutCodec) ::
      ("redeemScript" | lengthDelimited(bytes))).as[InputInfo]

  val txWithInputInfoCodec: Codec[TransactionWithInputInfo] = discriminated[TransactionWithInputInfo].by(uint16)
    .typecase(0x01, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[CommitTx])
    .typecase(0x02, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec) :: ("paymentHash" | bytes32)).as[HtlcSuccessTx])
    .typecase(0x03, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[HtlcTimeoutTx])
    .typecase(0x04, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimHtlcSuccessTx])
    .typecase(0x05, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimHtlcTimeoutTx])
    .typecase(0x06, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimP2WPKHOutputTx])
    .typecase(0x07, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClaimLocalDelayedOutputTx])
    .typecase(0x08, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[MainPenaltyTx])
    .typecase(0x09, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[HtlcPenaltyTx])
    .typecase(0x10, (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[ClosingTx])

  val htlcTxAndSigsCodec: Codec[HtlcTxAndSigs] = (
    ("txinfo" | txWithInputInfoCodec) ::
      ("localSig" | lengthDelimited(bytes64)) :: // we store as variable length for historical purposes (we used to store as DER encoded)
      ("remoteSig" | lengthDelimited(bytes64))).as[HtlcTxAndSigs]

  val publishableTxsCodec: Codec[PublishableTxs] = (
    ("commitTx" | (("inputInfo" | inputInfoCodec) :: ("tx" | txCodec)).as[CommitTx]) ::
      ("htlcTxsAndSigs" | listOfN(uint16, htlcTxAndSigsCodec))).as[PublishableTxs]

  val localCommitCodec: Codec[LocalCommit] = (
    ("index" | uint64overflow) ::
      ("spec" | commitmentSpecCodec) ::
      ("publishableTxs" | publishableTxsCodec)).as[LocalCommit]

  val remoteCommitCodec: Codec[RemoteCommit] = (
    ("index" | uint64overflow) ::
      ("spec" | commitmentSpecCodec) ::
      ("txid" | bytes32) ::
      ("remotePerCommitmentPoint" | publicKey)).as[RemoteCommit]

  val updateMessageCodec: Codec[UpdateMessage] = lengthDelimited(lightningMessageCodec.narrow[UpdateMessage](Attempt successful _.asInstanceOf[UpdateMessage], identity))

  val localChangesCodec: Codec[LocalChanges] = (
    ("proposed" | listOfN(uint16, updateMessageCodec)) ::
      ("signed" | listOfN(uint16, updateMessageCodec)) ::
      ("acked" | listOfN(uint16, updateMessageCodec))).as[LocalChanges]

  val remoteChangesCodec: Codec[RemoteChanges] = (
    ("proposed" | listOfN(uint16, updateMessageCodec)) ::
      ("acked" | listOfN(uint16, updateMessageCodec)) ::
      ("signed" | listOfN(uint16, updateMessageCodec))).as[RemoteChanges]

  val waitingForRevocationCodec: Codec[WaitingForRevocation] = (
    ("nextRemoteCommit" | remoteCommitCodec) ::
      ("sent" | lengthDelimited(commitSigCodec)) ::
      ("sentAfterLocalCommitIndex" | uint64overflow) ::
      ("reSignAsap" | bool8)).as[WaitingForRevocation]

  val spentMapCodec: Codec[Map[OutPoint, ByteVector32]] = mapCodec(outPointCodec, bytes32)

  def localParamsCodec(channelVersion: ChannelVersion): Codec[LocalParams] = {
    ("channelPath" | keyPathCodec) ::
      ("dustLimit" | satoshi) ::
      ("maxHtlcValueInFlightMsat" | uint64) ::
      ("channelReserve" | satoshi) ::
      ("htlcMinimum" | millisatoshi) ::
      ("toSelfDelay" | cltvExpiryDelta) ::
      ("maxAcceptedHtlcs" | uint16) ::
      ("isFunder" | bool8) ::
      ("defaultFinalScriptPubKey" | lengthDelimited(bytes)) ::
      ("walletStaticPaymentBasepoint" | optional(provide(channelVersion.paysDirectlyToWallet), publicKey))
  }.as[LocalParams]

  val remoteParamsCodec: Codec[RemoteParams] = {
    ("dustLimit" | satoshi) ::
      ("maxHtlcValueInFlightMsat" | uint64) ::
      ("channelReserve" | satoshi) ::
      ("htlcMinimum" | millisatoshi) ::
      ("toSelfDelay" | cltvExpiryDelta) ::
      ("maxAcceptedHtlcs" | uint16) ::
      ("fundingPubKey" | publicKey) ::
      ("revocationBasepoint" | publicKey) ::
      ("paymentBasepoint" | publicKey) ::
      ("delayedPaymentBasepoint" | publicKey) ::
      ("htlcBasepoint" | publicKey)
  }.as[RemoteParams]

  val remoteNodeInfoCodec = {
    (publicKey withContext "nodeId") ::
      (nodeaddress withContext "address") ::
      (zeropaddedstring(32) withContext "alias")
  }.as[RemoteNodeInfo]

  val commitmentsCodec: Codec[NormalCommits] = (
    ("channelVersion" | channelVersionCodec) >>:~ { channelVersion =>
      ("remoteInfo" | remoteNodeInfoCodec) ::
      ("localParams" | localParamsCodec(channelVersion)) ::
      ("remoteParams" | remoteParamsCodec) ::
      ("channelFlags" | byte) ::
      ("localCommit" | localCommitCodec) ::
      ("remoteCommit" | remoteCommitCodec) ::
      ("localChanges" | localChangesCodec) ::
      ("remoteChanges" | remoteChangesCodec) ::
      ("localNextHtlcId" | uint64overflow) ::
      ("remoteNextHtlcId" | uint64overflow) ::
      ("remoteNextCommitInfo" | either(bool8, waitingForRevocationCodec, publicKey)) ::
      ("commitInput" | inputInfoCodec) ::
      ("remotePerCommitmentSecrets" | byteAligned(ShaChain.shaChainCodec)) ::
      ("channelId" | bytes32) ::
      ("updateOpt" | optional(bool8, lengthDelimited(channelUpdateCodec))) ::
      ("startedAt" | int64)
  }).as[NormalCommits]

  val closingTxProposedCodec: Codec[ClosingTxProposed] = (
    ("unsignedTx" | txCodec) ::
      ("localClosingSigned" | lengthDelimited(closingSignedCodec))).as[ClosingTxProposed]

  val localCommitPublishedCodec: Codec[LocalCommitPublished] = (
    ("commitTx" | txCodec) ::
      ("claimMainDelayedOutputTx" | optional(bool8, txCodec)) ::
      ("htlcSuccessTxs" | listOfN(uint16, txCodec)) ::
      ("htlcTimeoutTxs" | listOfN(uint16, txCodec)) ::
      ("claimHtlcDelayedTx" | listOfN(uint16, txCodec)) ::
      ("spent" | spentMapCodec)).as[LocalCommitPublished]

  val remoteCommitPublishedCodec: Codec[RemoteCommitPublished] = (
    ("commitTx" | txCodec) ::
      ("claimMainOutputTx" | optional(bool8, txCodec)) ::
      ("claimHtlcSuccessTxs" | listOfN(uint16, txCodec)) ::
      ("claimHtlcTimeoutTxs" | listOfN(uint16, txCodec)) ::
      ("spent" | spentMapCodec)).as[RemoteCommitPublished]

  val revokedCommitPublishedCodec: Codec[RevokedCommitPublished] = (
    ("commitTx" | txCodec) ::
      ("claimMainOutputTx" | optional(bool8, txCodec)) ::
      ("mainPenaltyTx" | optional(bool8, txCodec)) ::
      ("htlcPenaltyTxs" | listOfN(uint16, txCodec)) ::
      ("claimHtlcDelayedPenaltyTxs" | listOfN(uint16, txCodec)) ::
      ("spent" | spentMapCodec)).as[RevokedCommitPublished]

  val DATA_WAIT_FOR_FUNDING_CONFIRMED_Codec: Codec[DATA_WAIT_FOR_FUNDING_CONFIRMED] = {
    ("commitments" | commitmentsCodec) ::
      ("fundingTx" | optional(bool8, txCodec)) ::
      ("waitingSince" | int64) ::
      ("lastSent" | either(bool8, lengthDelimited(fundingCreatedCodec), lengthDelimited(fundingSignedCodec))) ::
      ("deferred" | optional(bool8, lengthDelimited(fundingLockedCodec)))
  }.as[DATA_WAIT_FOR_FUNDING_CONFIRMED]

  val DATA_WAIT_FOR_FUNDING_LOCKED_Codec: Codec[DATA_WAIT_FOR_FUNDING_LOCKED] = {
    ("commitments" | commitmentsCodec) ::
      ("shortChannelId" | shortchannelid) ::
      ("lastSent" | lengthDelimited(fundingLockedCodec))
  }.as[DATA_WAIT_FOR_FUNDING_LOCKED]

  val DATA_NORMAL_Codec: Codec[DATA_NORMAL] = (
    ("commitments" | commitmentsCodec) ::
      ("shortChannelId" | shortchannelid) ::
      ("localShutdown" | optional(bool8, lengthDelimited(shutdownCodec))) ::
      ("remoteShutdown" | optional(bool8, lengthDelimited(shutdownCodec)))).as[DATA_NORMAL]

  val DATA_NEGOTIATING_Codec: Codec[DATA_NEGOTIATING] = (
    ("commitments" | commitmentsCodec) ::
      ("localShutdown" | lengthDelimited(shutdownCodec)) ::
      ("remoteShutdown" | lengthDelimited(shutdownCodec)) ::
      ("closingTxProposed" | listOfN(uint16, listOfN(uint16, lengthDelimited(closingTxProposedCodec)))) ::
      ("bestUnpublishedClosingTxOpt" | optional(bool8, txCodec))).as[DATA_NEGOTIATING]

  val DATA_CLOSING_Codec: Codec[DATA_CLOSING] = (
    ("commitments" | commitmentsCodec) ::
      ("fundingTx" | optional(bool8, txCodec)) ::
      ("waitingSince" | int64) ::
      ("mutualCloseProposed" | listOfN(uint16, txCodec)) ::
      ("mutualClosePublished" | listOfN(uint16, txCodec)) ::
      ("localCommitPublished" | optional(bool8, localCommitPublishedCodec)) ::
      ("remoteCommitPublished" | optional(bool8, remoteCommitPublishedCodec)) ::
      ("nextRemoteCommitPublished" | optional(bool8, remoteCommitPublishedCodec)) ::
      ("futureRemoteCommitPublished" | optional(bool8, remoteCommitPublishedCodec)) ::
      ("revokedCommitPublished" | listOfN(uint16, revokedCommitPublishedCodec))).as[DATA_CLOSING]

  val DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT_Codec: Codec[DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT] = {
    ("commitments" | commitmentsCodec) ::
      ("remoteChannelReestablish" | channelReestablishCodec)
  }.as[DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT]

  val hostedCommitsCodec: Codec[HostedCommits] = {
    (remoteNodeInfoCodec withContext "remoteInfo") ::
      (lengthDelimited(lastCrossSignedStateCodec) withContext "lastCrossSignedState") ::
      (listOfN(uint16, updateMessageCodec) withContext "nextLocalUpdates") ::
      (listOfN(uint16, updateMessageCodec) withContext "nextRemoteUpdates") ::
      (commitmentSpecCodec withContext "localSpec") ::
      (optional(bool8, lengthDelimited(channelUpdateCodec)) withContext "updateOpt") ::
      (optional(bool8, lengthDelimited(errorCodec)) withContext "localError") ::
      (optional(bool8, lengthDelimited(errorCodec)) withContext "remoteError") ::
      (optional(bool8, lengthDelimited(resizeChannelCodec)) withContext "resizeProposal") ::
      (int64 withContext "startedAt")
  }.as[HostedCommits]

  val persistentDataCodec: Codec[PersistentChannelData] =
    discriminated[PersistentChannelData].by(uint16)
      .typecase(1, DATA_WAIT_FOR_FUNDING_CONFIRMED_Codec)
      .typecase(2, DATA_WAIT_FOR_FUNDING_LOCKED_Codec)
      .typecase(3, DATA_NORMAL_Codec)
      .typecase(4, DATA_NEGOTIATING_Codec)
      .typecase(5, DATA_CLOSING_Codec)
      .typecase(6, DATA_WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT_Codec)
      .typecase(7, hostedCommitsCodec)
}
