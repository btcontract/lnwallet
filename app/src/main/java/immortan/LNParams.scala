package immortan

import fr.acinq.eclair._
import fr.acinq.bitcoin._
import fr.acinq.eclair.wire._
import immortan.crypto.Tools._
import fr.acinq.eclair.Features._
import scala.concurrent.duration._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.bitcoin.DeterministicWallet._
import scodec.bits.{ByteVector, HexStringSyntax}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import immortan.sqlite.{DBInterface, PreparedQuery, RichCursor}
import fr.acinq.eclair.router.Router.{PublicChannel, RouterConf}
import fr.acinq.eclair.channel.{LocalParams, PersistentChannelData}
import akka.actor.{ActorRef, ActorSystem, Props, SupervisorStrategy}
import fr.acinq.eclair.transactions.{DirectedHtlc, RemoteFulfill, Transactions}
import immortan.utils.{Denomination, FeeRatesInfo, FiatRatesInfo, PaymentRequestExt, WalletEventsCatcher}
import fr.acinq.eclair.blockchain.electrum.ElectrumClientPool.ElectrumServerAddress
import fr.acinq.eclair.blockchain.electrum.db.WalletDb
import scala.concurrent.ExecutionContextExecutor
import fr.acinq.eclair.router.ChannelUpdateExt
import java.util.concurrent.atomic.AtomicLong
import immortan.SyncMaster.ShortChanIdSet
import fr.acinq.eclair.crypto.Generators
import immortan.crypto.Noise.KeyPair
import java.io.ByteArrayInputStream
import java.nio.ByteOrder
import akka.util.Timeout
import scala.util.Try


object LNParams {
  val blocksPerDay: Int = 144 // On average we can expect this many blocks per day
  val cltvRejectThreshold: Int = 144 // Reject incoming payment if CLTV expiry is closer than this to current chain tip when HTLC arrives
  val incomingPaymentCltvExpiry: Int = 144 + 72 // Ask payer to set final CLTV expiry to payer's current chain tip + this many blocks

  val maxCltvExpiryDelta: CltvExpiryDelta = CltvExpiryDelta(1008)
  val maxToLocalDelay: CltvExpiryDelta = CltvExpiryDelta(2016)
  val maxFundingSatoshis: Satoshi = Satoshi(10000000000L)
  val maxReserveToFundingRatio: Double = 0.05
  val maxNegotiationIterations: Int = 20
  val maxChainConnectionsCount: Int = 5
  val maxAcceptedHtlcs: Int = 483

  val minHostedOnChainRefund: Satoshi = Satoshi(1000000L)
  val minPayment: MilliSatoshi = MilliSatoshi(5000L)
  val minFundingSatoshis: Satoshi = Satoshi(100000L)
  val minHostedLiabilityBlockdays: Int = 365
  val minDustLimit: Satoshi = Satoshi(546L)
  val minDepthBlocks: Int = 3

  val chainHash: ByteVector32 = Block.LivenetGenesisBlock.hash
  val reserveToFundingRatio: Double = 0.0025 // %
  val offChainFeeRatio: Double = 0.01 // %

  // Init messages + features

  private[this] val networks: InitTlv = InitTlv.Networks(chainHash :: Nil)
  private[this] val tlvStream: TlvStream[InitTlv] = TlvStream(networks)

  val ourInit: Init = Init(Features(
    (ChannelRangeQueries, FeatureSupport.Optional),
    (ChannelRangeQueriesExtended, FeatureSupport.Optional),
    (OptionDataLossProtect, FeatureSupport.Optional),
    (BasicMultiPartPayment, FeatureSupport.Optional),
    (VariableLengthOnion, FeatureSupport.Optional),
    (TrampolineRouting, FeatureSupport.Optional),
    (StaticRemoteKey, FeatureSupport.Optional),
    (HostedChannels, FeatureSupport.Optional),
    (AnchorOutputs, FeatureSupport.Optional),
    (PaymentSecret, FeatureSupport.Optional),
    (ChainSwap, FeatureSupport.Optional),
    (Wumbo, FeatureSupport.Optional)
  ), tlvStream)

  // Variables to be assigned at runtime

  var cm: ChannelMaster = _
  var format: StorageFormat = _
  var chainWallet: WalletExt = _
  var syncParams: SyncParams = _
  var fiatRatesInfo: FiatRatesInfo = _
  var feeRatesInfo: FeeRatesInfo = _
  var denomination: Denomination = _
  var trampoline: TrampolineOn = _

  var routerConf: RouterConf =
    RouterConf(maxCltvDelta = CltvExpiryDelta(2016), routeHopDistance = 6,
      mppMinPartAmount = MilliSatoshi(10000000L), maxRemoteAttempts = 12,
      maxChannelFailures = 6, maxStrangeNodeFailures = 12)

  // Last known chain tip
  val blockCount: AtomicLong = new AtomicLong(0L)
  // Chain wallet has lost connection this long time ago
  // this can only happen after wallet has initally connected
  var lastDisconnect: Option[Long] = None

  implicit val timeout: Timeout = Timeout(30.seconds)
  implicit val system: ActorSystem = ActorSystem("immortan-actor-system")
  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global

  def createWallet(addresses: Set[ElectrumServerAddress], walletDb: WalletDb, seed: ByteVector): WalletExt = {
    val clientPool = system.actorOf(SimpleSupervisor.props(Props(new ElectrumClientPool(blockCount, addresses)), "pool", SupervisorStrategy.Resume))
    val watcher = system.actorOf(SimpleSupervisor.props(Props(new ElectrumWatcher(blockCount, clientPool)), "watcher", SupervisorStrategy.Resume))
    val wallet = system.actorOf(ElectrumWallet.props(seed, clientPool, ElectrumWallet.WalletParameters(chainHash, walletDb)), "wallet")
    val catcher = system.actorOf(Props(new WalletEventsCatcher), "catcher")
    val eclairWallet = new ElectrumEclairWallet(wallet, chainHash)
    WalletExt(eclairWallet, catcher, clientPool, watcher)
  }

  def currentBlockDay: Long = blockCount.get / blocksPerDay

  def chainDisconnectedForTooLong: Boolean = lastDisconnect.exists(_ < System.currentTimeMillis - 60 * 60 * 1000L * 2)

  def incorrectDetails(amount: MilliSatoshi): FailureMessage = IncorrectOrUnknownPaymentDetails(amount, blockCount.get)
}

class SyncParams {
  val blw: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03144fcc73cea41a002b2865f98190ab90e4ff58a2ce24d3870f5079081e42922d"), NodeAddress.unresolved(9735, host = 5, 9, 83, 143), "BLW Den")
  val lightning: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03baa70886d9200af0ffbd3f9e18d96008331c858456b16e3a9b41e735c6208fef"), NodeAddress.unresolved(9735, host = 45, 20, 67, 1), "LIGHTNING")
  val conductor: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03c436af41160a355fc1ed230a64f6a64bcbd2ae50f12171d1318f9782602be601"), NodeAddress.unresolved(9735, host = 18, 191, 89, 219), "Conductor")
  val cheese: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"0276e09a267592e7451a939c932cf685f0754de382a3ca85d2fb3a864d4c365ad5"), NodeAddress.unresolved(9735, host = 94, 177, 171, 73), "Cheese")
  val acinq: RemoteNodeInfo = RemoteNodeInfo(PublicKey(hex"03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f"), NodeAddress.unresolved(9735, host = 34, 239, 230, 56), "ACINQ")

  val hostedChanNodes: Set[RemoteNodeInfo] = Set(blw, lightning, acinq) // Trusted nodes which are shown as default ones when user chooses providers
  val hostedSyncNodes: Set[RemoteNodeInfo] = Set(blw, lightning, acinq) // Semi-trusted PHC-enabled nodes which can be used as seeds for PHC sync
  val syncNodes: Set[RemoteNodeInfo] = Set(lightning, acinq, conductor) // Nodes with extended queries support used as seeds for normal sync

  val maxPHCCapacity: MilliSatoshi = MilliSatoshi(1000000000000000L) // PHC can not be larger than 10 000 BTC
  val minPHCCapacity: MilliSatoshi = MilliSatoshi(50000000000L) // PHC can not be smaller than 0.5 BTC
  val minNormalChansForPHC = 5 // How many normal chans a node must have to be eligible for PHCs
  val maxPHCPerNode = 2 // How many PHCs a node can have in total

  val minCapacity: MilliSatoshi = MilliSatoshi(1000000000L) // 1M sat
  val maxNodesToSyncFrom = 3 // How many disjoint peers to use for majority sync
  val acceptThreshold = 1 // ShortIds and updates are accepted if confirmed by more than this peers
  val messagesToAsk = 1000 // Ask for this many messages from peer before they say this chunk is done
  val chunksToWait = 4 // Wait for at least this much chunk iterations from any peer before recording results
}

// Important: LNParams.format must be defined
case class RemoteNodeInfo(nodeId: PublicKey, address: NodeAddress, alias: String) {
  lazy val nodeSpecificExtendedKey: DeterministicWallet.ExtendedPrivateKey = LNParams.format.keys.ourFakeNodeIdKey(nodeId)

  lazy val nodeSpecificPrivKey: PrivateKey = nodeSpecificExtendedKey.privateKey

  lazy val nodeSpecificPubKey: PublicKey = nodeSpecificPrivKey.publicKey

  lazy val nodeSpecificPair: KeyPairAndPubKey = KeyPairAndPubKey(KeyPair(nodeSpecificPubKey.value, nodeSpecificPrivKey.value), nodeId)

  private def derivePrivKey(path: KeyPath) = derivePrivateKey(nodeSpecificExtendedKey, path)

  private val channelPrivateKeysMemo = memoize(derivePrivKey)

  private val channelPublicKeysMemo = memoize(channelPrivateKeysMemo.get _ andThen publicKey)

  private def internalKeyPath(channelKeyPath: DeterministicWallet.KeyPath, index: Long): Seq[Long] = channelKeyPath.path :+ hardened(index)

  private def shaSeed(channelKeyPath: DeterministicWallet.KeyPath): ByteVector32 = {
    val extendedKey = channelPrivateKeysMemo getUnchecked internalKeyPath(channelKeyPath, 5L)
    Crypto.sha256(extendedKey.privateKey.value :+ 1.toByte)
  }

  def keyPath(localParams: LocalParams): DeterministicWallet.KeyPath = {
    val fundPubKey = fundingPublicKey(localParams.fundingKeyPath).publicKey
    val bis = new ByteArrayInputStream(Crypto.sha256(fundPubKey.value).toArray)
    def nextHop: Long = Protocol.uint32(input = bis, order = ByteOrder.BIG_ENDIAN)
    val path = Seq(nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop)
    DeterministicWallet.KeyPath(path)
  }

  def newFundingKeyPath(isFunder: Boolean): KeyPath = {
    def nextHop: Long = secureRandom.nextInt & 0xFFFFFFFFL
    val last = if (isFunder) DeterministicWallet.hardened(1) else DeterministicWallet.hardened(0)
    val path = Seq(nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, nextHop, last)
    DeterministicWallet.KeyPath(path)
  }

  def fundingPublicKey(channelKeyPath: DeterministicWallet.KeyPath): ExtendedPublicKey = channelPublicKeysMemo getUnchecked internalKeyPath(channelKeyPath, 0L)

  def revocationPoint(channelKeyPath: DeterministicWallet.KeyPath): ExtendedPublicKey = channelPublicKeysMemo getUnchecked internalKeyPath(channelKeyPath, 1L)

  def paymentPoint(channelKeyPath: DeterministicWallet.KeyPath): ExtendedPublicKey = channelPublicKeysMemo getUnchecked internalKeyPath(channelKeyPath, 2L)

  def delayedPaymentPoint(channelKeyPath: DeterministicWallet.KeyPath): ExtendedPublicKey = channelPublicKeysMemo getUnchecked internalKeyPath(channelKeyPath, 3L)

  def htlcPoint(channelKeyPath: DeterministicWallet.KeyPath): ExtendedPublicKey = channelPublicKeysMemo getUnchecked internalKeyPath(channelKeyPath, 4L)

  def commitmentSecret(channelKeyPath: DeterministicWallet.KeyPath, index: Long): PrivateKey = Generators.perCommitSecret(shaSeed(channelKeyPath), index)

  def commitmentPoint(channelKeyPath: DeterministicWallet.KeyPath, index: Long): PublicKey = Generators.perCommitPoint(shaSeed(channelKeyPath), index)

  def sign(tx: Transactions.TransactionWithInputInfo, publicKey: ExtendedPublicKey, txOwner: Transactions.TxOwner, commitmentFormat: Transactions.CommitmentFormat): ByteVector64 =
    Transactions.sign(tx, channelPrivateKeysMemo.get(publicKey.path).privateKey, txOwner, commitmentFormat)

  def sign(tx: Transactions.TransactionWithInputInfo, publicKey: ExtendedPublicKey, remotePoint: PublicKey, txOwner: Transactions.TxOwner, commitmentFormat: Transactions.CommitmentFormat): ByteVector64 =
    Transactions.sign(tx, Generators.derivePrivKey(channelPrivateKeysMemo.get(publicKey.path).privateKey, remotePoint), txOwner, commitmentFormat)

  def sign(tx: Transactions.TransactionWithInputInfo, publicKey: ExtendedPublicKey, remoteSecret: PrivateKey, txOwner: Transactions.TxOwner, commitmentFormat: Transactions.CommitmentFormat): ByteVector64 =
    Transactions.sign(tx, Generators.revocationPrivKey(channelPrivateKeysMemo.get(publicKey.path).privateKey, remoteSecret), txOwner, commitmentFormat)
}

case class WalletExt(wallet: ElectrumEclairWallet, eventsCatcher: ActorRef, clientPool: ActorRef, watcher: ActorRef)

case class UpdateAddHtlcExt(theirAdd: UpdateAddHtlc, remoteInfo: RemoteNodeInfo)

case class SwapInStateExt(state: SwapInState, nodeId: PublicKey)

// Interfaces

trait NetworkBag {
  def addChannelAnnouncement(ca: ChannelAnnouncement, newSqlPQ: PreparedQuery): Unit
  def addChannelUpdateByPosition(cu: ChannelUpdate, newSqlPQ: PreparedQuery, updSqlPQ: PreparedQuery): Unit
  def addExcludedChannel(shortId: ShortChannelId, untilStamp: Long, newSqlPQ: PreparedQuery): Unit // Disregard position
  def removeChannelUpdate(shortId: ShortChannelId, killSqlPQ: PreparedQuery): Unit

  def addChannelUpdateByPosition(cu: ChannelUpdate): Unit
  def removeChannelUpdate(shortId: ShortChannelId): Unit

  def listChannelAnnouncements: Iterable[ChannelAnnouncement]
  def listChannelUpdates: Iterable[ChannelUpdateExt]
  def listChannelsWithOneUpdate: ShortChanIdSet
  def listExcludedChannels: Set[Long]

  def incrementChannelScore(cu: ChannelUpdate): Unit
  def getRoutingData: Map[ShortChannelId, PublicChannel]
  def removeGhostChannels(ghostIds: ShortChanIdSet, oneSideIds: ShortChanIdSet): Unit
  def processCompleteHostedData(pure: CompleteHostedRoutingData): Unit
  def processPureData(data: PureRoutingData): Unit
}

// Bag of stored payments and successful relays

trait PaymentBag {
  def addPreimage(paymentHash: ByteVector32, preimage: ByteVector32)
  def addSearchablePayment(search: String, paymentHash: ByteVector32): Unit
  def addRelayedPreimageInfo(fullTag: FullPaymentTag, preimage: ByteVector32, relayed: MilliSatoshi, earned: MilliSatoshi)

  def replaceOutgoingPayment(prex: PaymentRequestExt, desc: PaymentDescription, action: Option[PaymentAction],
                             finalAmount: MilliSatoshi, balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc,
                             chainFee: MilliSatoshi): Unit

  def replaceIncomingPayment(prex: PaymentRequestExt, preimage: ByteVector32, description: PaymentDescription,
                             balanceSnap: MilliSatoshi, fiatRateSnap: Fiat2Btc, chainFee: MilliSatoshi): Unit

  def getPaymentInfo(paymentHash: ByteVector32): Try[PaymentInfo]
  def getPreimage(hash: ByteVector32): Try[ByteVector32]

  // These MUST be the only two methods capable of updating payment state to SUCCEEDED
  def updOkIncoming(receivedAmount: MilliSatoshi, paymentHash: ByteVector32): Unit
  def updOkOutgoing(fulfill: RemoteFulfill, fee: MilliSatoshi): Unit
  def updAbortedOutgoing(paymentHash: ByteVector32): Unit

  def listRecentRelays(limit: Int): RichCursor
  def listRecentPayments(limit: Int): RichCursor
  def toRelayedPreimageInfo(rc: RichCursor): RelayedPreimageInfo
  def toPaymentInfo(rc: RichCursor): PaymentInfo
}

trait DataBag {
  def putFormat(format: StorageFormat): Unit
  def tryGetFormat: Try[StorageFormat]

  def putReport(paymentHash: ByteVector32, report: String): Unit
  def tryGetReport(paymentHash: ByteVector32): Try[String]

  def putBranding(nodeId: PublicKey, branding: HostedChannelBranding): Unit
  def tryGetBranding(nodeId: PublicKey): Try[HostedChannelBranding]

  def putSwapInState(nodeId: PublicKey, state: SwapInState): Unit
  def tryGetSwapInState(nodeId: PublicKey): Try[SwapInStateExt]
}

object ChannelBag {
  case class Hash160AndCltv(hash160: ByteVector, cltvExpiry: CltvExpiry)
}

trait ChannelBag {
  val db: DBInterface
  def all: Iterable[PersistentChannelData]
  def delete(channelId: ByteVector32): Unit
  def put(data: PersistentChannelData): PersistentChannelData

  def htlcInfos(commitNumer: Long): Iterable[ChannelBag.Hash160AndCltv]
  def putHtlcInfo(sid: ShortChannelId, commitNumber: Long, paymentHash: ByteVector32, cltvExpiry: CltvExpiry): Unit
  def putHtlcInfos(htlcs: Seq[DirectedHtlc], sid: ShortChannelId, commitNumber: Long): Unit
  def rmHtlcInfos(sid: ShortChannelId): Unit
}