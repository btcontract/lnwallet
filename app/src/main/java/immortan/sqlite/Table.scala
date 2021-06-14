package immortan.sqlite

import immortan.PaymentStatus.{SUCCEEDED, PENDING, ABORTED}
import java.util.concurrent.atomic.AtomicInteger


trait Table {
  def createStatements: Seq[String]

  val Tuple2(id, fts) = Tuple2("_id", "fts4")

  val IDAUTOINC = s"$id INTEGER PRIMARY KEY AUTOINCREMENT"

  val UNIQUE = "UNIQUE"
}

object Table {
  val DEFAULT_LIMIT = new AtomicInteger(10)
}

// Database #1, essential data, exportable to backup

object ChannelTable extends Table {
  val (table, channelId, data) = ("channel", "chanid", "data")

  val newSql = s"INSERT OR IGNORE INTO $table ($channelId, $data) VALUES (?, ?)"

  val updSql = s"UPDATE $table SET $data = ? WHERE $channelId = ?"

  val killSql = s"DELETE FROM $table WHERE $channelId = ?"

  val selectAllSql = s"SELECT * FROM $table ORDER BY $id ASC"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $channelId TEXT NOT NULL $UNIQUE, $data BLOB NOT NULL)" :: Nil
}

object HtlcInfoTable extends Table {
  val (table, sid, commitNumber, paymentHash160, cltvExpiry) = ("htlcinfo", "sid", "cnumber", "hash160", "cltv")

  val newSql = s"INSERT INTO $table ($sid, $commitNumber, $paymentHash160, $cltvExpiry) VALUES (?, ?, ?, ?)"

  val selectAllSql = s"SELECT * FROM $table WHERE $commitNumber = ?"

  val killSql = s"DELETE FROM $table WHERE $sid = ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $sid INTEGER NOT NULL, $commitNumber INTEGER NOT NULL,
      $paymentHash160 BLOB NOT NULL, $cltvExpiry INTEGER NOT NULL
    )"""

    // Index can not be unique because for each commit we may have same local or remote number
    val addIndex1 = s"CREATE INDEX IF NOT EXISTS idx1$table ON $table ($commitNumber)"
    val addIndex2 = s"CREATE INDEX IF NOT EXISTS idx2$table ON $table ($sid)"
    createTable :: addIndex1 :: addIndex2 :: Nil
  }
}

object PreimageTable extends Table {
  val (table, hash, preimage) = ("preimages", "hash", "preimage")

  val newSql = s"INSERT OR IGNORE INTO $table ($hash, $preimage) VALUES (?, ?)"

  val selectByHashSql = s"SELECT * FROM $table WHERE $hash = ?"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $hash TEXT NOT NULL $UNIQUE, $preimage TEXT NOT NULL)" :: Nil
}

// Database #2, graph data, disposable since can be re-synchronized

abstract class ChannelAnnouncementTable(val table: String) extends Table {
  val (features, shortChannelId, nodeId1, nodeId2) = ("features", "shortchannelid", "nodeid1", "nodeid2")

  val newSql = s"INSERT OR IGNORE INTO $table ($features, $shortChannelId, $nodeId1, $nodeId2) VALUES (?, ?, ?, ?)"

  val selectAllSql = s"SELECT * FROM $table"

  val killAllSql = s"DELETE FROM $table"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $features BLOB NOT NULL, $shortChannelId INTEGER NOT NULL $UNIQUE, $nodeId1 BLOB NOT NULL, $nodeId2 BLOB NOT NULL)" :: Nil

  // This one must be a method, not a value because of late binding of abstract val
  def killNotPresentInChans = s"DELETE FROM $table WHERE $shortChannelId NOT IN ($selectFromRelatedUpdateTable)"

  val selectFromRelatedUpdateTable: String
}

object NormalChannelAnnouncementTable extends ChannelAnnouncementTable("normal_announcements") {
  val selectFromRelatedUpdateTable = s"SELECT ${NormalChannelUpdateTable.sid} FROM ${NormalChannelUpdateTable.table}"
}

object HostedChannelAnnouncementTable extends ChannelAnnouncementTable("hosted_announcements") {
  val selectFromRelatedUpdateTable = s"SELECT ${HostedChannelUpdateTable.sid} FROM ${HostedChannelUpdateTable.table}"
}

abstract class ChannelUpdateTable(val table: String, val useHeuristics: Boolean) extends Table {
  val (sid, timestamp, msgFlags, chanFlags, cltvExpiryDelta, minMsat, base, proportional, maxMsat, position, score, crc32) =
    ("shortchannelid", "timestamp", "messageflags", "channelflags", "cltvdelta", "htlcminimum", "feebase", "feeproportional", "htlcmaximum", "position", "score", "crc32")

  val updScoreSql = s"UPDATE $table SET $score = $score + 1 WHERE $sid = ?"

  val updSQL = s"UPDATE $table SET $timestamp = ?, $msgFlags = ?, $chanFlags = ?, $cltvExpiryDelta = ?, $minMsat = ?, $base = ?, $proportional = ?, $maxMsat = ?, $crc32 = ? WHERE $sid = ? AND $position = ?"

  val newSql = s"INSERT OR IGNORE INTO $table ($sid, $timestamp, $msgFlags, $chanFlags, $cltvExpiryDelta, $minMsat, $base, $proportional, $maxMsat, $position, $score, $crc32) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  val selectHavingOneUpdate = s"SELECT $sid FROM $table GROUP BY $sid HAVING COUNT($sid) < 2"

  val selectAllSql = s"SELECT * FROM $table"

  val killSql = s"DELETE FROM $table WHERE $sid = ?"

  val killAllSql = s"DELETE FROM $table"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $sid INTEGER NOT NULL, $timestamp INTEGER NOT NULL, $msgFlags INTEGER NOT NULL, $chanFlags INTEGER NOT NULL,
      $cltvExpiryDelta INTEGER NOT NULL, $minMsat INTEGER NOT NULL, $base INTEGER NOT NULL, $proportional INTEGER NOT NULL,
      $maxMsat INTEGER NOT NULL, $position INTEGER NOT NULL, $score INTEGER NOT NULL, $crc32 INTEGER NOT NULL
    )"""

    // For each channel we have up to two unique updates indexed by nodeId position
    val addIndex = s"CREATE $UNIQUE INDEX IF NOT EXISTS idx1$table ON $table ($sid, $position)"
    createTable :: addIndex :: Nil
  }
}

object NormalChannelUpdateTable extends ChannelUpdateTable("normal_updates", useHeuristics = true)

object HostedChannelUpdateTable extends ChannelUpdateTable("hosted_updates", useHeuristics = false)

abstract class ExcludedChannelTable(val table: String) extends Table {
  val Tuple2(shortChannelId, until) = ("shortchannelid", "excludeduntilstamp")

  val newSql = s"INSERT OR IGNORE INTO $table ($shortChannelId, $until) VALUES (?, ?)"

  val selectSql = s"SELECT * FROM $table WHERE $until > ? LIMIT 1000000"

  val killOldSql = s"DELETE FROM $table WHERE $until < ?"

  def createStatements: Seq[String] = {
    val createTable = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $shortChannelId INTEGER NOT NULL $UNIQUE, $until INTEGER NOT NULL)"
    // Excluded channels expire to give them second chance (e.g. channels with one update, channels without max sendable amount)
    val addIndex = s"CREATE INDEX IF NOT EXISTS idx1$table ON $table ($until)"
    createTable :: addIndex :: Nil
  }

  // This one must be a method, not a value because of late binding of abstract val
  def killPresentInChans = s"DELETE FROM $table WHERE $shortChannelId IN ($selectFromRelatedUpdateTable)"

  val selectFromRelatedUpdateTable: String
}

object NormalExcludedChannelTable extends ExcludedChannelTable("normal_excluded_updates") {
  val selectFromRelatedUpdateTable = s"SELECT ${NormalChannelUpdateTable.sid} FROM ${NormalChannelUpdateTable.table}"
}

object HostedExcludedChannelTable extends ExcludedChannelTable("hosted_excluded_updates") {
  val selectFromRelatedUpdateTable = s"SELECT ${HostedChannelUpdateTable.sid} FROM ${HostedChannelUpdateTable.table}"
}

// Database #3, unrecoverable, but not critically important data, will not go to backup

object RelayTable extends Table {
  val (table, hash, secret, preimage, seenAt, relayed, earned) = ("relay", "hash", "secret", "preimage", "seen", "relayed", "earned")

  val newSql = s"INSERT OR IGNORE INTO $table ($hash, $secret, $preimage, $seenAt, $relayed, $earned) VALUES (?, ?, ?, ?, ?, ?)"

  val selectSummarySql = s"SELECT SUM($relayed), SUM($earned), COUNT($id) FROM $table"

  val selectRecentSql = s"SELECT * FROM $table ORDER BY $id DESC LIMIT ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $hash TEXT NOT NULL, $secret TEXT NOT NULL, $preimage TEXT NOT NULL,
      $seenAt INTEGER NOT NULL, $relayed INTEGER NOT NULL, $earned INTEGER NOT NULL
    )"""

    // Account for many relayed payment sets with same hash but different payment secrets
    val addIndex1 = s"CREATE $UNIQUE INDEX IF NOT EXISTS idx2$table ON $table ($hash, $secret)"
    createTable :: addIndex1 :: Nil
  }
}

object PaymentTable extends Table {
  val (search, table, pr, preimage, status, seenAt, description, action, hash, secret, receivedMsat, sentMsat, feeMsat, balanceMsat, fiatRates, chainFeeMsat, incoming) =
    ("psearch", "payment", "pr", "preimage", "status", "seenAt", "desc", "action", "hash", "secret", "received", "sent", "fee", "balance", "fiatrates", "chainfee", "incoming")

  private val inserts = s"$pr, $preimage, $status, $seenAt, $description, $action, $hash, $secret, $receivedMsat, $sentMsat, $feeMsat, $balanceMsat, $fiatRates, $chainFeeMsat, $incoming"

  val newSql: String = s"INSERT OR IGNORE INTO $table ($inserts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"

  val deleteSql = s"DELETE FROM $table WHERE $hash = ?"

  // Selecting

  val selectByHashSql = s"SELECT * FROM $table WHERE $hash = ?"

  val selectRecentSql: String = {
    val recentFailed = s"($seenAt > ? AND $status = $ABORTED)" // Skip all payments which have been failed a long time ago
    val nonFailedOutgoing = s"($seenAt > 0 AND $status <> $ABORTED AND $incoming = 0)" // Select all outgoing payments which are not failed yet
    val recentPendingIncoming = s"($seenAt > ? AND $status = $PENDING AND $incoming = 1)" // Skip unfulfilled incoming payments which are expired
    val allFulfilledIncoming = s"($seenAt > 0 AND $status = $SUCCEEDED AND $incoming = 1)" // Select all incoming payments which are fulfilled by now
    s"SELECT * FROM $table WHERE $recentFailed OR $nonFailedOutgoing OR $recentPendingIncoming OR $allFulfilledIncoming ORDER BY $id DESC LIMIT ?"
  }

  val selectSummarySql = s"SELECT SUM($feeMsat), SUM($chainFeeMsat), SUM($receivedMsat), SUM($sentMsat), COUNT($id) FROM $table WHERE $status = $SUCCEEDED"

  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $search MATCH ? LIMIT 50)"

  // Updating

  val updOkOutgoingSql = s"UPDATE $table SET $status = $SUCCEEDED, $preimage = ?, $feeMsat = ? WHERE $hash = ? AND ($seenAt > 0 AND status <> $SUCCEEDED AND $incoming = 0)"

  val updOkIncomingSql = s"UPDATE $table SET $status = $SUCCEEDED, $receivedMsat = ?, $seenAt = ? WHERE $hash = ? AND ($seenAt > 0 AND status <> $SUCCEEDED AND $incoming = 1)"

  val updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ? AND ($seenAt > 0 AND status <> $SUCCEEDED)"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $pr TEXT NOT NULL, $preimage TEXT NOT NULL, $status INTEGER NOT NULL, $seenAt INTEGER NOT NULL, $description TEXT NOT NULL,
      $action TEXT NOT NULL, $hash TEXT NOT NULL $UNIQUE, $secret TEXT NOT NULL, $receivedMsat INTEGER NOT NULL, $sentMsat INTEGER NOT NULL,
      $feeMsat INTEGER NOT NULL, $balanceMsat INTEGER NOT NULL, $fiatRates TEXT NOT NULL, $chainFeeMsat INTEGER NOT NULL,
      $incoming INTEGER NOT NULL
    )"""

    val addSearchTable = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $hash)"
    val addIndex1 = s"CREATE INDEX IF NOT EXISTS idx2$table ON $table ($seenAt, $status, $incoming)"
    createTable :: addSearchTable :: addIndex1 :: Nil
  }
}

object TxTable extends Table {
  val (search, table, rawTx, txid, depth, receivedSat, sentSat, feeSat, firstSeen, description, balanceMsat, fiatRates, incoming, doubleSpent) =
    ("tsearch", "txs", "raw", "txid", "depth", "received", "sent", "fee", "seen", "desc", "balance", "fiatrates", "incoming", "doublespent")

  private val inserts = s"$rawTx, $txid, $depth, $receivedSat, $sentSat, $feeSat, $firstSeen, $description, $balanceMsat, $fiatRates, $incoming, $doubleSpent"

  val newSql = s"INSERT OR IGNORE INTO $table ($inserts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  val newVirtualSql = s"INSERT INTO $fts$table ($search, $txid) VALUES (?, ?)"

  val deleteSql = s"DELETE FROM $table WHERE $txid = ?"

  // Selecting, updating

  val selectRecentSql = s"SELECT * FROM $table ORDER BY $id DESC LIMIT ?"

  val selectSummarySql = s"SELECT SUM($feeSat), SUM($receivedSat), SUM($sentSat), COUNT($id) FROM $table WHERE $doubleSpent = 0"

  val searchSql = s"SELECT * FROM $table WHERE $txid IN (SELECT $txid FROM $fts$table WHERE $search MATCH ? LIMIT 50)"

  val updStatusSql = s"UPDATE $table SET $depth = ?, $doubleSpent = ? WHERE $txid = ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $rawTx TEXT NOT NULL, $txid TEXT NOT NULL $UNIQUE, $depth INTEGER NOT NULL,
      $receivedSat INTEGER NOT NULL, $sentSat INTEGER NOT NULL, $feeSat INTEGER NOT NULL, $firstSeen INTEGER NOT NULL,
      $description TEXT NOT NULL, $balanceMsat INTEGER NOT NULL, $fiatRates TEXT NOT NULL, $incoming INTEGER NOT NULL,
      $doubleSpent INTEGER NOT NULL
    )"""

    val addSearchTable = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $txid)"
    createTable :: addSearchTable :: Nil
  }
}

object ChannelTxFeesTable extends Table {
  final val TAG_CHAIN_FEE = "tag-chain-fee"
  final val TAG_DUSTY_HTLC = "tag-dusty-htlc"

  val (table, identifier, tag, feeSat) = ("chantxfees", "identifier", "tag", "fee")

  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $tag, $feeSat) VALUES (?, ?, ?)"

  val selectSummarySql = s"SELECT SUM($feeSat), COUNT($id) FROM $table"

  def createStatements: Seq[String] =
    s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $identifier TEXT NOT NULL $UNIQUE,
      $tag TEXT NOT NULL, $feeSat INTEGER NOT NULL
    )""" :: Nil
}

object DataTable extends Table {
  val (table, label, content) = ("data", "label", "content")

  val newSql = s"INSERT OR IGNORE INTO $table ($label, $content) VALUES (?, ?)"

  val updSql = s"UPDATE $table SET $content = ? WHERE $label = ?"

  val selectSql = s"SELECT * FROM $table WHERE $label = ?"

  val killSql = s"DELETE FROM $table WHERE $label = ?"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $label TEXT NOT NULL $UNIQUE, $content BLOB NOT NULL)" :: Nil
}

object ElectrumHeadersTable extends Table {
  val (table, height, blockHash, header) = ("headers", "height", "blockhash", "header")

  val addHeaderSql = s"INSERT OR IGNORE INTO $table ($height, $blockHash, $header) VALUES (?, ?, ?)"

  val selectByHeightSql = s"SELECT * FROM $table WHERE $height = ?"

  val selectByBlockHashSql = s"SELECT * FROM $table WHERE $blockHash = ?"

  val selectHeadersSql = s"SELECT * FROM $table WHERE $height >= ? ORDER BY $height LIMIT ?"

  val selectTipSql = s"SELECT * FROM $table INNER JOIN (SELECT MAX($height) AS maxHeight FROM $table) t1 ON $height = t1.maxHeight"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $height INTEGER NOT NULL $UNIQUE, $blockHash TEXT NOT NULL, $header BLOB NOT NULL)" :: Nil
}

object PayMarketTable extends Table {
  val (table, search, lnurl, text, lastMsat, lastDate, hash, image) = ("paymarket", "search", "lnurl", "text", "lastmsat", "lastdate", "hash", "image")

  val newSql = s"INSERT OR IGNORE INTO $table ($lnurl, $text, $lastMsat, $lastDate, $hash, $image) VALUES (?, ?, ?, ?, ?, ?)"

  val newVirtualSql = s"INSERT INTO $fts$table ($search, $lnurl) VALUES (?, ?)"

  val selectRecentSql = s"SELECT * FROM $table ORDER BY $lastDate DESC LIMIT ?"

  val searchSql = s"SELECT * FROM $table WHERE $lnurl IN (SELECT $lnurl FROM $fts$table WHERE $search MATCH ?) LIMIT 50"

  val updInfoSql = s"UPDATE $table SET $text = ?, $lastMsat = ?, $lastDate = ?, $hash = ?, $image = ? WHERE $lnurl = ?"

  val killSql = s"DELETE FROM $table WHERE $lnurl = ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $lnurl STRING NOT NULL $UNIQUE, $text STRING NOT NULL,
      $lastMsat INTEGER NOT NULL, $lastDate INTEGER NOT NULL,
      $hash STRING NOT NULL, $image STRING NOT NULL
    )"""

    // Payment links are searchable by their text descriptions (text metadata + domain name)
    val addIndex1 = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $lnurl)"
    val addIndex2 = s"CREATE INDEX IF NOT EXISTS idx1$table ON $table ($lastDate)"
    createTable :: addIndex1 :: addIndex2 :: Nil
  }
}