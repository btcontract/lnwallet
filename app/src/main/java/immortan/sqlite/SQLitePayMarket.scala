package immortan.sqlite

import immortan.{ChannelMaster, PayLinkInfo}
import immortan.utils.{LNUrl, PayRequest}
import fr.acinq.eclair.MilliSatoshi


class SQLitePayMarket(db: DBInterface) {
  def remove(lnUrl: LNUrl): Unit = db.change(PayMarketTable.killSql, lnUrl.request)

  def updateLabel(lnUrl: LNUrl, newLabel: String): Unit = {
    db.change(PayMarketTable.newVirtualSql, newLabel, lnUrl.request)
    db.change(PayMarketTable.updLabelSql, newLabel)
  }

  def saveLink(lnUrl: LNUrl, payReq: PayRequest, msat: MilliSatoshi, hash: String): Unit = db txWrap {
    val stamp = System.currentTimeMillis: java.lang.Long
    val lastMsat = msat.toLong: java.lang.Long

    db.change(PayMarketTable.updInfoSql, payReq.metadata, lastMsat, stamp, hash, lnUrl.request)
    db.change(PayMarketTable.newSql, lnUrl.request, payReq.metadata, lastMsat, stamp, hash, new String)
    db.change(PayMarketTable.newVirtualSql, payReq.meta.queryText, lnUrl.request)
    ChannelMaster.next(ChannelMaster.payMarketDbStream)
  }

  def searchLinks(rawSearchQuery: String): RichCursor = db.search(PayMarketTable.searchSql, rawSearchQuery)

  def listRecentLinks(limit: Int): RichCursor = db.select(PayMarketTable.selectRecentSql, limit.toString)

  def toLinkInfo(rc: RichCursor): PayLinkInfo =
    PayLinkInfo(lnurlString = rc string PayMarketTable.lnurl, metaString = rc string PayMarketTable.meta,
      lastMsat = MilliSatoshi(rc long PayMarketTable.lastMsat), lastDate = rc long PayMarketTable.lastDate,
      labelString = rc string PayMarketTable.label)
}