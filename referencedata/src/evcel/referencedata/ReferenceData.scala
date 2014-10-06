package evcel.referencedata

import evcel.referencedata.calendar.Calendars
import evcel.referencedata.market.Markets
import evcel.utils.Cache

trait ReferenceDataTrait
trait ReferenceDataIdentifier

case class CalendarIdentifier(name : String) extends ReferenceDataIdentifier

object ReferenceDataTrait{
  val CALENDAR = "Calendar"
  val FUTURES_EXPIRY_RULE = "FuturesExpiryRule"
  val FUTURES_MARKET = "FuturesMarket"
}

case class ReferenceData(
  futuresExpiryRules: FuturesExpiryRules,
  calendars: Calendars,
  markets: Markets
  ) {

  val indexCache = Cache.createStaticCache("ReferenceData.indexCache")
  val indexSpreadCache = Cache.createStaticCache("ReferenceData.indexSpreadCache")
}
