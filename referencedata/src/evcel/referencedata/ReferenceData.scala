package evcel.referencedata

import evcel.referencedata.calendar.{Calendars, Calendar}
import evcel.referencedata.market._
import evcel.utils.{Cache, EvcelFail, GeneralEvcelFail}
import scala.util.{Either, Left, Right}

trait ReferenceDataTrait
trait ReferenceDataIdentifier

case class CalendarIdentifier(name : String) extends ReferenceDataIdentifier

object ReferenceDataTrait{
  val CALENDAR = "Calendar"
  val FUTURES_EXPIRY_RULE = "FuturesExpiryRule"
  val FUTURES_MARKET = "FuturesMarket"
  val CURRENCY = "Currency"
}

case class ReferenceData(
  futuresExpiryRules: FuturesExpiryRules,
  calendars: Calendars,
  markets: Markets
  ) {

  val indexCache = Cache.createStaticCache("ReferenceData.indexCache")
  val indexSpreadCache = Cache.createStaticCache("ReferenceData.indexSpreadCache")

  def fxMarket(pair: FXPair) = {
    FXMarket(this, pair)
  }

  def volumeCalcRule(label : VolumeCalcRuleLabel) : Either[EvcelFail, VolumeCalcRule] = label match {
    case VolumeCalcRuleLabel.DailyPower => Right(DailyPowerVolumeCalcRule)
    case VolumeCalcRuleLabel.Default => Right(DefaultVolumeCalcRule)
    case other => Left(GeneralEvcelFail(s"Unknown volume calc rule - $other"))
  }

  def futuresMarket(label : String) = markets.futuresMarket(label)
  def futuresExpiryRule(label : String) : Either[EvcelFail, FuturesExpiryRule] = {
    futuresExpiryRules.expiryRule(label)
  }
  def calendar(label : String) : Either[EvcelFail, Calendar] = {
    calendars.calendar(label)
  }

}
