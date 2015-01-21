package evcel.valuation

import evcel.referencedata.calendar.Calendar
import scala.util.Either
import evcel.utils.EvcelFail
import evcel.utils.EitherUtils._
import evcel.referencedata.ReferenceData
import evcel.referencedata.market.{SpotMarket, VolumeCalcRule}

case class RichSpotMarket(
  market : SpotMarket,
  calendar : Calendar, 
  volumeCalcRule : VolumeCalcRule
){
  def name = market.name
  def priceUOM = market.priceUOM
  def quotedVolumeUOM = market.quotedVolumeUOM
  def conversions = market.conversions
}

object RichSpotMarket{
  def apply(refData : ReferenceData, name : String) : Either[EvcelFail, RichSpotMarket] = {
    for {
      spotMarket <- refData.markets.spotMarket(name)
      volumeCalcRule <- refData.volumeCalcRule(spotMarket.volumeCalcRuleLabel)
      calendar <- refData.calendar(spotMarket.calendarName)
    } yield 
      RichSpotMarket(spotMarket, calendar, volumeCalcRule)
      
  }
}
