package evcel.curve

import evcel.referencedata.ReferenceData
import evcel.referencedata.calendar.Calendar
import evcel.referencedata.market.{SpotMarket, VolumeCalcRule}
import evcel.utils.EitherUtils._
import evcel.utils.EvcelFail

import scala.util.Either

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
