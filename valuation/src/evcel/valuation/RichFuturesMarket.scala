package evcel.valuation

import evcel.referencedata.calendar.Calendar
import evcel.daterange.{Day, Month}
import scala.util.{Either, Left, Right}
import evcel.utils.{EvcelFail, GeneralEvcelFail}
import evcel.instrument.{Future, HedgeInstrument}
import evcel.referencedata.{FuturesExpiryRule, ReferenceData}
import evcel.referencedata.market.{FuturesMarket, VolumeCalcRule}
import evcel.quantity.{Qty, QtyConversions}
import evcel.utils.EitherUtils._

case class RichFuturesMarket(
  market : FuturesMarket,
  calendar : Calendar,
  expiryRule : FuturesExpiryRule,
  volumeCalcRule : VolumeCalcRule,
  conversions : QtyConversions
){
  def name = market.name
  def priceUOM = market.priceUOM
  def quotedVolumeUOM = market.quotedVolumeUOM
  def frontMonth(observationDay : Day) : Either[EvcelFail, Month] = {
    val month : Month = Iterator.iterate(observationDay.containingMonth)(m => m.next).find(
      m => 
        expiryRule.futureExpiryDay(m) match {
          case Right(d) => d >= observationDay
          case Left(_) => true
        }
    ).get
    expiryRule.futureExpiryDay(month) match {
      case Right(_) => Right(month)
      case Left(_) => Left(GeneralEvcelFail(s"Failed to find observedMonth for $name and $observationDay"))
    }
  }

  def optionExpiryDay(month : Month) : Either[EvcelFail, Day] = expiryRule.optionExpiryDay(month)
  def lastTradingDay(month: Month) = expiryRule.futureExpiryDay(month)

  def unitHedge(month : Month) = {
    HedgeInstrument.intern(
      Future(market.name, month, strike = Qty(0, priceUOM), volume = Qty(1, quotedVolumeUOM))
    )
  }
}

object RichFuturesMarket{
  def apply(refData : ReferenceData, label : String) : Either[EvcelFail, RichFuturesMarket] = {
    for {
      market <- refData.futuresMarket(label)
      expiryRule <- refData.futuresExpiryRule(label)
      calendar <- refData.calendar(market.calendarName)
      volumeCalcRule <- refData.volumeCalcRule(market.volumeCalcRuleLabel)
    } yield{
      RichFuturesMarket(market, calendar, expiryRule, volumeCalcRule, market.conversions)
    }
  }

}
