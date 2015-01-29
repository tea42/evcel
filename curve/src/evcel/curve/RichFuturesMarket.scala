package evcel.curve

import evcel.daterange.{Day, Month}
import evcel.quantity.{Qty, QtyConversions}
import evcel.referencedata.calendar.Calendar
import evcel.referencedata.market.{FuturesContractIndexLabel, FuturesMarket, VolumeCalcRule}
import evcel.referencedata.{FuturesExpiryRule, ReferenceData}
import evcel.utils.EitherUtils._
import evcel.utils.{EvcelFail, GeneralEvcelFail}

import scala.util.{Either, Left, Right}

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

  /**
   * Returns the forward price for the month if it is still live on vc.marketDay,
   * otherwise returns the fixing as observed on the last trading day of the month.
   */
  def price(vc: ValuationContext, month: Month): Either[EvcelFail, Qty] = {
    index(vc.refData, month).forwardPriceOrLTDFixing(vc)
  }

  def index(refData: ReferenceData, month: Month): RichFuturesContractIndex = {
    RichFuturesContractIndex(refData, FuturesContractIndexLabel(market.name, month), market.level).getOrErrorLeft(
      // since we already have a rich market, going to rich index shouldn't fail
      fail => sys.error(s"Couldn't create RichIndex from ${this.market}, $month")
    )
  }

  def optionExpiryDay(month : Month) : Either[EvcelFail, Day] = expiryRule.optionExpiryDay(month)
  def lastTradingDay(month: Month) = expiryRule.futureExpiryDay(month)
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
