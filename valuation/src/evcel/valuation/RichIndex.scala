package evcel.valuation

import evcel.referencedata.calendar.Calendar
import evcel.daterange.{DateRange, Day, Month}
import scala.util.{Either, Right}
import evcel.utils.{EitherUtils, EvcelFail}
import evcel.quantity.{UOM, Qty, QtyConversions}
import evcel.referencedata.ReferenceData
import evcel.referencedata.market.VolumeCalcRule
import evcel.curve.ValuationContext
import evcel.curve.environment.MarketDay._
import evcel.utils.EitherUtils._
import evcel.instrument._

trait RichIndex{
  def index : Index
  def volumeCalcRule : VolumeCalcRule
  def calendar : Calendar
  def price(vc : ValuationContext, observationDay: Day): Either[EvcelFail, Qty]
  def priceUOM : UOM
  def quotedVolumeUOM : UOM
  def observationDays(period: DateRange): Seq[Day]
  def marketConversions: QtyConversions
  def liveObservationDays(vc: ValuationContext, period: DateRange) = {
    observationDays(period).filter(_.endOfDay > vc.marketDay)
  }
  def averagePrice(vc : ValuationContext, days : Seq[Day]) : Either[EvcelFail, Qty] = {
    EitherUtils.mapOrErrorLeft(days, {day : Day => price(vc, day)}).map{
      prices => Qty.average(prices)
    }
  }

  def unitHedge(averagingPeriod : DateRange) = {
    val swap = CommoditySwap(
      index, averagingPeriod, 
      Qty(0, priceUOM),
      Qty(1, quotedVolumeUOM), 
      // TODO - should default biz days be reference data?
      bizDaysToSettlement = None
    )
    HedgeInstrument.intern(swap)
  }
}

abstract class RichFuturesBasedIndex extends RichIndex{
  def market : RichFuturesMarket
  def priceUOM = market.priceUOM
  def quotedVolumeUOM = market.quotedVolumeUOM
  def volumeCalcRule = market.volumeCalcRule
  def calendar = market.calendar
  def observationDays(period: DateRange) = {
    period.days.filter(market.calendar.isBusinessDay).toSeq
  }

  def marketConversions = market.conversions
  def observedMonth(observationDay : Day) : Either[EvcelFail, Month]
}

object RichFuturesBasedIndex{
  def apply(refData : ReferenceData, index : Index) : Either[EvcelFail, RichFuturesBasedIndex] = {
    index match {
      case FuturesFrontPeriodIndex(marketName, nearby, rollEarlyDays) => 
        for {market <- RichFuturesMarket(refData, marketName)}
          yield
            RichFuturesFrontPeriodIndex(market, nearby, rollEarlyDays)

      case FuturesContractIndex(marketLabel, month) => 
        for {market <- RichFuturesMarket(refData, marketLabel)}
          yield
            RichFuturesContractIndex(market, month)
        
    }
  }
}

case class RichFuturesFrontPeriodIndex(market : RichFuturesMarket, nearby : Int, rollEarlyDays : Int) 
  extends RichFuturesBasedIndex
{
  def observedMonth(observationDay : Day) : Either[EvcelFail, Month] = {
    
    market.frontMonth(market.calendar.addBusinessDays(observationDay, rollEarlyDays)).map(
      _ + (nearby - 1)
    )
  }
  val index = FuturesFrontPeriodIndex(market.name, nearby, rollEarlyDays)
  def price(vc : ValuationContext, observationDay : Day) : Either[EvcelFail, Qty] = {
    observedMonth(observationDay).flatMap{
      month => 
        vc.futuresPrice(market.market, month)
    }
  }
}

case class RichFuturesContractIndex(market : RichFuturesMarket, month : Month) extends RichFuturesBasedIndex{
  def observedMonth(observationDay : Day) : Either[EvcelFail, Month] = Right(month)
  val index = FuturesContractIndex(market.name, month)
  // TODO - obs days should take account of last trading day
  def price(vc : ValuationContext, observationDay : Day) : Either[EvcelFail, Qty] = {
    vc.futuresPrice(market.market, month)
  }
}

object RichIndex{
  def apply(refData : ReferenceData, index : Index) : Either[EvcelFail, RichIndex] = {
    index match {
      case SpotMarketIndex(marketLabel) => 
        for (market <- RichSpotMarket(refData, marketLabel))
          yield
            RichSpotMarketIndex(market)
      case _ => 
        RichFuturesBasedIndex(refData, index)
    }
  }

}

case class RichSpotMarketIndex(market : RichSpotMarket) extends RichIndex{
  val index = SpotMarketIndex(market.name)
  def priceUOM = market.priceUOM
  def quotedVolumeUOM = market.quotedVolumeUOM
  def marketConversions = market.conversions
  def calendar = market.calendar
  def volumeCalcRule = market.volumeCalcRule

  def observationDays(period: DateRange) = {
    period.days.filter(market.calendar.isBusinessDay).toSeq
  }

  def price(vc : ValuationContext, observationDay : Day) = {
    // TODO - vc shouldn't need observation day here
    vc.spotPrice(market.market, observationDay)
  }
}

case class RichIndexSpread(index1 : RichIndex, index2 : RichIndex){
  def commonCalendar = new Calendar{
    def isHoliday(day : Day) = index1.calendar.isHoliday(day) || index2.calendar.isHoliday(day)
  }
  def spreadPrice(
    vc : ValuationContext, 
    rule : SwapSpreadPricingRule, 
    period : DateRange, 
    expectedUOM : UOM) : Either[EvcelFail, Qty] = {
    val List(index1Calendar, index2Calendar) = rule match {
      case CommonSwapPricingRule => 
        val cal = CommonSwapPricingRule.calendar(index1.calendar, index2.calendar)
        List(cal, cal)
      case NonCommonSwapPricingRule => 
        List(index1.calendar, index2.calendar)
    }
    val (obsDays1, obsDays2) = (
      period.days.filter(index1Calendar.isBusinessDay), 
      period.days.filter(index2Calendar.isBusinessDay)
    )

      
    for {
      p1 <- index1.averagePrice(vc, obsDays1)
      p1_converted <- p1.in(expectedUOM, index1.marketConversions)
      p2 <- index2.averagePrice(vc, obsDays2)
      p2_converted <- p2.in(expectedUOM, index2.marketConversions)
    }
      yield
        p1_converted - p2_converted
  }
}

object RichIndexSpread{
  def apply(refData : ReferenceData, spread : IndexSpread) : Either[EvcelFail, RichIndexSpread] = {
    for {
      richIndex1 <- RichIndex(refData, spread.index1)
      richIndex2 <- RichIndex(refData, spread.index2)
    }
      yield
        RichIndexSpread(richIndex1, richIndex2)
  }
}
