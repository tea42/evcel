package evcel.valuation

import evcel.curve.environment.MarketDay
import evcel.referencedata.calendar.Calendar
import evcel.daterange.{DateRange, Day, Month}
import scala.util.{Either, Right}
import evcel.utils.{EitherUtils, EvcelFail}
import evcel.quantity.{UOM, Qty, QtyConversions}
import evcel.referencedata.{Level, ReferenceData}
import evcel.referencedata.market._
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

  /**
   * hasFixed is defined here so we that can override for each index as MarketDay becomes more detailed
   */
  def hasFixed(fixingDay: Day, marketDay: MarketDay) = {
    (marketDay.day == fixingDay && marketDay.timeOfDay.fixingsShouldExist) || marketDay.day > fixingDay
  }

  def unitHedge(averagingPeriod : DateRange) = {
    val swap = CommoditySwap(
      index.label, averagingPeriod,
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
      case ndx:FuturesFrontPeriodIndex =>
        for {market <- RichFuturesMarket(refData, ndx.market.name)}
          yield
            RichFuturesFrontPeriodIndex(ndx, market, ndx.label.nearby, ndx.label.rollEarlyDays)

      case ndx@FuturesContractIndex(indexLabel, market, level) =>
        for {market <- RichFuturesMarket(refData, market.name)}
          yield
            RichFuturesContractIndex(ndx, market, indexLabel.month)
        
    }
  }

  def apply(refData : ReferenceData, label : IndexLabel, level: Level) : Either[EvcelFail, RichFuturesBasedIndex] = {
    Index(refData, label, level).flatMap(i => apply(refData, i))
  }

}

case class RichFuturesFrontPeriodIndex(index: FuturesFrontPeriodIndex,
                                       market : RichFuturesMarket, nearby : Int, rollEarlyDays : Int)
  extends RichFuturesBasedIndex
{
  def observedMonth(observationDay : Day) : Either[EvcelFail, Month] = {
    market.frontMonth(market.calendar.addBusinessDays(observationDay, rollEarlyDays)).map(
      _ + (nearby - 1)
    )
  }

  def lastTradingDay(month: Month) = {
    market.lastTradingDay(month - (nearby - 1)).map(ltd =>
      market.calendar.addBusinessDays(ltd, -rollEarlyDays)
    )
  }

  def price(vc : ValuationContext, observationDay : Day) : Either[EvcelFail, Qty] = {
    observedMonth(observationDay).flatMap(month =>
      lastTradingDay(month).flatMap(ltd =>
        if (hasFixed(ltd, vc.marketDay))
          vc.fixing(index, observationDay)
        else
          vc.futuresPrice(market.market, month)
      )
    )
  }
}

case class RichFuturesContractIndex(index: FuturesContractIndex,
                                    market : RichFuturesMarket, month : Month) extends RichFuturesBasedIndex{
  def observedMonth(observationDay : Day) : Either[EvcelFail, Month] = Right(month)

  def price(vc : ValuationContext, observationDay : Day) : Either[EvcelFail, Qty] = {
    market.lastTradingDay(month).flatMap { ltd =>
      if (hasFixed(ltd, vc.marketDay)) {
        vc.fixing(index, observationDay)
      } else {
        vc.futuresPrice(market.market, month)
      }
    }
  }

  def forwardPriceOrLTDFixing(vc: ValuationContext): Either[EvcelFail, Qty] = {
    market.lastTradingDay(month).flatMap { ltd =>
      if (hasFixed(ltd, vc.marketDay)) {
        vc.fixing(index, ltd)
      } else {
        vc.futuresPrice(market.market, month)
      }
    }
  }
}

object RichFuturesContractIndex {
  def apply(refData : ReferenceData, label: FuturesContractIndexLabel, level: Level)
        : Either[EvcelFail, RichFuturesContractIndex] = {
    for(fm <- refData.futuresMarket(label.marketName);
        fci = FuturesContractIndex(label, fm, level);
        rfm <- RichFuturesMarket(refData, fm.name)
    ) yield RichFuturesContractIndex(fci, rfm, label.month)
  }
}

object RichIndex{
  def apply(refData : ReferenceData, index : Index) : Either[EvcelFail, RichIndex] = {
    index match {
      case s@SpotIndex(spotMarket) =>
        for (market <- RichSpotMarket(refData, spotMarket.name))
          yield
            RichSpotMarketIndex(s, market)
      case _ => 
        RichFuturesBasedIndex(refData, index)
    }
  }

  def apply(refData : ReferenceData, label : IndexLabel, level: Level) : Either[EvcelFail, RichIndex] = {
    Index(refData, label, level).flatMap(i => apply(refData, i))
  }
}

case class RichSpotMarketIndex(index: SpotIndex, market : RichSpotMarket) extends RichIndex{
  def priceUOM = market.priceUOM
  def quotedVolumeUOM = market.quotedVolumeUOM
  def marketConversions = market.conversions
  def calendar = market.calendar
  def volumeCalcRule = market.volumeCalcRule

  def observationDays(period: DateRange) = {
    period.days.filter(market.calendar.isBusinessDay).toSeq
  }

  def price(vc : ValuationContext, observationDay : Day) = {
    if(hasFixed(observationDay, vc.marketDay))
      vc.fixing(index, observationDay)
    else
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
      index1 <- Index(refData, spread.spread.index1, spread.level1)
      index2 <- Index(refData, spread.spread.index2, spread.level2)
      richIndex1 <- RichIndex(refData, index1)
      richIndex2 <- RichIndex(refData, index2)
    }
      yield
        RichIndexSpread(richIndex1, richIndex2)
  }
}
