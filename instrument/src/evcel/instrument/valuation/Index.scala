package evcel.instrument.valuation

import evcel.referencedata.calendar.Calendar
import evcel.referencedata.ReferenceData
import evcel.curve.ValuationContext
import evcel.daterange.{DateRange, Day, Month}
import evcel.quantity.{QtyConversions, Qty}
import evcel.curve.environment.MarketDay._

trait Index {
  def price(vc: ValuationContext, observationDay: Day): Qty
  def observationDays(vc: ValuationContext, period: DateRange): Seq[Day]

  def liveObservationDays(vc: ValuationContext, period: DateRange) = {
    observationDays(vc, period).filter(_.endOfDay > vc.marketDay)
  }

  def indexName = toString

  def calendar(refData: ReferenceData): Calendar
  def marketConversions(refData: ReferenceData): Option[QtyConversions]
}

object Index {
  def parse(name: String)(implicit refData: ReferenceData): Option[Index] = refData.indexCache.memoize(name) {
    FuturesContractIndex.parse(name).orElse(FuturesFrontPeriodIndex.parse(name)).orElse(SpotMarketIndex.parse(name))
  }
}

trait FuturesDerivedIndex extends Index {
  def underlyingMarketName: String
}

case class FuturesContractIndex(marketName: String, month: Month) extends FuturesDerivedIndex {
  override def price(vc: ValuationContext, observationDay: Day) = {
    // when we have fixings
    // if(expired) vc.fixing(marketName, observationDay, month)
    vc.futuresPrice(marketName, month)
  }

  override def observationDays(vc: ValuationContext, period: DateRange) = period match {
    case d: Day => d :: Nil
    case o => sys.error("Not valid: " + o)
  }

  override def calendar(refData: ReferenceData) = {
    refData.markets.futuresMarket(marketName).flatMap(m =>
      refData.calendars.calendar(m.calendarName)
    ).getOrElse(sys.error(s"No calendar for $marketName"))
  }

  override def marketConversions(refData: ReferenceData) = {
    refData.markets.futuresMarket(marketName).flatMap(_.conversions)
  }

  override def underlyingMarketName = marketName

  override def toString = s"$marketName ($month)"
}

object FuturesContractIndex {
  val Parse = ("""(.*) \(""" + Month.Format + """+\)""").r

  def parse(name: String)(implicit refData: ReferenceData): Option[FuturesContractIndex] = {
    name match {
      case Parse(market, yy, mm) => refData.markets.futuresMarket(market).map(
        fm => FuturesContractIndex(fm.name, Month(yy.toInt, mm.toInt))
      )
      case _ => None
    }
  }

  def fromMarketAndPeriod(refData: ReferenceData, marketName: String, period: DateRange) = {
    val market = refData.markets.futuresMarket(marketName)
    market.flatMap(fm => period match {
      case month: Month =>
        Some(new FuturesContractIndex(fm.name, month))
      case o => None
    })
  }
}

case class SpotMarketIndex(marketName: String) extends Index {
  override def price(vc: ValuationContext, observationDay: Day) = {
    vc.spotPrice(marketName, observationDay)
  }

  override def observationDays(vc: ValuationContext, period: DateRange) = {
    vc.refData.indexCache.memoize((this, "observationDays", period)) {
      val calendar = vc.spotCalendarOrThrow(marketName)
      period.days.filter(calendar.isBusinessDay).toSeq
    }
  }

  override def calendar(refData: ReferenceData) = {
    refData.markets.spotMarket(marketName).flatMap(m =>
      refData.calendars.calendar(m.calendarName)
    ).getOrElse(sys.error(s"No calendar for $marketName"))
  }

  override def marketConversions(refData: ReferenceData) = {
    refData.markets.spotMarket(marketName).flatMap(_.conversions)
  }
  override def toString = marketName
}

object SpotMarketIndex {
  def parse(name: String)(implicit refData: ReferenceData): Option[SpotMarketIndex] = {
    refData.markets.spotMarket(name).map(s => SpotMarketIndex(s.name))
  }
}

case class FuturesFrontPeriodIndex(marketName: String, nearby: Int, rollEarlyDays: Int) extends FuturesDerivedIndex {
  require(nearby > 0, "nearby: " + nearby)
  require(rollEarlyDays >= 0, "rollEarlyDays: " + rollEarlyDays)

  override def toString = {
    val nearbyString = " nearby " + nearby
    val rollString = if (rollEarlyDays == 0) "" else " roll " + rollEarlyDays
    marketName + nearbyString + rollString
  }

  def observationDays(vc: ValuationContext, period: DateRange) = {
    vc.refData.indexCache.memoize((this, "observationDays", period)) {
      val cal = calendar(vc.refData)
      period.days.filter(cal.isBusinessDay).toSeq
    }
  }

  override def price(vc: ValuationContext, observationDay: Day) = {
    val observed = observedDaysToMonth(vc, observationDay)(observationDay)
    vc.futuresPrice(marketName, observed)
  }

  override def calendar(refData: ReferenceData) = {
    refData.markets.futuresMarket(marketName).flatMap(m =>
      refData.calendars.calendar(m.calendarName)
    ).getOrElse(sys.error(s"No calendar for $marketName"))
  }

  override def marketConversions(refData: ReferenceData) = {
    refData.markets.futuresMarket(marketName).flatMap(_.conversions)
  }

  override def underlyingMarketName = marketName

  private[instrument] def observedDaysToMonth(
    vc: ValuationContext, delivery: DateRange
    ): Map[Day, Month] = {
    val fm = vc.futuresMarketOrThrow(marketName)
    val calendar = vc.futuresCalendarOrThrow(marketName)
    val observationDays = delivery.days.filter(calendar.isBusinessDay).map(
      calendar.addBusinessDays(_, -rollEarlyDays)
    )
    observationDays.map(d => d -> (fm.frontMonth(vc.refData, d) + (nearby - 1))).toMap
  }
}

object FuturesFrontPeriodIndex {
  val Parse = """(.+) nearby ([0-9]+)[ ]?(roll )?([0-9]?)""".r

  def parse(name: String)(implicit refData: ReferenceData) = name match {
    case Parse(market, nearby, _, roll) =>
      val nearbyNum = if (nearby.isEmpty) 1 else nearby.toInt
      val rollNum = if (roll.isEmpty) 0 else roll.toInt
      refData.markets.futuresMarket(market).map(_ =>
        new FuturesFrontPeriodIndex(
          market, nearbyNum, rollNum
        )
      )
    case _ => None
  }
}

case class IndexSpread(index1: Index, index2: Index) {
  override def toString = s"$index1 vs $index2"

  def calendar(refData: ReferenceData, rule: SwapPricingRule) = {
    rule.calendar(index1.calendar(refData), index2.calendar(refData))
  }
}

object IndexSpread {
  val Parse = """(.+?) vs (.+)""".r

  def parse(name: String)(implicit refData: ReferenceData): Option[IndexSpread] = {
    refData.indexSpreadCache.memoize(name) {
      name match {
        case Parse(i1, i2) =>
          for (index1 <- Index.parse(i1); index2 <- Index.parse(i2))
          yield IndexSpread(index1, index2)
        case _ => None
      }
    }
  }
}
