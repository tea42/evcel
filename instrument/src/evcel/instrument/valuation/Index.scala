package evcel.instrument.valuation

import evcel.curve.{ReferenceData, ValuationContext}
import evcel.daterange.{DateRange, Day, Month}
import evcel.quantity.Qty

trait Index {
  def price(vc: ValuationContext, observationDay: Day): Qty
  def observationDays(vc: ValuationContext, period: DateRange): Iterable[Day]
}

case class FuturesContractIndex(marketName: String, month: Month) extends Index {
  override def price(vc: ValuationContext, observationDay: Day) = {
    // when we have fixings
    // if(expired) vc.fixing(marketName, observationDay, month)
    vc.futuresPrice(marketName, month)
  }

  override def observationDays(vc: ValuationContext, period: DateRange) = period match {
    case d: Day => d :: Nil
    case o => sys.error("Not valid: " + o)
  }
}

case class SpotMarketIndex(marketName: String) extends Index {
  override def price(vc: ValuationContext, observationDay: Day) = {
    vc.spotPrice(marketName, observationDay)
  }

  override def observationDays(vc: ValuationContext, period: DateRange) = {
    val calendar = vc.spotCalendarOrThrow(marketName)
    period.days.filter(calendar.isBusinessDay).toIterable
  }
}

case class FuturesFrontPeriodIndex(marketName: String, nearby: Int, rollEarlyDays: Int) extends Index {
  require(nearby > 0, "nearby: " + nearby)
  require(rollEarlyDays >= 0, "rollEarlyDays: " + rollEarlyDays)

  override def toString = {
    val nearbyString = " nearby " + nearby
    val rollString = if (rollEarlyDays == 0) "" else " roll " + rollEarlyDays
    marketName + nearbyString + rollString
  }

  def observationDays(vc: ValuationContext, period: DateRange) = {
    val calendar = vc.futuresCalendarOrThrow(marketName)
    period.days.filter(calendar.isBusinessDay).toIterable
  }

  override def price(vc: ValuationContext, observationDay: Day) = {
    val observed = observedDaysToMonth(vc, observationDay)(observationDay)
    vc.futuresPrice(marketName, observed)
  }

  private[instrument] def observedDaysToMonth(
    vc: ValuationContext, delivery: DateRange
    ): Map[Day, Month] = {
    val fm = vc.futuresMarketOrThrow(marketName)
    val calendar = vc.futuresCalendarOrThrow(marketName)
    val observationDays = delivery.days.filter(calendar.isBusinessDay).toIterable.map(
      calendar.addBusinessDays(_, -rollEarlyDays)
    )
    observationDays.map(d => d -> (fm.frontMonth(vc.refData, d) + (nearby - 1))).toMap
  }
}

object FuturesFrontPeriodIndex {
  val Parse = """(.+) nearby ([0-9]+)[ ]?(roll )?([0-9]?)""".r

  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => name match {
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
    case _ => None
  }
}
