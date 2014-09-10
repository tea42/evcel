package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.daterange.{Day, DateRange, Month}
import evcel.quantity.Qty

trait Index {
  def price(vc: ValuationContext, delivery: DateRange): Qty
}

case class FuturesContractIndex(marketName: String) extends Index {
  override def price(vc: ValuationContext, delivery: DateRange) = delivery match {
    case m: Month => vc.futuresPrice(marketName, m)
    case o => sys.error("Can't handle FuturesContractIndex with period: " + o)
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

  def observationDays(vc: ValuationContext, delivery: DateRange) = {
    val calendar = vc.futuresCalendarOrThrow(marketName)
    delivery.days.filter(calendar.isBusinessDay)
  }

  def observed(vc: ValuationContext, delivery: DateRange): Map[Day, Month] = {
    val market = vc.futuresMarketOrThrow(this.marketName)
    observationDays(vc, delivery).map(d => d -> market.observedMonth(vc.refData, d)).toMap
  }

  override def price(vc: ValuationContext, delivery: DateRange) = {
    val months = observed(vc, delivery).values
    val prices = months.map(m => vc.futuresPrice(marketName, m))
    Qty.average(prices)
  }
}

object FuturesFrontPeriodIndex {
  val Parse = """(.+) nearby ([0-9]+)[ ]?(roll )?([0-9]?)""".r

  def unapply(name: String): Option[FuturesFrontPeriodIndex] = name match {
    case Parse(market, nearby, _, roll) =>
      val nearbyNum = if (nearby.isEmpty) 1 else nearby.toInt
      val rollNum = if (roll.isEmpty) 0 else roll.toInt
      Some(new FuturesFrontPeriodIndex(
        market, nearbyNum, rollNum
      ))
    case _ => None
  }
}
