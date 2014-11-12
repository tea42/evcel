package evcel.curve.marketdata

import evcel.curve.curves.DiscountCurve
import evcel.curve.environment.MarketDay
import evcel.daterange.Day
import evcel.quantity.{BDQty, UOM}

case class ZeroRateData(dayCount: DayCount, rates: List[(Day, BDQty)])
  extends MarketData
{
  require(rates.map(_._1).distinct.size == rates.size, "Duplicate days in rates: " + rates)

  def buildCurve(currency: UOM, marketDay: MarketDay) = {
    Right(DiscountCurve(currency, marketDay.day, dayCount, rates))
  }
}

