package evcel.curve.marketdata

import evcel.referencedata.ReferenceData
import evcel.curve.curves.DiscountCurve
import evcel.curve.environment.{Curve, MarketDay}
import evcel.curve.marketdata.MarketData.CantBuildCurve
import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.quantity.BDQty

case class ZeroRateData(dayCount: DayCount, rates: List[(Day, BDQty)])
  extends MarketData
{
  def buildCurve(currency: UOM, marketDay: MarketDay) = {
    Right(DiscountCurve(currency, marketDay.day, dayCount, rates))
  }
}

