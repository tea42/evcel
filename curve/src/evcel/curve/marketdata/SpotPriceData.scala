package evcel.curve.marketdata

import evcel.curve.ReferenceData
import evcel.curve.curves.SpotPrices
import evcel.curve.environment.{Curve, MarketDay}
import evcel.curve.marketdata.MarketData.CantBuildCurve
import evcel.daterange.DateRange
import evcel.quantity.BDQty

case class SpotPriceData(prices : List[(DateRange, BDQty)])
  extends MarketData
{
  def buildCurve(market: String, marketDay: MarketDay): Either[CantBuildCurve, SpotPrices] = {
    Right(SpotPrices(market, marketDay, prices.toMap))
  }
}
