package evcel.curve.marketdata

import evcel.curve.ReferenceData
import evcel.curve.curves.FuturesPrices
import evcel.curve.environment.{FuturesPricesIdentifier, MarketDataIdentifier, MarketDay, Curve}
import evcel.quantity.BDQty
import evcel.daterange.Month
import scala.collection.SortedMap
import evcel.daterange.Day

case class FuturesPriceData(prices: List[(Month, BDQty)]) 
  extends MarketData
{
  def buildCurve(market : String, marketDay: MarketDay): Either[MarketData.CantBuildCurve, FuturesPrices] =
    Right(FuturesPrices(market, marketDay, prices.toMap))
}

object FuturesPriceData{
  def apply(firstPrice : (Month, BDQty) , otherPrices : (Month, BDQty) *) : FuturesPriceData = 
    FuturesPriceData(firstPrice :: otherPrices.toList)
}
