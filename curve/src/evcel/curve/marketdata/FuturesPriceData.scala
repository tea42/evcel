package evcel.curve.marketdata

import evcel.quantity.BDQty
import evcel.daterange.Month
import scala.collection.SortedMap
import evcel.daterange.Day
import evcel.curve.marketdata.MarketData.FuturesPricesKey

case class FuturesPriceData(override val marketDay: Day, market: String, prices: List[(Month, BDQty)]) 
extends MarketData 
{
  def eventStoreKey = FuturesPricesKey(market, marketDay)
}
