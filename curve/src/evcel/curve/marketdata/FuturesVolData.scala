package evcel.curve.marketdata

import evcel.quantity.BDQty
import evcel.daterange.Month
import evcel.daterange.Day

case class FuturesVolData(
  market: String, override val marketDay: Day,
  data: List[(Month, List[(Double, BDQty)])])
    extends MarketData {
  def eventStoreKey = MarketData.FuturesVolKey(market, marketDay)
}
