package evcel.curve.marketdata

import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.curve.marketdata.MarketData._
import evcel.quantity.BDQty

case class ZeroRateData(
    currency: UOM,
    override val marketDay: Day,
    dayCount: DayCount,
    rates: List[(Day, BDQty)]) extends MarketData {
  def eventStoreKey = ZeroRateDataKey(currency, marketDay)
}
