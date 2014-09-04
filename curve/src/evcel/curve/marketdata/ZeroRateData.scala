package evcel.curve.marketdata

import evcel.quantity.Percentage
import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.curve.marketdata.MarketData._
import evcel.quantity.Qty

case class ZeroRateData(
    currency: UOM,
    override val marketDay: Day,
    dayCount: DayCount,
    rates: List[(Day, Qty)]) extends MarketData {
  def eventStoreKey = ZeroRateDataKey(currency, marketDay)
}
