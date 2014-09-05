package evcel.curve.marketdata

import evcel.daterange.Day
import evcel.quantity.UOM

trait MarketData {
  def eventStoreKey: MarketData.EventStoreKey
  def marketDay: Day = eventStoreKey.marketDay
}

object MarketData {

  sealed trait EventStoreKey {
    def marketDay: Day
  }

  case class FuturesPricesKey(market: String, marketDay: Day) extends EventStoreKey

  case class ZeroRateDataKey(currency: UOM, marketDay: Day) extends EventStoreKey

  case class FuturesVolKey(market: String, marketDay: Day) extends EventStoreKey
}
