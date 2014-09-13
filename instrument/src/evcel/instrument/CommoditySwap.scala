package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty

/**
 *
 */
case class CommoditySwap(market: String, period: DateRange, strike: BDQty, volume: BDQty)
  extends Instrument {
}



