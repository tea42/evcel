package evcel.instrument

import evcel.daterange.Month
import evcel.quantity.BDQty

case class Future(market: String, delivery: Month, strike: BDQty, volume: BDQty)
  extends Instrument {
}

object Future extends InstrumentType

