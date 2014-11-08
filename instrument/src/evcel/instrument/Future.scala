package evcel.instrument

import evcel.daterange.Month
import evcel.instrument.valuation.HedgeInstrument
import evcel.quantity.BDQty

case class Future(market: String, delivery: Month, strike: BDQty, volume: BDQty)
  extends Instrument with HedgeInstrument {
}

object Future extends InstrumentType

