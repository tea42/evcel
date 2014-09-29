package evcel.instrument

import evcel.quantity.BDQty

trait Instrument {
  def volume: BDQty
}

trait InstrumentType