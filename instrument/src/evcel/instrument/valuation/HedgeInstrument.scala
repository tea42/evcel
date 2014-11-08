package evcel.instrument.valuation

import evcel.instrument.Instrument
import evcel.quantity.Qty

trait HedgeInstrument extends Instrument {
  def volume: Qty
}
