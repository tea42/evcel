package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.instrument.{InstrumentType, FuturesOption, Instrument}
import evcel.quantity.{Qty, UOM}


object Valuer { // maybe an instance should be passed around with a valuation context? object for now
  def value(vc: ValuationContext, ccy: UOM, instr: Instrument): Qty = instr match {
    case fo: FuturesOption => new OptionOnFutureValuer(fo).value(vc, ccy)
    case o => sys.error("No valuation code for " + o)
  }
}
