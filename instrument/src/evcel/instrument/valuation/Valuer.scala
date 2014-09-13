package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.AtomicDatumIdentifier
import evcel.instrument.{CommoditySwap, Future, FuturesOption, Instrument}
import evcel.quantity.Qty

trait Valuer {
  def value(vc: ValuationContext, instr: Instrument): Qty
  def keys(vc: ValuationContext, instr: Instrument): Set[AtomicDatumIdentifier]
}

class DefaultValuer extends Valuer {
  def value(vc: ValuationContext, instr: Instrument): Qty = instr match {
    case fo: FuturesOption => new OptionOnFutureValuer(fo).value(vc)
    case f: Future => SwapLikeValuer(vc, f).value(vc)
    case s: CommoditySwap => SwapLikeValuer(vc, s).value(vc)
    case o => sys.error("No valuation code for " + o)
  }

  def keys(vc: ValuationContext, instr: Instrument): Set[AtomicDatumIdentifier] = {
    val (recordingVC, record) = vc.keyRecordingVC
    value(recordingVC, instr)
    record.keys
  }
}
