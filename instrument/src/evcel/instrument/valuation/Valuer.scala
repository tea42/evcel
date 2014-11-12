package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{PriceIdentifier, AtomicDatumIdentifier}
import evcel.instrument._
import evcel.quantity.Qty
import evcel.quantity.BDQty
import evcel.referencedata.market.FXPair

trait Valuer {
  def value(vc: ValuationContext, instr: Instrument): Qty
  def keys(vc: ValuationContext, instr: Instrument): Set[AtomicDatumIdentifier]

  def priceKeys(vc: ValuationContext, instr: Instrument): Set[PriceIdentifier] = {
    keys(vc, instr).flatMap{case pi: PriceIdentifier => Some(pi); case _ => None}
  }

  def firstOrderPriceDiff(vc: ValuationContext, instr: Instrument, pi: PriceIdentifier): Qty = {
    val dP = pi.dP(vc)
    val mtmUp = value(vc.shiftPrice(pi, dP), instr)
    val mtmDown = value(vc.shiftPrice(pi, -dP), instr)
    (mtmUp - mtmDown) / (2 * dP)
  }
  def positions(vc: ValuationContext, instr: Instrument): Iterable[HedgeInfo]
}

object Valuer {
  implicit class RichValuerInstrument(instr: Instrument) {
    def mtm(vc: ValuationContext)(implicit valuer: Valuer) = valuer.value(vc, instr)
    def keys(vc: ValuationContext)(implicit valuer: Valuer) = valuer.keys(vc, instr)
    def priceKeys(vc: ValuationContext)(implicit valuer: Valuer) = valuer.priceKeys(vc, instr)

    def firstOrderPriceDiff(vc: ValuationContext, pi: PriceIdentifier)(implicit valuer: Valuer) = {
      valuer.firstOrderPriceDiff(vc, instr, pi)
    }
    def positions(vc: ValuationContext)(implicit valuer : Valuer) = valuer.positions(vc, instr)
  }
}

trait Valuation {
  def value: Qty

}

class DefaultValuer extends Valuer {
  implicit val valuer: Valuer = this

  def value(vc: ValuationContext, instr: Instrument): Qty = {
    val mtm = valuer(vc, instr).value.inBaseCcy
    if(mtm.uom != vc.valuationCcy) {
      val rate = vc.todayFX(FXPair(mtm.uom, vc.valuationCcy))
      mtm * rate
    } else {
      mtm
    }
  }

  def valuer(vc: ValuationContext, instr: Instrument): Valuation = instr match {
    case fo: FuturesOption => new OptionOnFutureValuer(vc, fo)
    case f: Future => SwapLikeValuer(vc, f)
    case s: CommoditySwap => SwapLikeValuer(vc, s)
    case s: CommoditySwapSpread => SwapLikeValuer(vc, s)
    case s: CommoditySwapLookalike => SwapLikeValuer(vc, s)
    case f: FXForward => FXForwardValuer(vc, f)
    case c: Cash => CashValuer(vc, c)
    case o => sys.error("No valuation code for " + o)
  }

  def keys(vc: ValuationContext, instr: Instrument): Set[AtomicDatumIdentifier] = {
    val (recordingVC, record) = vc.keyRecordingVC
    value(recordingVC, instr)
    record.keys
  }

  def positions(vc: ValuationContext, instr: Instrument): Iterable[HedgeInfo] =  
    SVDPositions.positions(vc, instr)(this)
}
