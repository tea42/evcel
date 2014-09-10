package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.daterange.{DateRange, Day}
import evcel.instrument.{CommoditySwap, Future}
import evcel.quantity.BDQty
import evcel.quantity.Qty._

case class SwapLikeValuer(
  ndx: Index, delivery: DateRange, initialPrice: BDQty, volume: BDQty, settlementDay: Option[Day] = None
  ) {
  def value(vc: ValuationContext) = {
    val price = ndx.price(vc, delivery)
    val undiscounted = (price - initialPrice) * volume
    settlementDay.map(d => undiscounted * vc.discountRate(vc.valuationCcy, d).toQty).getOrElse(undiscounted)
  }
}

object SwapLikeValuer {
  def apply(vc: ValuationContext, f: Future) = {
    new SwapLikeValuer(FuturesContractIndex(f.market), f.delivery, f.strike, f.volume)
  }
  def apply(vc: ValuationContext, swap: CommoditySwap) = swap.market match {
    case FuturesFrontPeriodIndex(ndx) =>
      new SwapLikeValuer(ndx, swap.delivery, swap.strike, swap.volume)
    case _ => sys.error("Not a matching index: " + swap.market)
  }
}