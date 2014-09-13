package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.markets.{SpotMarket, FuturesMarket}
import evcel.daterange.{Month, DateRange, Day}
import evcel.instrument.{CommoditySwap, Future}
import evcel.quantity.{Qty, BDQty}
import evcel.quantity.Qty._

case class SwapLikeValuer(
  ndx: Index, delivery: DateRange, initialPrice: BDQty, volume: BDQty, settlementDay: Option[Day] = None
  ) {
  def value(vc: ValuationContext) = {
    val observationDays = ndx.observationDays(vc, delivery)
    val price = Qty.average(observationDays.map(d => ndx.price(vc, d)))
    val undiscounted = (price - initialPrice) * volume
    settlementDay.map(d => undiscounted * vc.discountRate(vc.valuationCcy, d).toQty).getOrElse(undiscounted)
  }
}

object SwapLikeValuer {
  def apply(vc: ValuationContext, f: Future) = {
    val ltd = vc.futureExpiryDayOrThrow(f.market, f.delivery)
    new SwapLikeValuer(FuturesContractIndex(f.market, f.delivery), ltd, f.strike, f.volume)
  }
  def apply(vc: ValuationContext, swap: CommoditySwap) = (vc.refData, swap.market) match {
    case FuturesFrontPeriodIndex(ndx) =>
      new SwapLikeValuer(ndx, swap.period, swap.strike, swap.volume)
    case FuturesMarket(market) => swap.period match {
      case m: Month =>
        val ndx = new FuturesContractIndex(market.name, m)
        val ltd = vc.futureExpiryDayOrThrow(market.name, m)
        new SwapLikeValuer(ndx, ltd, swap.strike, swap.volume)
      case o => sys.error(s"Not a valid period $o")
    }
    case SpotMarket(ndx) => new SwapLikeValuer(SpotMarketIndex(ndx.name), swap.period, swap.strike, swap.volume)
    case _ => sys.error(s"Not a matching index ${swap.market}")
  }
}