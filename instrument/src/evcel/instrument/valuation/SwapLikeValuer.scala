package evcel.instrument.valuation

import evcel.referencedata.calendar.Calendar
import evcel.curve.ValuationContext
import evcel.curve.environment.PriceIdentifier
import evcel.referencedata.market.{SpotMarket, FuturesMarket}
import evcel.daterange.{Month, DateRange, Day}
import evcel.instrument.{CommoditySwapLookalike, CommoditySwap, Future}
import evcel.quantity.{Qty, BDQty}
import evcel.quantity.Qty._
import evcel.curve.environment.MarketDay._

trait SwapLikeValuer {
  def calendar: Calendar

  def value: Qty

  def observationDays: Seq[Day]
  def liveObservationDays: Seq[Day]
}

case class SingleUnderlyingSwapLikeValuer(
  vc: ValuationContext, index: Index, period: DateRange,
  initialPrice: BDQty, volume: BDQty, settlementDay: Option[Day] = None
  ) extends SwapLikeValuer {
  def isLive = index.liveObservationDays(vc, period).nonEmpty

  def calendar = index.calendar(vc.refData)

  def observationDays: Seq[Day] = index.observationDays(vc, period)
  def liveObservationDays: Seq[Day] = index.liveObservationDays(vc, period)

  def value = {
    val price = Qty.average(observationDays.map(d => index.price(vc, d)))
    val undiscounted = (price - initialPrice) * volume
    settlementDay.map(d => undiscounted * vc.discountRate(vc.valuationCcy, d).toQty).getOrElse(undiscounted)
  }
}

case class SwapSpreadLikeValuer(
  vc: ValuationContext, spread: IndexSpread, rule: SwapPricingRule, period: DateRange,
  initialPrice: BDQty, volume: BDQty, settlementDay: Option[Day] = None
  ) extends SwapLikeValuer {
  val (index1, index2) = (spread.index1, spread.index2)

  def indexes = List(index1, index2)

  def observationDays: Seq[Day] = {
    val cal = calendar
    (index1.observationDays(vc, period) ++ index2.observationDays(vc, period)).filter(cal.isBusinessDay).toSeq
  }

  def observationDays(index: Index): Iterable[Day] = {
    index.observationDays(vc, period).toSet.intersect(observationDays.toSet).toList
  }

  def liveObservationDays = observationDays.filter(_.endOfDay > vc.marketDay)

  def isLive = liveObservationDays.nonEmpty

  def calendar = rule.calendar(index1.calendar(vc.refData), index2.calendar(vc.refData))

  def value = {
    def convUOM(price: Qty, index: Index) = {
      val conversions = index.marketConversions(vc.refData)
      val uom = initialPrice.uom
      price.in(uom, conversions).getOrElse(sys.error(s"Couldn't convert $price to $uom ($index)"))
    }

    val price1 = convUOM(Qty.average(observationDays(index1).map(d => index1.price(vc, d))), index1)
    val price2 = convUOM(Qty.average(observationDays(index2).map(d => index2.price(vc, d))), index2)

    val undiscounted = (price1 - price2 - initialPrice) * volume
    settlementDay.map(d => undiscounted * vc.discountRate(vc.valuationCcy, d).toQty).getOrElse(undiscounted)
  }
}

object SwapLikeValuer {
  def apply(vc: ValuationContext, f: Future): SingleUnderlyingSwapLikeValuer = {
    val ltd = vc.futureExpiryDayOrThrow(f.market, f.delivery)
    new SingleUnderlyingSwapLikeValuer(vc, FuturesContractIndex(f.market, f.delivery), ltd, f.strike, f.volume)
  }

  def apply(vc: ValuationContext, swap: CommoditySwap): SwapLikeValuer = (vc.refData, swap.market) match {
    case FuturesFrontPeriodIndex(ndx) =>
      new SingleUnderlyingSwapLikeValuer(vc, ndx, swap.averagingPeriod, swap.strike, swap.volume)
    case FuturesContractIndex(ndx) =>
      new SingleUnderlyingSwapLikeValuer(vc, ndx, swap.averagingPeriod, swap.strike, swap.volume)
    case SpotMarket(ndx) =>
      new SingleUnderlyingSwapLikeValuer(vc, SpotMarketIndex(ndx.name), swap.averagingPeriod, swap.strike, swap.volume)
    case IndexSpread(ndx) =>
      new SwapSpreadLikeValuer(vc, ndx, swap.pricingRule, swap.averagingPeriod, swap.strike, swap.volume)
    case _ => sys.error(s"Not a matching index ${swap.market}")
  }

  def apply(vc: ValuationContext, lookalike: CommoditySwapLookalike): SwapLikeValuer = {
    apply(vc, lookalike.asCommoditySwap(vc.refData))
  }
}
