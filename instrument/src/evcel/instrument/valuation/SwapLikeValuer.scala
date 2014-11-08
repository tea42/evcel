package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.MarketDay._
import evcel.daterange.{DateRange, Day}
import evcel.instrument.{CommoditySwap, CommoditySwapLookalike, CommoditySwapSpread, Future}
import evcel.quantity.Qty._
import evcel.quantity.{UOM, BDQty, Qty}
import evcel.referencedata.calendar.Calendar

trait SwapLikeValuer extends Valuation {
  def calendar: Calendar

  def value: Qty

  def observationDays: Seq[Day]
  def liveObservationDays: Seq[Day]
}

case class SingleUnderlyingSwapLikeValuer(
  vc: ValuationContext, index: Index, averagingPeriod: DateRange,
  initialPrice: BDQty, volume: BDQty, bizDaysToSettlement: Option[Int] = None
  ) extends SwapLikeValuer {
  def isLive = index.liveObservationDays(vc, averagingPeriod).nonEmpty

  def calendar = index.calendar(vc.refData)

  def observationDays: Seq[Day] = index.observationDays(vc, averagingPeriod)
  def liveObservationDays: Seq[Day] = index.liveObservationDays(vc, averagingPeriod)

  def settlementDay: Option[Day] = {
    bizDaysToSettlement.map(n => calendar.addBusinessDays(averagingPeriod.lastDay, n))
  }

  def value = {
    val price = Qty.average(observationDays.map(d => index.price(vc, d)))
    val undiscounted = (price - initialPrice) * amount
    settlementDay.map(d => undiscounted * vc.discountRate(vc.valuationCcy, d).toQty).getOrElse(undiscounted)
  }

  def amount: BDQty = {
    def calcAmount(period: DateRange, perTimeUnit: Option[UOM]): BDQty = {
      perTimeUnit.map {
        case UOM.DAY => Qty(period.size, UOM.DAY) * volume
        case o => sys.error("No handler for " + o)
      }.getOrElse(volume).asInstanceOf[BDQty]
    }

    index match {
      case smi: SpotMarketIndex => calcAmount(averagingPeriod, smi.perTimeUnit(vc.refData))
      case fci@FuturesContractIndex(_, month) => calcAmount(month, fci.perTimeUnit(vc.refData))
      case fdi: FuturesDerivedIndex => calcAmount(averagingPeriod, fdi.perTimeUnit(vc.refData))
      case _ => volume
    }
  }
}

case class SwapSpreadLikeValuer(
  vc: ValuationContext, spread: IndexSpread, rule: SwapPricingRule, period: DateRange,
  initialPrice: BDQty, volume: BDQty, bizDaysToSettlement: Option[Int] = None
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

  def settlementDay: Option[Day] = {
    bizDaysToSettlement.map(n => calendar.addBusinessDays(period.lastDay, n))
  }

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

  def apply(vc: ValuationContext, swap: CommoditySwap): SingleUnderlyingSwapLikeValuer = {
    Index.parse(swap.index)(vc.refData) match {
      case Some(ndx: Index) =>
        new SingleUnderlyingSwapLikeValuer(vc, ndx, swap.averagingPeriod, swap.strike, swap.volume,
          swap.bizDaysToSettlement)
      case _ => sys.error(s"Not a matching index ${swap.index}")
    }
  }

  def apply(vc: ValuationContext, spread: CommoditySwapSpread): SwapSpreadLikeValuer = {
    IndexSpread.parse(spread.spreadName)(vc.refData) match {
      case Some(ndx: IndexSpread) =>
        new SwapSpreadLikeValuer(
          vc, ndx, spread.pricingRule, spread.averagingPeriod, spread.strike, spread.volume, spread.bizDaysToSettlement
        )
      case _ => sys.error(s"Not a value spread index ${spread.spreadName}")
    }
  }

  def apply(vc: ValuationContext, lookalike: CommoditySwapLookalike): SwapLikeValuer = {
    apply(vc, lookalike.asCommoditySwap(vc.refData))
  }
}
