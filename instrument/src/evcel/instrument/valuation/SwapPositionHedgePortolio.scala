package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.curves.FuturesPriceIdentifier
import evcel.curve.environment.PriceIdentifier
import evcel.daterange._
import evcel.instrument.valuation.Valuer._
import evcel.instrument.{CommoditySwap, CommoditySwapSpread, Future, Instrument}
import evcel.quantity.{BDQty, Qty}

import scalaz.Scalaz._

class SwapPositionHedgePortolio(vc: ValuationContext, indexes: List[Index], obDays: Map[Index, Iterable[Day]],
  period: DateRange, keys: List[PriceIdentifier]) extends HedgePortfolio {

  override def unscaledHedges = {

    def hedgesForFuturesIndexes(index: Index): Iterable[Instrument] = index match {
      case fdi: FuturesDerivedIndex =>
        val market = fdi.underlyingMarketName
        val priceKeys = keys.collect { case pi@FuturesPriceIdentifier(`market`, _) => pi}
        if (priceKeys.isEmpty) {
          Nil
        } else if (vc.params.showEqFutures) {
          priceKeys.map(key => {
            val priceUOM = vc.refData.markets.futuresMarketOrThrow(key.market).priceUOM
            Future(key.market, key.month, Qty("1.0", priceUOM), Qty("1.0", priceUOM.denominator))
          })
        } else {
          val priceUOM = vc.refData.markets.futuresMarketOrThrow(market).priceUOM
          obDays(index).map(d =>
            CommoditySwap(index.indexName, d, Qty("1.0", priceUOM), Qty("1.0", priceUOM.denominator))
          )
        }
      case _ => Nil
    }

    def hedgesForSwapIndexes(index: Index): Iterable[Instrument] = vc.refData.markets.spotMarket(index.indexName).map {
      sm =>
        val priceUOM = sm.priceUOM
        obDays(index).map(d =>
          CommoditySwap(index.indexName, d, Qty("1.0", priceUOM), Qty("1.0", priceUOM.denominator))
        ).toList
    }.orZero

    indexes.flatMap(i => hedgesForFuturesIndexes(i) ++ hedgesForSwapIndexes(i))
  }

  override def instrumentToHedgeInfo(vc: ValuationContext, unscaledHedge: Instrument, volume: BDQty) = {
    unscaledHedge match {
      case swapHedge: CommoditySwap =>
        require(swapHedge.averagingPeriod.days.size == 1, "This should be one day: " + swapHedge.averagingPeriod)
        val d = swapHedge.averagingPeriod.firstDay
        val periodLabel = vc.params.tenor match {
          case None => PeriodLabel(period)
          case Some(Day) => PeriodLabel(d)
          case Some(Month) =>
            val month = d.containingMonth.remainder(vc.marketDay.day).getOrElse(
              sys.error("Shouldn't be trying to get month for expired day")
            )
            PeriodLabel(month)
          case o => sys.error("Invalid: " + o)
        }
        SwapHedgeInfo(swapHedge.market, periodLabel, volume)
      case f: Future => FutureHedgeInfo(f.market, PeriodLabel(f.delivery), volume)
      case o => sys.error("Not valid: " + o)
    }
  }
}

object SwapPositionHedgePortolio {
  def apply(vc: ValuationContext, swap: CommoditySwap)(implicit valuer: Valuer): SwapPositionHedgePortolio = {
    val keys = valuer.priceKeys(vc, swap).toList.sortWith(_.point < _.point)
    val swapValuer = SwapLikeValuer(vc, swap)
    new SwapPositionHedgePortolio(
      vc, swapValuer.index :: Nil, Map(swapValuer.index -> swapValuer.observationDays), swapValuer.averagingPeriod, keys
    )
  }

  def apply(vc: ValuationContext, swap: CommoditySwapSpread)(implicit valuer: Valuer): SwapPositionHedgePortolio = {
    val keys = valuer.priceKeys(vc, swap).toList.sortWith(_.point < _.point)
    val swapValuer = SwapLikeValuer(vc, swap)
    val obDays = swapValuer.indexes.map(i => i -> swapValuer.observationDays(i)).toMap
    new SwapPositionHedgePortolio(vc, swapValuer.indexes, obDays, swapValuer.period, keys)
  }

}
