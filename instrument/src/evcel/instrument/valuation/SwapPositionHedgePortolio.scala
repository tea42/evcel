package evcel.instrument.valuation

import evcel.curve.{EnvironmentParams, ValuationContext}
import evcel.curve.curves.FuturesPriceIdentifier
import evcel.curve.environment.PriceIdentifier
import evcel.daterange._
import evcel.instrument.valuation.Valuer._
import evcel.instrument.{CommoditySwap, CommoditySwapSpread, Future, Instrument}
import evcel.quantity.{UOM, DblQty, BDQty, Qty}

import scalaz.Scalaz._
import evcel.instrument.HedgeInstrument

class SwapPositionHedgePortolio(vc: ValuationContext, indexes: List[Index], obDays: Map[Index, Iterable[Day]],
  period: DateRange, keys: List[PriceIdentifier]) extends HedgePortfolio {

  override def unscaledHedges = {

    def hedgesForFuturesIndexes(index: Index): Iterable[HedgeInstrument] = index match {
      case fdi: FuturesDerivedIndex =>
        val market = fdi.underlyingMarketName
        val priceUOM = vc.refData.markets.futuresMarketOrThrow(market).priceUOM
        val priceKeys = keys.collect { case pi@FuturesPriceIdentifier(`market`, _) => pi}
        val unitVolume = fdi.perTimeUnit(vc.refData).map{
          time => Qty("1.0", priceUOM.denominator / time)
        }.getOrElse(Qty("1.0", priceUOM.denominator))

        if (priceKeys.isEmpty) {
          Nil
        } else if (vc.params.showEqFutures) {
          priceKeys.map(key => {
            Future(key.market, key.month, Qty("1.0", priceUOM), unitVolume)
          })
        } else {
          obDays(index).map(d =>
            CommoditySwap(index.indexName, d, Qty("1.0", priceUOM), unitVolume)
          )
        }
      case _ => Nil
    }

    def hedgesForSwapIndexes(index: Index): Iterable[HedgeInstrument] = {
      vc.refData.markets.spotMarket(index.indexName).map {
        sm =>
          val priceUOM = sm.priceUOM
          obDays(index).map(d =>
            CommoditySwap(index.indexName, d, Qty("1.0", priceUOM), Qty("1.0", priceUOM.denominator))
          ).toList
      }.orZero
    }

    indexes.flatMap(i => hedgesForFuturesIndexes(i) ++ hedgesForSwapIndexes(i))
  }

  override def combineToHedgeInfo(vc: ValuationContext, hedgesWithScale: Seq[(Instrument, DblQty)]) = {
    val combinedPeriods = hedgesWithScale.map{
      case (swapHedge: CommoditySwap, volume) =>
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
        HedgeInfo(swapHedge.index, periodLabel, volume)
      case (f: Future, volume) => HedgeInfo(f.market, PeriodLabel(f.period), volume)
      case o => sys.error("Not valid: " + o)
    }
    combinedPeriods.groupBy { hi => (hi.riskMarket, hi.riskPeriod)}.map {
      case ((_, DateRangePeriodLabel(dr)), grouped) =>
        val isPerTimeUnit = grouped.map(_.volume.uom).head.isPerTimeUnit
        val volume = if (isPerTimeUnit) {
          if (vc.params.positionAsPower) {
            Qty.average(grouped.map(_.volume))
          } else {
            require(grouped.map(_.volume.uom).head.denominator == UOM.DAY,
              "Only handle per day at the moment " + grouped.map(_.volume.uom).head.denominator)
            Qty.average(grouped.map(_.volume)) * Qty(dr.size, UOM.DAY)
          }
        } else {
          val summed = Qty.sum(grouped.map(_.volume))
          summed
        }

        // round because we get volumes like 1.99999999999 after svd
        grouped.head.copyWithVolume(volume.round(9))
    }(scala.collection.breakOut)
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
