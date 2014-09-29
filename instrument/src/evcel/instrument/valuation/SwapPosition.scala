package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.curves.{FuturesPriceIdentifier, SpotPriceIdentifier}
import evcel.curve.environment.{MarketDay, PriceIdentifier}
import evcel.daterange._
import evcel.instrument.{CommoditySwap, Future, Instrument}
import evcel.quantity.Qty
import scalaz.Scalaz._
import evcel.instrument.valuation.Valuer._

class SwapPosition(swap: CommoditySwap)(implicit val valuer: Valuer) {

  def positions(vc: ValuationContext): Iterable[HedgeInstrument] = {
    val swapValuer = SwapLikeValuer(vc, swap)
    val keys = swap.priceKeys(vc).toList.sortWith(_.point < _.point)
    val positions = swapValuer match {
      case sv: SingleUnderlyingSwapLikeValuer =>
        indexPositions(vc, sv.index :: Nil, Map(sv.index-> sv.observationDays), sv.period, keys)
      case sv: SwapSpreadLikeValuer =>
        val obDays = sv.indexes.map(i => i -> sv.observationDays(i)).toMap
        indexPositions(vc, sv.indexes, obDays, sv.period, keys)
    }
    HedgeInstrument.combineSameMarketAndPeriod(positions)
  }

  private def indexPositions(
    vc: ValuationContext, indexes: List[Index], obDays: Map[Index, Iterable[Day]],
    period: DateRange, keys: List[PriceIdentifier]): Seq[HedgeInstrument] = {

    def futuresHedges(index: Index): Iterable[Instrument] = index match {
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

    def swapHedges(index: Index): Iterable[Instrument] = vc.refData.markets.spotMarket(index.indexName).map {
      sm =>
        val priceUOM = sm.priceUOM
        obDays(index).map(d =>
          CommoditySwap(index.indexName, d, Qty("1.0", priceUOM), Qty("1.0", priceUOM.denominator))
        ).toList
    }.orZero

    val unscaledHedges: Seq[Instrument] = indexes.flatMap(i => futuresHedges(i) ++ swapHedges(i))

    val scaled = Position.scaleHedges(vc, swap :: Nil, unscaledHedges)(valuer)
    scaled.map{
      case (s: CommoditySwap, position) =>
        SwapHedgeInstrument(s.market, periodLabel(vc.marketDay, s.averagingPeriod, vc.params.tenor, period), position)
      case (f: Future, position) => FutureHedgeInstrument(f.market, PeriodLabel(f.delivery), position)
      case o => sys.error("Invalid: " + o)
    }
  }

  // helper method to turn Swap observation days into the required tenor
  private def periodLabel(md: MarketDay, day: DateRange, tenor: Option[TenorType], period: DateRange): PeriodLabel = {
    require(day.firstDay == day.lastDay, "This should be a day: " + day)
    val d = day.firstDay
    tenor match {
      case None => PeriodLabel(period)
      case Some(Day) => PeriodLabel(d)
      case Some(Month) =>
        val month = d.containingMonth.remainder(md.day).getOrElse(
          sys.error("Shouldn't be trying to get month for expired day")
        )
        PeriodLabel(month)
      case o => sys.error("Invalid: " + o)
    }
  }
}

