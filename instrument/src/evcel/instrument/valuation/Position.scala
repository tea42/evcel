package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{MarketDay, PriceIdentifier}
import evcel.daterange._
import evcel.instrument.valuation.Valuer._
import evcel.instrument._
import evcel.quantity.{BDQty, Qty}
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, SingularValueDecomposition}

trait HedgePortfolio {
  def unscaledHedges: Seq[Instrument]

  def instrumentToHedgeInfo(vc: ValuationContext, unscaledHedge: Instrument, volume: BDQty): HedgeInfo
}

class FutureHedgePortfolio(f: Future) extends HedgePortfolio {
  override def unscaledHedges = f.copy(volume = f.volume.one) :: Nil

  override def instrumentToHedgeInfo(vc: ValuationContext, unscaledHedge: Instrument, volume: BDQty) = {
    unscaledHedge match {
      case f: Future => FutureHedgeInfo(f.market, PeriodLabel(f.delivery), volume)
      case o => sys.error("Not valid: " + o)
    }
  }
}

class Position(implicit val valuer: Valuer) {

  def positions(vc: ValuationContext, instr: Instrument): Iterable[HedgeInfo] = {
    val hedgePortfolio = instr match {
      case s: CommoditySwapLookalike =>
        SwapPositionHedgePortolio(vc, s.asCommoditySwap(vc.refData))
      case s: CommoditySwapSpread =>
        SwapPositionHedgePortolio(vc, s)
      case s: CommoditySwap =>
        SwapPositionHedgePortolio(vc, s)
      case f: Future =>
        new FutureHedgePortfolio(f)
    }
    val scaled = Position.scaleHedges(vc, instr :: Nil, hedgePortfolio.unscaledHedges)(valuer)
    val hedgeInfos = scaled.map{
      case (unscaled: Instrument, volume: BDQty) => hedgePortfolio.instrumentToHedgeInfo(vc, unscaled, volume)
    }
    HedgeInfo.combineSameMarketAndPeriod(hedgeInfos)
  }

}

object Position {
  implicit class RichPositionInstrument(instr: Instrument) {
    def positions(vc: ValuationContext)(implicit position: Position) = position.positions(vc, instr)
  }

  def scaleHedges(
    vc: ValuationContext,
    portfolio: Seq[Instrument],
    hedges: Seq[Instrument])
    (implicit valuer: Valuer): Seq[(Instrument, BDQty)] = {

    val portfolioKeys = portfolio.map(i => i -> i.priceKeys(vc)).toMap
    val hedgeKeys = hedges.map(i => i -> i.priceKeys(vc)).toMap
    val instrumentKeys = portfolioKeys ++ hedgeKeys

    val allKeys = instrumentKeys.values.flatten.toSeq.distinct.sortWith{
      case (p1:PriceIdentifier, p2: PriceIdentifier) => p1.point < p2.point
    }
    val jacobian = new Array2DRowRealMatrix(allKeys.size, hedges.size)
    val hedgePos = new Array2DRowRealMatrix(allKeys.size, 1)

    def fastDeriv(i: Instrument, key: PriceIdentifier) = {
      val keys = instrumentKeys(i)
      if(keys.contains(key)) {
        val linearAndNotDiscounted = i match {
          case s: CommoditySwap => s.isCleared
          case f: Future => true
          case _ => false
        }
        if(linearAndNotDiscounted && keys == Set(key))
          i.volume.doubleValue
        else
          i.firstOrderPriceDiff(vc, key).doubleValue
      } else {
        0.0
      }
    }

    allKeys.zipWithIndex.foreach{
      case (key, i) =>
        hedgePos.setEntry(i, 0, portfolio.map(i => fastDeriv(i, key)).sum)
        hedges.zipWithIndex.foreach{
          case (hedge, j) =>
            val position = fastDeriv(hedge, key)
            jacobian.setEntry(i, j, position)
        }
    }
    val svd = new SingularValueDecomposition(jacobian)
    val solution = svd.getSolver.solve(hedgePos)
    hedges.zipWithIndex.map {
      case (hedge, i) =>
        (hedge, Qty(solution.getEntry(i, 0).toString, hedge.volume.uom))
    }
  }
}
