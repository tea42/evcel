package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{MarketDay, PriceIdentifier}
import evcel.instrument.valuation.Valuer._
import evcel.instrument._
import evcel.quantity.{DblQty, Qty}
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, SingularValueDecomposition}

object SVDPositions{
  private[valuation] def scaleHedges(
    vc: ValuationContext,
    portfolio: Seq[Instrument],
    hedges: Seq[HedgeInstrument])
    (implicit valuer: Valuer): Seq[(Instrument, DblQty)] = {

    val portfolioKeys = portfolio.map(i => i -> i.priceKeys(vc)).toMap
    val hedgeKeys = hedges.map(i => i -> i.priceKeys(vc)).toMap
    val instrumentKeys = portfolioKeys ++ hedgeKeys

    val allKeys = instrumentKeys.values.flatten.toSeq.distinct.sortWith{
      case (p1:PriceIdentifier, p2: PriceIdentifier) => p1.point.firstDay < p2.point.firstDay
    }
    val jacobian = new Array2DRowRealMatrix(allKeys.size, hedges.size)
    val hedgePos = new Array2DRowRealMatrix(allKeys.size, 1)

    allKeys.zipWithIndex.foreach{
      case (key, i) =>
        val portfolioDelta = portfolio.map(i => fastDeriv(vc, i, instrumentKeys(i), key)).sum
        hedgePos.setEntry(i, 0, portfolioDelta)
        hedges.zipWithIndex.foreach{
          case (hedge, j) =>
            val position = fastDeriv(vc, hedge, instrumentKeys(hedge), key)
            jacobian.setEntry(i, j, position)
        }
    }
    val svd = new SingularValueDecomposition(jacobian)
    val solution = svd.getSolver.solve(hedgePos)
    hedges.zipWithIndex.map {
      case (hedge, i) =>
        (hedge, Qty(solution.getEntry(i, 0), hedge.volume.uom))
    }(scala.collection.breakOut)
  }
  def positions(vc: ValuationContext, instr: Instrument)(implicit valuer : Valuer): Seq[HedgeInfo] = {
    val hedgePortfolio = instr match {
      case s: CommoditySwapLookalike =>
        SwapPositionHedgePortolio(vc, s.asCommoditySwap(vc.refData))(valuer)
      case s: CommoditySwapSpread =>
        SwapPositionHedgePortolio(vc, s)(valuer)
      case s: CommoditySwap =>
        SwapPositionHedgePortolio(vc, s)(valuer)
      case f: Future =>
        new FutureHedgePortfolio(f)
    }
    val hedgesWithScale = scaleHedges(vc, instr :: Nil, hedgePortfolio.unscaledHedges)(valuer)
    val hedgeInfos = hedgePortfolio.combineToHedgeInfo(vc, hedgesWithScale)
    hedgeInfos
  }

  private def fastDeriv(vc: ValuationContext, i: Instrument,
                        instrumentKeys: Set[PriceIdentifier], key: PriceIdentifier)(implicit valuer: Valuer) = {
    if (instrumentKeys.contains(key)) {
      val delta = if (instrumentKeys == Set(key)) {
        i match {
          case s: CommoditySwap if s.isCleared && !s.volume.uom.isPerTimeUnit => Some(s.volume.doubleValue)
          case f: Future if !f.volume.uom.isPerTimeUnit => Some(f.volume.doubleValue)
          case _ => None
        }
      } else None
      delta.getOrElse(i.firstOrderPriceDiff(vc, key).doubleValue)
    } else {
      0.0
    }
  }

}
