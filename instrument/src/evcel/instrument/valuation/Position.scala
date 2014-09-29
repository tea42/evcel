package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.curves.FuturesPriceIdentifier
import evcel.curve.environment.PriceIdentifier
import evcel.daterange.PeriodLabel
import evcel.instrument.valuation.Valuer._
import evcel.instrument.{CommoditySwap, CommoditySwapLookalike, Future, Instrument}
import evcel.quantity.{BDQty, Qty}
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, SingularValueDecomposition}

class Position(implicit val valuer: Valuer) {

  def positions(vc: ValuationContext, instr: Instrument): Iterable[HedgeInstrument] = instr match {
    case s: CommoditySwapLookalike =>
      new SwapPosition(s.asCommoditySwap(vc.refData)).positions(vc)
    case s: CommoditySwap =>
      new SwapPosition(s).positions(vc)
    case f: Future =>
      val priceKeys = f.keys(vc).flatMap { case pi: FuturesPriceIdentifier => Some(pi); case _ => None}
      require(priceKeys.size == 1, s"Only expected 1 key: $priceKeys")
      val key = priceKeys.head
      val delta = f.firstOrderPriceDiff(vc, key)
      FutureHedgeInstrument(key.market, PeriodLabel(key.month), delta) :: Nil
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

    val portfolioKeys = portfolio.flatMap(_.priceKeys(vc))
    val hedgeKeys = hedges.flatMap(_.priceKeys(vc))
    val allKeys = (portfolioKeys ++ hedgeKeys).toList.distinct.sortWith{
      case (p1:PriceIdentifier, p2: PriceIdentifier) => p1.point < p2.point
    }
    val jacobian = new Array2DRowRealMatrix(allKeys.size, hedges.size)
    val hedgePos = new Array2DRowRealMatrix(allKeys.size, 1)
    allKeys.zipWithIndex.foreach{
      case (key, i) =>
        hedgePos.setEntry(i, 0, Qty.sum(portfolio.map(_.firstOrderPriceDiff(vc, key))).doubleValue)
        hedges.zipWithIndex.foreach{
          case (hedge, j) =>
            val position = hedge.firstOrderPriceDiff(vc, key)
            jacobian.setEntry(i, j, position.doubleValue)
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
