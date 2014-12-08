package evcel.instrument.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{MarketDay, PriceIdentifier}
import evcel.daterange._
import evcel.instrument.valuation.Valuer._
import evcel.instrument._
import evcel.quantity.{UOM, DblQty, BDQty, Qty}
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, SingularValueDecomposition}

trait HedgePortfolio {
  def unscaledHedges: Seq[HedgeInstrument]

  def combineToHedgeInfo(vc: ValuationContext, hedgesWithScale: Seq[(Instrument, DblQty)]): Seq[HedgeInfo]
}

class FutureHedgePortfolio(f: Future) extends HedgePortfolio {
  override def unscaledHedges = f.copy(volume = f.volume.one) :: Nil

  override def combineToHedgeInfo(vc: ValuationContext, hedgesWithScale: Seq[(Instrument, DblQty)]) = {
    hedgesWithScale.map {
      case (f: Future, _volume) =>
        val volumeUOM = _volume.uom
        val volume = if(volumeUOM.isPerTimeUnit && !vc.params.positionAsPower) {
          require(volumeUOM.denominator == UOM.DAY, "Only handle per day at the moment " + volumeUOM)
          _volume * Qty(f.period.size, UOM.DAY)
        } else {
          _volume
        }.round(9)
        HedgeInfo(f.market, PeriodLabel(f.period), volume)
      case o => sys.error("Not valid: " + o)
    }(scala.collection.breakOut)
  }
}
