package evcel.instrument.valuation

import evcel.daterange.PeriodLabel
import evcel.quantity.Qty

import scala.collection.immutable

trait HedgeInstrument {
  def market: String
  def period: PeriodLabel
  def volume: Qty
  def copyWithVolume(volume: Qty): HedgeInstrument
}

object HedgeInstrument {
  // Combine HedgeInstruments that have the same market and period into a single HedgeInstrument and
  // scale the volume accordingly.
  def combineSameMarketAndPeriod(hedges: Seq[HedgeInstrument]): Seq[HedgeInstrument] = {
    hedges.groupBy(h => (h.market, h.period)).map{
      case (_, grouped) =>
        // round because we get volumes like 1.99999999999 after svd
        val volume = Qty.sum(grouped.map(_.volume)).round(9)
        grouped.head.copyWithVolume(volume)
    }.toSeq
  }
}

case class FutureHedgeInstrument(market: String, period: PeriodLabel, volume: Qty) extends HedgeInstrument {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}

case class SwapHedgeInstrument(market: String, period: PeriodLabel, volume: Qty) extends HedgeInstrument {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}
