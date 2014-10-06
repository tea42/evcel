package evcel.instrument.valuation

import evcel.daterange.PeriodLabel
import evcel.quantity.Qty

import scala.collection.immutable

trait HedgeInfo {
  def market: String
  def period: PeriodLabel
  def volume: Qty
  def copyWithVolume(volume: Qty): HedgeInfo
}

object HedgeInfo {
  // Combine HedgeInstruments that have the same market and period into a single HedgeInstrument and
  // scale the volume accordingly.
  def combineSameMarketAndPeriod(hedges: Seq[HedgeInfo]): Seq[HedgeInfo] = {
    hedges.groupBy(h => (h.market, h.period)).map{
      case (_, grouped) =>
        // round because we get volumes like 1.99999999999 after svd
        val volume = Qty.sum(grouped.map(_.volume)).round(9)
        grouped.head.copyWithVolume(volume)
    }.toSeq
  }
}

case class FutureHedgeInfo(market: String, period: PeriodLabel, volume: Qty) extends HedgeInfo {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}

case class SwapHedgeInfo(market: String, period: PeriodLabel, volume: Qty) extends HedgeInfo {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}
