package evcel.instrument.valuation

import evcel.daterange.PeriodLabel
import evcel.quantity.Qty

object HedgeInfo {
  // Combine HedgeInstruments that have the same market and period into a single HedgeInstrument and
  // scale the volume accordingly.
  def combineSameMarketAndPeriod(hedges: Seq[HedgeInfo]): Seq[HedgeInfo] = {
    hedges.groupBy(h => (h.riskMarket, h.riskPeriod)).map{
      case (_, grouped) =>
        // round because we get volumes like 1.99999999999 after svd
        val volume = Qty.sum(grouped.map(_.volume)).round(9)
        grouped.head.copyWithVolume(volume)
    }(scala.collection.breakOut)
  }
}

case class HedgeInfo(riskMarket: String, riskPeriod: PeriodLabel, volume: Qty) {
  import Valuer._
  def copyWithVolume(volume: Qty) = copy(volume = volume)
}

