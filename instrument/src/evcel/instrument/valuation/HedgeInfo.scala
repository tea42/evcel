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

case class FutureHedgeInfo(market: String, period: PeriodLabel, volume: Qty) extends HedgeInfo {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}

case class SwapHedgeInfo(market: String, period: PeriodLabel, volume: Qty) extends HedgeInfo {
  override def copyWithVolume(volume: Qty) = copy(volume = volume)
}
