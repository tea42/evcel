package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty

case class CommoditySwap(market: String, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  bizDaysToSettlement: Option[Int] = None)
  extends Instrument {

  def isCleared = bizDaysToSettlement.isEmpty
}

object CommoditySwap {
  val defaultDaysToSettlement = 5
}



