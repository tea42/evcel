package evcel.instrument

import evcel.daterange.DateRange
import evcel.instrument.valuation.HedgeInstrument
import evcel.quantity.BDQty

case class CommoditySwap(index: String, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  bizDaysToSettlement: Option[Int] = None)
  extends Instrument with HedgeInstrument {

  def isCleared = bizDaysToSettlement.isEmpty
}

object CommoditySwap {
  val defaultDaysToSettlement = 5
}



