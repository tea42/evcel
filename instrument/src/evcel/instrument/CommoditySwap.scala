package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty

case class CommoditySwap(index: String, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  bizDaysToSettlement: Option[Int] = None)
  extends SingleInstrumentTradeable with HedgeInstrument {
  import CommoditySwap._

  def tradeableType = CommoditySwap


  def isCleared = bizDaysToSettlement.isEmpty
  def instrumentType = CommoditySwap
}

object CommoditySwap extends TradeableType with InstrumentType{
  val defaultDaysToSettlement = 5
  def name = "Commodity Swap"
    
}



