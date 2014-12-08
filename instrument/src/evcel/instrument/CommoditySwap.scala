package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._

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
    
  def samples = Vector(
    CommoditySwap(
      "Nymex WTI nearby 1",
      Jun / 2014,
      100(USD/BBL),
      123(BBL)
    ),
    CommoditySwap(
      "Nymex WTI nearby 1",
      Jun / 2014,
      100(USD/BBL),
      123(BBL),
      bizDaysToSettlement = Some(10)
    )
  )
}



