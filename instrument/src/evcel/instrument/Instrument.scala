package evcel.instrument

import evcel.quantity.{BDQty, Qty}
import evcel.daterange.PeriodLabel

trait Instrument extends Product{
  def instrumentType : InstrumentType
} 

object Instrument{
  val FUTURE = "Future"
  val FUTURES_OPTION = "Futures Option"
  val COMMODITY_SWAP = "Commodity Swap"
  val COMMODITY_SWAP_LOOKALIKE = "Commodity Swap Lookalike"
  val COMMODITY_SWAP_SPREAD = "Commodity Swap Spread"
  val FX_FORWARD = "FX Forward"
  val CASH = "Cash"
}

trait InstrumentType {
  def name : String
  def samples : Seq[Instrument] 
  override def toString = name
}

object InstrumentType{
  val types = Vector[InstrumentType](
    Future, FuturesOption, 
    CommoditySwap, CommoditySwapSpread, CommoditySwapLookalike,
    FXForward,
    Cash
  )
}
