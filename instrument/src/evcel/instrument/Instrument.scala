package evcel.instrument

import evcel.quantity.BDQty
import evcel.quantity.Qty

trait Instrument extends Product{
  def instrumentType : InstrumentType
} 

object Instrument{
  val FUTURE = "Future"
  val FUTURES_OPTION = "Futures Option"
  val COMMODITY_SWAP = "Commodity Swap"
  val COMMODITY_SWAP_LOOKALIKE = "Commodity Swap Lookalike"
  val COMMODITY_SWAP_SPREAD = "Commodity Swap Spread"
}

/**
  * In general, instruments don't have a single volume. E.g. market spreads between MT/BBl volumes of 
  * different commodities. However the standard instruments used to hedge portfolios will need 
  * a volume of some sort that is used to describe the position.
  */
trait HedgeInstrument extends Instrument{
  def volume: Qty
}

trait InstrumentType{
  def name : String
}
