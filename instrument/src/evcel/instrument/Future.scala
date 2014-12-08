package evcel.instrument

import evcel.daterange.Month
import evcel.quantity.BDQty

case class Future(market: String, period: Month, strike: BDQty, volume: BDQty)
  extends SingleInstrumentTradeable with HedgeInstrument 
{
  
  def tradeableType = Future

  def instrumentType = Future
}

object Future extends InstrumentType with TradeableType{
  val name = "Future"
}

