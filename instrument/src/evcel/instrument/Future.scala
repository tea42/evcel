package evcel.instrument

import evcel.daterange.Month
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._

case class Future(market: String, period: Month, strike: BDQty, volume: BDQty)
  extends SingleInstrumentTradeable with HedgeInstrument 
{
  
  def tradeableType = Future

  def instrumentType = Future
}

object Future extends InstrumentType with TradeableType{
  val name = "Future"
  def samples = Vector(
    Future("WTI", Jun / 2014, 100(USD/BBL), 123(BBL))
  )
}

