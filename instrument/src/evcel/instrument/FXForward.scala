package evcel.instrument

import evcel.daterange.Day
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._
import scala.language.reflectiveCalls

/**
 * @param volume e.g. 1000 USD
 * @param strike e.g. 1.6 GBPUSD
 */
case class FXForward(volume: BDQty, strike: BDQty, delivery: Day)
  extends SingleInstrumentTradeable {
  def instrumentType = FXForward
  def tradeableType = FXForward
}

object FXForward extends InstrumentType with TradeableType{
  def name = Instrument.FX_FORWARD
  def samples = Vector(
    FXForward(100(USD), 12(GBP/USD), 10 / May / 2014)
  )
}



