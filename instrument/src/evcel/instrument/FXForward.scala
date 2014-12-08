package evcel.instrument

import evcel.daterange.Day
import evcel.quantity.BDQty

/**
 * @param volume e.g. 1000 USD
 * @param strike e.g. 1.6 GBPUSD
 */
case class FXForward(volume: BDQty, strike: BDQty, delivery: Day)
  extends Instrument {
  def instrumentType = FXForward
}

object FXForward extends InstrumentType{
  def name = "FXForward"
}



