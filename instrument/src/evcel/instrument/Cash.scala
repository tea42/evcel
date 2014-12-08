package evcel.instrument

import evcel.daterange.Day
import evcel.quantity.BDQty

/**
 * @param volume e.g. 1000 USD
 * @param delivery optional delivery for discounting
 */
case class Cash(volume: BDQty, delivery: Option[Day])
  extends Instrument {
  def instrumentType = Cash
}

object Cash extends InstrumentType{
  def name = "Cash"
}



