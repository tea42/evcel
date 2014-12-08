package evcel.instrument

import evcel.daterange.Day
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._
import scala.language.reflectiveCalls

/**
 * @param volume e.g. 1000 USD
 * @param delivery optional delivery for discounting
 */
case class Cash(volume: BDQty, delivery: Option[Day])
  extends SingleInstrumentTradeable {
  def instrumentType = Cash
  def tradeableType = Cash
}

object Cash extends InstrumentType with TradeableType{
  def name = Instrument.CASH
  def samples = Vector(
    Cash(100(USD), Some(10 / Jun / 2014)),
    Cash(100(USD), None)
  )
}



