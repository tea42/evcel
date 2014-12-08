package evcel.instrument

import evcel.daterange.{Day, Month}
import evcel.maths.{OptionType, OptionRight}
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._
import evcel.maths.Call
import evcel.maths.EuropeanOption

case class FuturesOption(
  market: String, period: Month, 
  strike: BDQty, volume: BDQty, right: OptionRight,
  optionType: OptionType,
  isCashSettled: Boolean = false, 
  bizDaysAfterExpiryToSettlement: Int = 0, 
  customExpiry: Option[Day] = None
)
  extends SingleInstrumentTradeable 
{
  def tradeableType = FuturesOption

  def instrumentType = FuturesOption
}

object FuturesOption extends TradeableType with InstrumentType{
  val name = "Futures Option"
  def samples = Vector(
    FuturesOption(
      "WTI",
      Jun / 2014,
      100(USD/BBL),
      50(BBL),
      Call,
      EuropeanOption
    )
  )

}
