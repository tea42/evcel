package evcel.instrument

import evcel.daterange.{Day, Month}
import evcel.maths.{OptionType, OptionRight}
import evcel.quantity.BDQty

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
}
