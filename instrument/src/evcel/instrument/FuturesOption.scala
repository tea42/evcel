package evcel.instrument

import evcel.daterange.{Day, Month}
import evcel.maths.{OptionType, OptionRight}
import evcel.quantity.BDQty

case class FuturesOption(market: String, delivery: Month, strike: BDQty, volume: BDQty, right: OptionRight,
  optionType: OptionType,
  isCashSettled: Boolean = false, bizDaysAfterExpiryToSettlement: Int = 0, customExpiry: Option[Day] = None)
  extends Instrument {
}

object FuturesOption extends InstrumentType
