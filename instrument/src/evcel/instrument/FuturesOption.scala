package evcel.instrument

import evcel.daterange.{Day, Month}
import evcel.maths.OptionRight
import evcel.quantity.BDQty

case class FuturesOption(market: String, delivery: Month, strike: BDQty, volume: BDQty, right: OptionRight,
  optionType: OptionType, isCashSettled: Boolean, bizDaysAfterExpiryToSettlement: Int,
  customExpiry: Option[Day]) extends Instrument {
}

object FuturesOption extends InstrumentType
