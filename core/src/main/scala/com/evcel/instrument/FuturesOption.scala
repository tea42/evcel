package com.evcel.instrument

import com.evcel.core.qty.BDQty
import com.evcel.date.Month
import com.evcel.models.OptionRight
import com.evcel.valuation.OptionOnFutureValuer

case class FuturesOption(market: String, delivery: Month, strike: BDQty, volume: BDQty, right: OptionRight,
    optionType: OptionType, isCashSettled: Boolean, bizDaysAfterExpiryToSettlement: Int) extends Instrument {

  override def valuer = new OptionOnFutureValuer(this)
}
