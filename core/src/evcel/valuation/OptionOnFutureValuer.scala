package com.evcel.valuation

import com.evcel.Environment.Environment
import com.evcel.core.qty.{ Qty, UOM }
import com.evcel.instrument.{ EuropeanOption, FuturesOption }

case class OptionOnFutureValuer(v: FuturesOption) extends Valuer {
  override def value(env: Environment, ccy: UOM) = {
    require(v.optionType == EuropeanOption, "Only EuropeanOption supported")

    //      val value = new BlackScholes(v.right, F, v.strike, vol, T, r)
    val value = 1.0

    Qty(value, v.strike.uom) * v.volume
  }
}
