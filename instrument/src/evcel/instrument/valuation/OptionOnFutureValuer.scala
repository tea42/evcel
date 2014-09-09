package evcel.instrument.valuation

import evcel.instrument.{ EuropeanOption, FuturesOption }
import evcel.curve.Environment
import evcel.quantity.{Qty, UOM}

case class OptionOnFutureValuer(v: FuturesOption) {
  def value(env: Environment, ccy: UOM) = {
    require(v.optionType == EuropeanOption, "Only EuropeanOption supported")

//    val F = env.futuresPrice(v.market, v.delivery)
//    val T = v.
//    val value = new BlackScholes(v.right, F, v.strike, vol, T, r)
    val value = 1.0

    Qty(value, v.strike.uom) * v.volume
  }
}
