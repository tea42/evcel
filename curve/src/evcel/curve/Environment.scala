package evcel.curve

import evcel.curve.curves.FuturesPriceIdentifier
import evcel.daterange.Month
import evcel.quantity.Qty
import evcel.curve.environment.AtomicEnvironment
import evcel.curve.curves.DiscountRateIdentifier
import evcel.quantity.UOM
import evcel.daterange.Day
import evcel.curve.curves.FuturesVolIdentifier
import evcel.quantity.Percentage
import evcel.curve.environment.PerturbedAtomicEnvironment

case class Environment(atomic: AtomicEnvironment) {
  def futuresPrice(market: String, month: Month) = atomic(FuturesPriceIdentifier(market, month)).asInstanceOf[Qty]
  def discountRate(currency: UOM, day: Day) = atomic(DiscountRateIdentifier(currency, day)).asInstanceOf[Double]
  def futuresVol(market: String, month: Month, strike: Qty) = {
    atomic(FuturesVolIdentifier(market, month, strike, futuresPrice(market, month))).asInstanceOf[Qty]
  }

  def shiftFuturesPrice(market: String, month: Month, dP: Qty) = Environment {
    PerturbedAtomicEnvironment(
      atomic, 
      { case FuturesPriceIdentifier(`market`, `month`) => futuresPrice(market, month) + dP })
  }
}
