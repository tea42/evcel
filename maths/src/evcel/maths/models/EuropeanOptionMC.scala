package evcel.maths.models

import evcel.maths.OptionRight

object EuropeanOptionMC {
  def valuation(right: OptionRight, K: Double) = {
    new MCValuationSimple {
      override def payoff(St: Double): Double = right.payoff(St, K)
    }
  }
}
