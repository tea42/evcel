package evcel.xl

import evcel.maths.OptionRight
import evcel.maths.models.BlackScholes

class EVPricing {
  def euroOptionFuture(right: String, S: Double, K: Double, vol: Double, T: Double, r: Double): Double = {
    new BlackScholes(OptionRight(right), S, K, vol, T).undiscountedValue * math.exp(-r * T)
  }
}
