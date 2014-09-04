package com.evcel.xl

import com.evcel.models.{ OptionRight, BlackScholes }

class EVPricing {
  def euroOptionFuture(right: String, S: Double, K: Double, vol: Double, T: Double, r: Double): Double = {
    new BlackScholes(OptionRight(right), S, K, vol, T, r).discountedValue
  }
}
