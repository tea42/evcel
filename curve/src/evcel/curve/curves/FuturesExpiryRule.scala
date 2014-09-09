package evcel.curve.curves

import evcel.daterange.Day
import evcel.daterange.Month

/**
 * Part of versioned reference data
 */
case class FuturesExpiryRule(market: String, optionExpiries: Map[Month, Day]) {
  def optionExpiryDay(month: Month): Day = 
    optionExpiries.getOrElse(month, throw new RuntimeException("No known option expiry for $market/$month"))
}

class FuturesExpiryRules(rules: Map[String, FuturesExpiryRule]) {
  def expiryRule(market: String) = rules.get(market)
}

