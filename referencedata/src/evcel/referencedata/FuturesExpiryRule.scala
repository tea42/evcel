package evcel.referencedata

import evcel.daterange.Day
import evcel.daterange.Month

case class FuturesExpiryRule(
  market: String, 
  futureExpiries: Map[Month, Day], 
  optionExpiries: Map[Month, Day]
) 
  extends ReferenceDataTrait
{
  def futureExpiryDay(month: Month): Option[Day] = futureExpiries.get(month)
  def futureExpiryDayOrThrow(month: Month): Day =
    futureExpiryDay(month).getOrElse(sys.error(s"No known option expiry for $market/$month"))
  def optionExpiryDay(month: Month): Option[Day] =
    optionExpiries.get(month)
  def optionExpiryDayOrThrow(month: Month): Day =
    optionExpiryDay(month).getOrElse(sys.error(s"No known option expiry for $market/$month"))
}

case class FuturesExpiryRuleIdentifier(market : String) extends ReferenceDataIdentifier

class FuturesExpiryRules(rules: Map[String, FuturesExpiryRule]) {
  def expiryRule(market: String) = rules.get(market)
}