package evcel.referencedata

import evcel.daterange.{Day, Month}
import scala.util.Either
import evcel.utils.{EvcelFail, GeneralEvcelFail}

case class FuturesExpiryRule(
  market: String, 
  futureExpiries: Map[Month, Day], 
  optionExpiries: Map[Month, Day]
) 
  extends ReferenceDataTrait
{
  def futureExpiryDay(month: Month): Either[EvcelFail, Day] = 
    futureExpiries.get(month).toRight(GeneralEvcelFail(s"Expiry not known for $market, $month"))
  def optionExpiryDay(month: Month): Either[EvcelFail, Day] =
    optionExpiries.get(month).toRight(GeneralEvcelFail(s"Option expiry not known for $market, $month"))
}

case class FuturesExpiryRuleIdentifier(market : String) extends ReferenceDataIdentifier

class FuturesExpiryRules(rules: Map[String, FuturesExpiryRule]) {
  def expiryRule(market: String) = rules.get(market).toRight(GeneralEvcelFail(s"No expiry rule for $market"))
}
