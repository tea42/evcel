package evcel.curve.markets

import evcel.curve.ReferenceData
import evcel.daterange.{Month, Day}
import evcel.quantity.UOM

case class FuturesMarket(name: String, calendarName: String, priceUOM: UOM) {
  def frontMonth(refData: ReferenceData, day: Day): Month = {
    val expiryRules = refData.futuresExpiryRules.expiryRule(name).getOrElse(sys.error(s"No expiry rule for $name"))
    val month = Iterator.iterate(day.containingMonth)(m => m.next).find(
      m => expiryRules.futureExpiryDayOrThrow(m) >= day
    )
    month.getOrElse(sys.error(s"Failed to find observedMonth for $name and $day"))
  }
}

object FuturesMarket {
  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => refData.markets.futuresMarket(name)
    case _ => None
  }
}
