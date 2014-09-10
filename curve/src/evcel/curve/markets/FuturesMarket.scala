package evcel.curve.markets

import evcel.curve.ReferenceData
import evcel.daterange.{Month, Day}
import evcel.quantity.UOM

case class FuturesMarket(name: String, calendarName: String, priceUOM: UOM) {
  def observedMonth(refData: ReferenceData, day: Day): Month = {
    val expiryRules = refData.futuresExpiryRules.expiryRule(name).getOrElse(sys.error("No expiry rule for $market"))
    val month = Iterator.iterate(day.containingMonth)(m => m.next).find(
      m => expiryRules.futureExpiryDayOrThrow(m) >= day
    )
    month.getOrElse(sys.error("Failed to find observedMonth for $market and $day"))
  }
}
