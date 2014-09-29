package evcel.curve

import evcel.calendar.{Calendar, Calendars}
import evcel.curve.curves.FuturesExpiryRules
import evcel.curve.markets.Markets

case class ReferenceData(
  futuresExpiryRules: FuturesExpiryRules,
  calendars: Calendars,
  markets: Markets
  )
