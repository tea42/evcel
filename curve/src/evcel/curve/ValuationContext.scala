package evcel.curve

import evcel.calendar.Calendar
import evcel.curve.curves.FuturesPriceIdentifier
import evcel.daterange.{DateRange, Month, Day}
import evcel.quantity.Qty
import evcel.curve.environment.AtomicEnvironment
import evcel.curve.curves.DiscountRateIdentifier
import evcel.quantity.UOM
import evcel.curve.curves.FuturesVolIdentifier
import evcel.curve.environment.PerturbedAtomicEnvironment

case class ValuationContext(atomic: AtomicEnvironment, refData: ReferenceData, params: EnvironmentParams) {
  def futuresPrice(market: String, month: Month) = atomic(FuturesPriceIdentifier(market, month)).asInstanceOf[Qty]
  def discountRate(currency: UOM, day: Day) = atomic(DiscountRateIdentifier(currency, day)).asInstanceOf[Double]
  def futuresVol(market: String, month: Month, strike: Qty) = {
    atomic(FuturesVolIdentifier(market, month, strike, futuresPrice(market, month))).asInstanceOf[Qty]
  }

  def marketDay = atomic.marketDay
  def optionExpiryDay(market: String, month: Month): Option[Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.optionExpiries.get(month))

  def futuresCalendar(market: String): Option[Calendar] =
    futureMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))
  def spotCalendar(market: String): Option[Calendar] =
    spotMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))

  def futureMarket(name: String) = refData.markets.futuresMarket(name)
  def spotMarket(name: String) = refData.markets.spotMarket(name)

  def shiftFuturesPrice(market: String, month: Month, dP: Qty) = copy(atomic =
    PerturbedAtomicEnvironment(
    atomic, { case FuturesPriceIdentifier(`market`, `month`) => futuresPrice(market, month) + dP})
  )
}
