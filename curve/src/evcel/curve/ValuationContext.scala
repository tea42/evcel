package evcel.curve

import evcel.calendar.Calendar
import evcel.curve.curves._
import evcel.curve.environment.{AtomicEnvironment, KeyRecordingAtomicEnvironment, PerturbedAtomicEnvironment}
import evcel.daterange.{Day, Month}
import evcel.quantity.{Qty, UOM}
import evcel.utils.EitherUtils._

case class ValuationContext(atomic: AtomicEnvironment, refData: ReferenceData, params: EnvironmentParams) {
  def futuresPrice(market: String, month: Month) =
    atomic.qty(FuturesPriceIdentifier(market, month)).getOrErrorLeft(_.s)
  def spotPrice(market: String, day: Day) =
    atomic.qty(SpotPriceIdentifier(market, day)).getOrErrorLeft(_.s)
  def discountRate(currency: UOM, day: Day) =
    atomic.double(DiscountRateIdentifier(currency, day)).getOrErrorLeft(_.s)
  def futuresVol(market: String, month: Month, strike: Qty) =
    atomic.qty(FuturesVolIdentifier(market, month, strike, futuresPrice(market, month))).getOrErrorLeft(_.s)

  def valuationCcy = params.valuationCcy

  def marketDay = atomic.marketDay
  def futureExpiryDay(market: String, month: Month): Option[Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.futureExpiryDay(month))
  def futureExpiryDayOrThrow(market: String, month: Month): Day =
    futureExpiryDay(market, month).getOrElse(sys.error(s"No expiry for $market, $month"))
  def optionExpiryDay(market: String, month: Month): Option[Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.optionExpiryDay(month))

  def futuresCalendar(market: String): Option[Calendar] =
    futuresMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))
  def futuresCalendarOrThrow(market: String): Calendar =
    futuresCalendar(market).getOrElse(sys.error(s"No calendar for $market"))
  def spotCalendar(market: String): Option[Calendar] =
    spotMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))
  def spotCalendarOrThrow(market: String): Calendar =
    spotCalendar(market).getOrElse(sys.error(s"No calendar for $market"))

  def futuresMarket(name: String) = refData.markets.futuresMarket(name)
  def futuresMarketOrThrow(name: String) = refData.markets.futuresMarketOrThrow(name)
  def spotMarket(name: String) = refData.markets.spotMarket(name)

  def shiftFuturesPrice(market: String, month: Month, dP: Qty) = copy(atomic =
    PerturbedAtomicEnvironment(
    atomic, { case FuturesPriceIdentifier(`market`, `month`) => futuresPrice(market, month) + dP})
  )

  def keyRecordingVC: (ValuationContext, KeyRecordingAtomicEnvironment) = {
    val record = KeyRecordingAtomicEnvironment(atomic, refData)
    (copy(atomic = record), record)
  }
}