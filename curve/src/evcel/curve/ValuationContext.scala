package evcel.curve

import evcel.calendar.Calendar
import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, FuturesVolIdentifier}
import evcel.curve.environment.{AtomicEnvironment, KeyRecordingAtomicEnvironment, PerturbedAtomicEnvironment}
import evcel.daterange.{Day, Month}
import evcel.quantity.{Qty, UOM}

case class ValuationContext(atomic: AtomicEnvironment, refData: ReferenceData, params: EnvironmentParams) {
  def futuresPrice(market: String, month: Month) =
    atomic(FuturesPriceIdentifier(market, month)).asInstanceOf[Qty]
  def discountRate(currency: UOM, day: Day) = atomic(DiscountRateIdentifier(currency, day)).asInstanceOf[Double]
  def futuresVol(market: String, month: Month, strike: Qty) = {
    atomic(FuturesVolIdentifier(market, month, strike, futuresPrice(market, month))).asInstanceOf[Qty]
  }

  def valuationCcy = params.valuationCcy

  def marketDay = atomic.marketDay
  def futureExpiryDay(market: String, month: Month): Option[Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.futureExpiryDay(month))
  def optionExpiryDay(market: String, month: Month): Option[Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.optionExpiryDay(month))

  def futuresCalendar(market: String): Option[Calendar] =
    futuresMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))
  def futuresCalendarOrThrow(market: String): Calendar =
    futuresCalendar(market).getOrElse(sys.error("No calendar for $market"))
  def spotCalendar(market: String): Option[Calendar] =
    spotMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))

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
