package evcel.curve

import evcel.curve.curves._
import evcel.curve.environment._
import evcel.daterange.{DateRange, Day, Month}
import evcel.quantity.UOM._
import evcel.quantity.{Qty, UOM}
import evcel.referencedata.ReferenceData
import evcel.referencedata.calendar.Calendar
import evcel.referencedata.market._
import evcel.utils.EitherUtils._
import evcel.utils.EvcelFail

import scala.util.{Either, Right}

case class ValuationContext(atomic: AtomicEnvironment, refData: ReferenceData, params: EnvironmentParams) {
  def futuresPrice(market: FuturesMarket, month: Month) : Either[EvcelFail, Qty] =
    atomic(FuturesPriceIdentifier(market, month))
  def spotPrice(market: SpotMarket, day: Day) =
    atomic(SpotPriceIdentifier(market, day))
  def discountRate(currency: UOM, day: Day) =
    atomic(DiscountRateIdentifier(currency, day))
  def fixing(index: Index, observationDay: Day): Either[EvcelFail, Qty] = {
      index.observable(refData, observationDay).flatMap(
        ndx => atomic(PriceFixingIdentifier(ndx, observationDay))
      )
  }
  def fixing(spotMarket: SpotMarket, observationDay: Day): Either[EvcelFail, Qty] =
    fixing(SpotIndex(spotMarket), observationDay)

  /**
   * spot fx discounted back to today
   */
  def todayFX(pair: FXPair): Either[EvcelFail, Qty] = {
    val fxMarket = refData.fxMarket(pair)
    val spotDay = fxMarket.spotDate(refData, marketDay.day)
    for {
      spotRate <- spotFX(pair)
      foreignDiscount <- discountRate(pair.foreignCurrency, spotDay)
      domesticDiscount <- discountRate(pair.domesticCurrency, spotDay)
    } yield {
      spotRate * (foreignDiscount / domesticDiscount)
    }
  }

  def spotFX(pair: FXPair): Either[EvcelFail, Qty] = {
    if(pair.domesticCurrency == baseCCY)
      atomic(new BaseFXRateKey(baseCCY, pair.foreignCurrency))
    else if(pair.foreignCurrency == baseCCY)
      atomic(new BaseFXRateKey(baseCCY, pair.domesticCurrency)).map(_.invert)
    else{
      for {
        baseToDomestic <- spotFX(FXPair(baseCCY, pair.domesticCurrency)) 
        foreignToBase <- spotFX(FXPair(pair.foreignCurrency, baseCCY))
      } yield {
        baseToDomestic * foreignToBase
      }
    }
  }
  def forwardFX(pair: FXPair, day: Day) = {
    for {
      rd <- discountRate(pair.domesticCurrency, day)
      rf <- discountRate(pair.foreignCurrency, day)
      todaysFX <- todayFX(pair)
    } yield {
      todaysFX * rf / rd
    }
  }

  def futuresVol(market: FuturesMarket, month: Month, strike: Qty) =
    futuresPrice(market, month).flatMap{
      F => 
        atomic(FuturesVolIdentifier(market, month, strike, F))
  }

  def baseCCY = params.baseCCY
  def valuationCcy = params.valuationCcy

  def marketDay = atomic.marketDay
  def futureExpiryDay(market: String, month: Month): Either[EvcelFail, Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.futureExpiryDay(month))
  def optionExpiryDay(market: String, month: Month): Either[EvcelFail, Day] =
    refData.futuresExpiryRules.expiryRule(market).flatMap(_.optionExpiryDay(month))

  def spotCalendar(market: String): Either[EvcelFail, Calendar] =
    spotMarket(market).flatMap(m => refData.calendars.calendar(m.calendarName))
  def futuresMarket(name: String) = refData.markets.futuresMarket(name)
  def spotMarket(name: String) = refData.markets.spotMarket(name)

  def shiftFuturesPrice(market: FuturesMarket, month: Month, dP: Qty) = copy(atomic =
    PerturbedAtomicEnvironment(
    atomic, { case FuturesPriceIdentifier(`market`, `month`) => futuresPrice(market, month).map(_ + dP)})
  )

  def shiftSpotPrice(market: SpotMarket, period: DateRange, dP: Qty) = copy(atomic =
    PerturbedAtomicEnvironment(
    atomic, { case SpotPriceIdentifier(`market`, day) if period.contains(day) => spotPrice(market, day).map(_ + dP)})
  )

  def shiftPrice(pi: PriceIdentifier, dP: Qty) = pi match {
    case SpotPriceIdentifier(market, day) => shiftSpotPrice(market, day, dP)
    case FuturesPriceIdentifier(market, month) => shiftFuturesPrice(market, month, dP)
    case o => sys.error("Invalid: " + o)
  }

  private def price(pi: PriceIdentifier) = atomic(pi).getOrErrorLeft(_.s)

  def keyRecordingVC: (ValuationContext, KeyRecordingAtomicEnvironment) = {
    val record = KeyRecordingAtomicEnvironment(atomic, refData)
    (copy(atomic = record), record)
  }

  def forwardState(forwardDay: MarketDay) = copy(atomic = ForwardStateEnvironment(refData, atomic, forwardDay))

  def undiscounted = {
    val perturbed = PerturbedAtomicEnvironment(
      atomic, 
      { case DiscountRateIdentifier(_, _) => Right(Qty(1.0, SCALAR)) }
    )
    copy(atomic = perturbed)
  }

  def withParam(f: EnvironmentParams => EnvironmentParams) = copy(params = f(params))
  def withValuationCCY(ccy: UOM) = withParam(_.withValuationCcy(ccy))
}
