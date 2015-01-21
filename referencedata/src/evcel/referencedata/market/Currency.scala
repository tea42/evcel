package evcel.referencedata.market

import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.quantity.UOM._
import evcel.referencedata.{ReferenceDataTrait, ReferenceDataIdentifier, ReferenceData}
import evcel.utils.EitherUtils._

/**
 * @param daysToSpot 0, 1 or 2
 * @param spotDayUsesBothCalendars Some currencies such as MXN use both the local and the foreign currency
 *                                 calendars to work out days to spot
 */
case class Currency(uom: UOM, name: String, daysToSpot: Int, calendarName: String, spotDayUsesBothCalendars: Boolean)
  extends ReferenceDataTrait {

  def spotDate(refData: ReferenceData, day: Day) = {
    refData.calendars.calendar(calendarName).map(
      _.addBusinessDays(day, daysToSpot)
    ).getOrElse(sys.error("No calendar for " + calendarName))
  }
}
case class CurrencyIdentifier(uom: UOM) extends ReferenceDataIdentifier

case class FXPair(foreignCurrency: UOM, domesticCurrency: UOM) {
  override def toString = s"$foreignCurrency$domesticCurrency"

  def invert = copy(foreignCurrency = domesticCurrency, domesticCurrency = foreignCurrency)
}

trait FXMarket {
  def spotDate(refData: ReferenceData, day: Day): Day
}

case class USDFXMarket private[market](pair: FXPair) extends FXMarket {

  def spotDate(refData: ReferenceData, day: Day) = {
    // http://www.londonfx.co.uk/valdates.html
    // For most T+2 currency pairs, if T+1 is a USD holiday, then this does not normally affect the spot date,
    // but if a non-USD currency in the currency pair has a holiday on T+1, then it will make the spot date
    // become T+3. If USD or either currency of a pair have a holiday on T+2, then the spot date will be T+3.
    // This means, for example, that crosses such as EUR/GBP can never have a spot date on 4th July
    // (although such a date could be quoted as an outright).

    val nonUSD = if(pair.domesticCurrency == USD)
      pair.foreignCurrency
    else
      pair.domesticCurrency

    val spotDay = for (
      usdCurrency <- refData.markets.currency(UOM.USD);
      nonUSDCurrency <- refData.markets.currency(nonUSD);
      usdCalendar <- refData.calendars.calendar(usdCurrency.calendarName);
      nonUSDCalendar <- refData.calendars.calendar(nonUSDCurrency.calendarName)) yield {
      nonUSDCurrency.daysToSpot match {
        case 0 => day
        case 1 => nonUSDCalendar.addBusinessDays(day, 1)
        case 2 =>
          val dayOneCalendar = if(nonUSDCurrency.spotDayUsesBothCalendars)
            usdCalendar && nonUSDCalendar
          else
            nonUSDCalendar
          val plusOne = dayOneCalendar.addBusinessDays(day, 1)
          (usdCalendar && nonUSDCalendar).addBusinessDays(plusOne, 1)
        case o => sys.error("Not valid, only 0, 1 or 2 days: " + o)
      }
    }
    spotDay.getOrElse(sys.error("Failed to calculate spot date for " + this))
  }
}

case class CrossFXMarket private[market](pair: FXPair) extends FXMarket {
  override def spotDate(refData: ReferenceData, day: Day): Day = {
    // I haven't found a clear rule for crosses. Guessing at this for now.
    val perUSD = FXMarket(refData, pair.copy(domesticCurrency = USD))
    val usdPer = FXMarket(refData, pair.copy(foreignCurrency = USD))
    perUSD.spotDate(refData, day).max(usdPer.spotDate(refData, day))
  }
}

object FXMarket {
  // http://en.wikipedia.org/wiki/Currency_pair
  // precedence, low to high
  val precedence = List(EUR, GBP, AUD, NZD, USD, CAD, CHF, JPY)

  def apply(refData: ReferenceData, pair: FXPair) = {
    if(pair.domesticCurrency == USD || pair.foreignCurrency == USD) {
      // index precedence is used to see which way up the pair should be.
      (precedence.indexOf(pair.foreignCurrency), precedence.indexOf(pair.domesticCurrency)) match {
        case (-1, _) => new USDFXMarket(pair.invert)
        case (_, -1) => new USDFXMarket(pair)
        case (a, b) if a < b => new USDFXMarket(pair)
        case (a, b) => new USDFXMarket(pair.invert)
      }
    } else {
      new CrossFXMarket(pair)
    }
  }
}
