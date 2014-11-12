package evcel.referencedata.market

import evcel.daterange.Day
import evcel.quantity.UOM._
import evcel.referencedata.calendar.Calendar.Holidays
import evcel.referencedata.calendar.{SimpleCalendar, Calendars, TestCalendars}
import evcel.referencedata.{TestFuturesExpiryRules, ReferenceData}
import org.scalatest.{ShouldMatchers, FunSuite}

class FXMarketTest extends FunSuite with ShouldMatchers {
  val rd = testRefData()
  val today = Day(2014, 11, 3) // monday
  val tomorrow = today + 1

  def hols(holidays: Map[String, Set[Day]]) = new Calendars(Map.empty) {
    override def calendar(name: String) = {
      Some(new SimpleCalendar(Holidays(holidays(name))))
    }
  }

  def testRefData(cals: Calendars = TestCalendars.Empty) = ReferenceData(
    TestFuturesExpiryRules.Test,
    cals,
    TestMarkets.Default
  )


  test("test majors") {
    val majors = List(EUR / USD, USD / JPY, GBP / USD, AUD / USD, USD / CHF, NZD / USD, USD / CAD).map {
      uom => FXPair(uom.numerator, uom.denominator)
    }
    majors.map(p => FXMarket(rd, p)) shouldEqual majors.map(new USDFXMarket(_))
    majors.map(p => FXMarket(rd, p.invert)) shouldEqual majors.map(new USDFXMarket(_))
    FXMarket(rd, FXPair(EUR, USD)) shouldEqual new USDFXMarket(FXPair(EUR, USD))
    FXMarket(rd, FXPair(USD, EUR)) shouldEqual new USDFXMarket(FXPair(EUR, USD))
  }

  test("test spot day calc") {
    // http://www.londonfx.co.uk/valdates.html
    // if the trade date is a Monday and a USD holiday falls on the Tuesday, then the spot date for EUR/USD will be
    // the Wednesday, but the spot date for USD/MXN will be the Thursday
    val usdHolD1 = testRefData(hols(Map("USD" -> Set(tomorrow), "EUR" -> Set(), "MXN" -> Set(), "TRY" -> Set())))
    FXMarket(usdHolD1, FXPair(EUR, USD)).spotDate(usdHolD1, today) shouldEqual today + 2
    FXMarket(usdHolD1, FXPair(USD, MXN)).spotDate(usdHolD1, today) shouldEqual today + 3
    FXMarket(usdHolD1, FXPair(USD, TRY)).spotDate(usdHolD1, today) shouldEqual today + 1

    // This cross logic is a bit of a guess. taking the max between EUR/USD and TRY/USD
    FXMarket(usdHolD1, FXPair(EUR, TRY)).spotDate(usdHolD1, today) shouldEqual today + 2
  }

  test("not majors") {
    FXMarket(rd, FXPair(USD, TRY)) shouldEqual new USDFXMarket(FXPair(USD, TRY))
    FXMarket(rd, FXPair(TRY, USD)) shouldEqual new USDFXMarket(FXPair(USD, TRY))
  }

  test("crosses") {
    FXMarket(rd, FXPair(EUR, GBP)) shouldEqual new CrossFXMarket(FXPair(EUR, GBP))
    FXMarket(rd, FXPair(GBP, EUR)) shouldEqual new CrossFXMarket(FXPair(GBP, EUR))

  }
}
