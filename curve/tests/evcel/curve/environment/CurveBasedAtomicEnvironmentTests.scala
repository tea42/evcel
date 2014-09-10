package evcel.curve.environment

import evcel.calendar.TestCalendars
import evcel.curve.markets.TestMarkets
import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import evcel.curve.curves.{TestFuturesExpiryRules, FuturesPrices, MissingCurveDataException, FuturesPricesIdentifier}
import evcel.quantity.UOM._
import evcel.quantity.Qty._
import evcel.curve.{EnvironmentParams, ReferenceData, ValuationContext}
import scala.language.reflectiveCalls

class CurveBasedAtomicEnvironmentTests extends FunSpec with Matchers {
  describe("CurveBasedAtomicEnvironment") {
    it("Should return futures prices") {
      val marketDay = MarketDay(10 / Aug / 2014, TimeOfDay.end)
      val futuresPrices = FuturesPrices(
        "WTI",
        MarketDay(10 / Aug / 2014, TimeOfDay.end),
        Map(Jan / 2015 -> 100.0(USD / MT))
      )
      val atomic: AtomicEnvironment = new CurveBasedAtomicEnvironment(
        marketDay,
        Map(FuturesPricesIdentifier("WTI") -> futuresPrices)
      )
      val testRefData = ReferenceData(
        TestFuturesExpiryRules.Test,
        TestCalendars.Empty,
        TestMarkets.Default
      )
      val env = ValuationContext(atomic, testRefData, EnvironmentParams.Default)
      env.futuresPrice("WTI", Jan / 2015) should be(100.0(USD / MT))
      info("Should throw MissingCurveData if price is missing")
      intercept[MissingCurveDataException] {
        env.futuresPrice("WTI", Feb / 2015)
      }
    }
  }
}
