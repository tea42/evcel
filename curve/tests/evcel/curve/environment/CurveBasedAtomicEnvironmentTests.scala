package evcel.curve.environment

import evcel.referencedata.calendar.TestCalendars
import evcel.referencedata.market.TestMarkets
import org.scalatest.{FunSpec, Matchers, EitherValues}
import evcel.daterange.DateRangeSugar._
import evcel.curve.curves.FuturesPrices
import evcel.referencedata.{TestFuturesExpiryRules, ReferenceData}
import evcel.quantity.UOM._
import evcel.quantity.Qty._
import evcel.curve.{EnvironmentParams, ValuationContext}
import scala.language.reflectiveCalls
import scala.util.{Left, Right}
import evcel.utils.EitherTestPimps

class CurveBasedAtomicEnvironmentTests extends FunSpec with Matchers with EitherValues
  with EitherTestPimps
{
  describe("CurveBasedAtomicEnvironment") {
    it("Should return futures prices") {
      val marketDay = MarketDay(10 / Aug / 2014, TimeOfDay.end)
      val futuresPrices = FuturesPrices(
        "Nymex WTI",
        MarketDay(10 / Aug / 2014, TimeOfDay.end),
        Map(Jan / 2015 -> 100.0(USD / MT))
      )
      val atomic: AtomicEnvironment = new CurveBasedAtomicEnvironment(
        marketDay,
        Map(FuturesPricesIdentifier("Nymex WTI") -> futuresPrices)
      )
      val testRefData = ReferenceData(
        TestFuturesExpiryRules.Test,
        TestCalendars.Empty,
        TestMarkets.Default
      )
      val env = ValuationContext(atomic, testRefData, EnvironmentParams.Default)
      val wti = env.futuresMarket("Nymex WTI").R
      env.futuresPrice(wti, Jan / 2015).R should be(100.0(USD / MT))
      info("Missing price should return a left")
      env.futuresPrice(wti, Feb / 2015) should be ('left)
    }
  }
}
