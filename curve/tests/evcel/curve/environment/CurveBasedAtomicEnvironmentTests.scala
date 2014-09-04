package evcel.curve.environment

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import evcel.curve.curves.FuturesPrices
import evcel.quantity.UOM._
import evcel.quantity.Qty._
import evcel.curve.Environment
import evcel.curve.curves.MissingCurveData
import evcel.curve.curves.FuturesPricesIdentifier
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
      val env = Environment(atomic)
      env.futuresPrice("WTI", Jan / 2015) should be(100.0(USD / MT))
      info("Should throw MissingCurveData if price is missing")
      intercept[MissingCurveData] {
        env.futuresPrice("WTI", Feb / 2015)
      }
    }
  }
}
