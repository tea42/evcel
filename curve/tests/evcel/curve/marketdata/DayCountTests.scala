package evcel.curve.marketdata

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import evcel.curve.environment.MarketDay
import evcel.curve.environment.TimeOfDay
import scala.language.reflectiveCalls

class DayCountTests extends FunSpec with Matchers {
  describe("Act365") {
    it("Should take account of time of day correctly") {
      import TimeOfDay._
      import Act365._
      val d1 = 10 / Aug / 2014
      val d2 = d1 + 1

      optionTimeBetween(
        MarketDay(d1, end),
        MarketDay(d2, start)
      ) should equal(0.0 +- 1e-9)

      optionTimeBetween(
        MarketDay(d1, start),
        MarketDay(d2, start)
      ) should equal(timeBetween(d1, d2) +- 1e-9)

      optionTimeBetween(
        MarketDay(d1, start),
        MarketDay(d2, end)
      ) should equal(timeBetween(d1, d2 + 1) +- 1e-9)

      optionTimeBetween(
        MarketDay(d1, end),
        MarketDay(d2, end)
      ) should equal(timeBetween(d1, d2) +- 1e-9)

      optionTimeBetween(
        MarketDay(d1, end),
        MarketDay(d1, start)
      ) should equal(-timeBetween(d1, d2) +- 1e-9)
    }
  }
}
