package evcel.curve.curves

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import evcel.quantity.UOM._
import evcel.curve.marketdata.Act365
import evcel.quantity.Percent
import scala.math._
import evcel.daterange.Day
import evcel.curve.environment.MarketDay
import evcel.curve.environment.TimeOfDay
import scala.language.reflectiveCalls

class DiscountCurveTests extends FunSpec with Matchers {
  describe("Discount Curve") {
    it("Should be undiscounted if given no rates") {
      val marketDay = 10 / Aug / 2014
      val curve = DiscountCurve(
        GBP,
        marketDay,
        Act365,
        rates = Nil
      )
      curve.discountRate(marketDay).doubleValue should equal(1.0)
      curve.discountRate(marketDay + 100).doubleValue should equal(1.0)
    }

    it("Should be a simple exponential if given a single rate") {
      val marketDay = 10 / Aug / 2014
      val zeroRate = 0.05
      val curve = DiscountCurve(
        GBP,
        marketDay,
        Act365,
        rates = List((marketDay + 10, Percent(zeroRate * 100.0)))
      )
      curve.discountRate(marketDay).doubleValue should equal(1.0)
      curve.discountRate(marketDay + 10).doubleValue should equal(exp(-zeroRate * 10.0 / 365.0) +- 1e-9)
    }

    it("Should match the simple exponential if given two identical rates") {
      val marketDay = 10 / Aug / 2014
      val zeroRate = 0.05
      val simpleCurve = DiscountCurve(
        GBP,
        marketDay,
        Act365,
        rates = List((marketDay + 10, Percent(zeroRate * 100.0)))
      )
      val twoPointCurve = DiscountCurve(
        GBP,
        marketDay,
        Act365,
        rates = List(
          (marketDay + 10, Percent(zeroRate * 100.0)),
          (marketDay + 50, Percent(zeroRate * 100.0))
        )
      )

      for (i <- 0 to 100 by 5) {
        twoPointCurve.discountRate(marketDay + i).doubleValue should be
          (simpleCurve.discountRate(marketDay + i).doubleValue +- 1e-9)
      }
    }

    it("Should have constant forward rates between rate points") {
      val marketDay = 10 / Aug / 2014
      val d1 = marketDay + 33
      val d2 = marketDay + 44
      val d3 = marketDay + 99
      val z1 = 0.05
      val z2 = 0.08
      val z3 = 0.1
      val curve = DiscountCurve(
        GBP,
        marketDay,
        Act365,
        rates = List(
          (d1, Percent(z1 * 100)),
          (d2, Percent(z2 * 100)),
          (d3, Percent(z3 * 100))
        )
      )
      def fwdRate(from: Day, to: Day) = {
        -log(curve.discountRate(to).doubleValue / curve.discountRate(from).doubleValue) / Act365.timeBetween(from, to)
      }
      intercept[RuntimeException] {
        curve(marketDay - 1)
      }
      curve.discountRate(marketDay).doubleValue should be(1.0 +- 1e-9)
      fwdRate(marketDay, marketDay + 1) should be(z1 +- 1e-9)
      fwdRate(marketDay + 1, marketDay + 20) should be(z1 +- 1e-9)
      fwdRate(marketDay + 10, d1) should be(z1 +- 1e-9)
      fwdRate(d1, d2) should be(fwdRate(d1 + 1, d2 - 1) +- 1e-9)
      fwdRate(d2, d3) should be(fwdRate(d2 + 1, d3 - 1) +- 1e-9)
      fwdRate(d2, d3) should be(fwdRate(d2 + 1, d3 + 100) +- 1e-9)

    }
  }
}
