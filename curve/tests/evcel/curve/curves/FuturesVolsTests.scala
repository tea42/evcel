package evcel.curve.curves

import evcel.curve.environment.{MarketDay, TimeOfDay}
import evcel.curve.marketdata.FuturesVolData
import evcel.daterange.DateRangeSugar._
import evcel.daterange.Month
import evcel.quantity.{BDQty, Percent}
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import org.scalatest.{FunSpec, Matchers}

import scala.language.reflectiveCalls

class FuturesVolsTests extends FunSpec with Matchers {
  private def makeFuturesVols(smiles: (Month, List[(Double, BDQty)])*) = {
    val marketDay = MarketDay(10 / Aug / 2014, TimeOfDay.end)
    FuturesVols(
      FuturesVolData(
        "WTI",
        marketDay.day,
        smiles.toList
      ),
      marketDay,
      FuturesExpiryRule(
        "WTI",
        (Sep / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9) }.toMap,
        (Sep / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10) }.toMap
      )
    )
  }

  describe("FuturesVols") {
    it("Should handle flat vol") {
      val vols = makeFuturesVols(
        (Dec / 2014, List((0.5, Percent("20"))))
      )
      vols.interpolateVol(Dec / 2014, X = 100.0, F = 110.0) should equal(0.2 +- 1e-6)
      vols.apply((Dec/2014, 100.0(USD/MT), 110.0(USD/MT))).right.get.checkedDouble(PERCENT) should
        equal(20.0 +- 1e-6)
    }

    it("Should be flat beyond boundary of supplied data") {
      val vols = makeFuturesVols(
        (Dec / 2014,
          List(
            (0.3, Percent("20")),
            (0.7, Percent("50"))))
      )
      // Deep OTM call
      vols.interpolateVol(Dec / 2014, X = 100.0, F = 1.0) should equal(0.2 +- 1e-6)
      // Deep ITM call
      vols.interpolateVol(Dec / 2014, X = 100.0, F = 1.0e6) should equal(0.5 +- 1e-6)
    }
  }
}
