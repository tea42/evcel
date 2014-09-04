package evcel.eventstore.json

import org.scalatest.FunSpec
import org.scalatest.Matchers
import EventStoreJsonProtocol._
import evcel.daterange.DateRangeSugar._
import spray.json._
import evcel.daterange.Day
import evcel.quantity.Qty._
import evcel.quantity.Qty
import evcel.quantity.BDQty
import evcel.quantity.UOM._
import evcel.quantity.UOM
import evcel.curve.marketdata.FuturesPriceData
import evcel.daterange.Month
import evcel.curve.marketdata.ZeroRateData
import evcel.curve.marketdata.Act365
import evcel.quantity.Percentage
import evcel.curve.marketdata.FuturesVolData
import evcel.curve.curves.FuturesExpiryRule
import scala.language.reflectiveCalls

class EventStoreJsonProtocolTests extends FunSpec with Matchers {
  describe("EventStoreProtocol") {

    it("Should be able to round trip days") {
      val day = 10 / Aug / 2014
      day.toJson.prettyPrint.parseJson.convertTo[Day] should equal(day)
    }

    it("Should round trip holiday maps") {
      val map = Map(
        "UK" -> Set(10 / Aug / 14),
        "US" -> Set[Day](),
        "FR" -> Set(14 / Jul / 2000, 14 / Jul / 2001)
      )
      map.toJson.prettyPrint.parseJson.convertTo[Map[String, Set[Day]]] should equal(map)
    }

    it("Should round trip quantities") {
      List(
        Qty("10.0", USD),
        Qty("5", MT),
        Qty.NULL,
        Qty("4.3", SCALAR)
      ).foreach {
          q =>
            q.toJson.prettyPrint.parseJson.convertTo[BDQty] should equal(q)
        }
    }

    it("Should round trip months") {
      val m = Jun / 2014
      m.toJson.prettyPrint.parseJson.convertTo[Month] should equal(m)
    }

    it("Should round trip futures prices") {
      val prices = FuturesPriceData(
        10 / Aug / 2014,
        "WTI",
        List(
          (Jun / 2014, Qty("100.0", USD / MT))
        )
      )
      prices.toJson.prettyPrint.parseJson.convertTo[FuturesPriceData] should equal(prices)
    }

    it("Should round trip ZeroRateData") {
      val data = ZeroRateData(
        GBP,
        10 / Aug / 2014,
        Act365,
        List(
          (11 / Aug / 14, Percentage("5")),
          (15 / Aug / 14, Percentage("50"))
        )
      )
      data.toJson.prettyPrint.parseJson.convertTo[ZeroRateData] should equal(data)
    }

    it("Should round trip futures vols") {
      val data = FuturesVolData(
        "WTI",
        10 / Aug / 2014,
        List(
          (Sep / 2014, List((0.5, Percentage("20")), (0.2, Percentage("10")))),
          (Dec / 2014, List((0.2, Percentage("20")))),
          (Jan / 2014, Nil)
        )
      )
      data.toJson.prettyPrint.parseJson.convertTo[FuturesVolData] should equal(data)
    }

    it("Should round trip expiry rule data") {
      val expiries = FuturesExpiryRule("WTI", Map[Month, Day](Jun / 2014 -> 30 / May / 2014))
      expiries.toJson.prettyPrint.parseJson.convertTo[FuturesExpiryRule] should equal(expiries)
    }
  }
}
