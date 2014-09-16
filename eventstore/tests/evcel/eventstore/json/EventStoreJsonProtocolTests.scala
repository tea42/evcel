package evcel.eventstore.json

import evcel.curve.curves.FuturesExpiryRule
import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.daterange.{SimpleDateRange, DateRange, Day, Month}
import evcel.eventstore.json.EventStoreJsonProtocol._
import evcel.quantity.{BDQty, Percent, Qty}
import evcel.quantity.UOM._
import org.scalatest.{FunSpec, Matchers}
import spray.json._

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
        List(
          (Jun / 2014, Qty("100.0", USD / MT))
        )
      )
      prices.toJson.prettyPrint.parseJson.convertTo[FuturesPriceData] should equal(prices)

      info("Also when type is MarketData")
      val marketData : MarketData = prices
      marketData.toJson.prettyPrint.parseJson.convertTo[MarketData] should equal(marketData)
    }

    it("Should round trip zero rates") {
      val zeros = ZeroRateData(
        Act365,
        List(
          (11 / Aug / 14, Percent("5")),
          (15 / Aug / 14, Percent("50"))
        )
      )
      zeros.toJson.prettyPrint.parseJson.convertTo[ZeroRateData] should equal(zeros)

      info("Also when type is MarketData")
      val marketData : MarketData = zeros
      marketData.toJson.prettyPrint.parseJson.convertTo[MarketData] should equal(marketData)

    }

    it("Should round trip futures vols") {
      val vols = FuturesVolData(
        List(
          (Sep / 2014, Percent("20"), List((0.5, Percent("20")), (0.2, Percent("10")))),
          (Dec / 2014, Percent("20"), List((0.2, Percent("20")))),
          (Jan / 2014, Percent("20"), Nil)
        )
      )
      vols.toJson.prettyPrint.parseJson.convertTo[FuturesVolData] should equal(vols)

      info("Also when type is MarketData")
      val marketData : MarketData = vols
      marketData.toJson.prettyPrint.parseJson.convertTo[MarketData] should equal(marketData)

    }

    it("Should round trip expiry rule data") {
      val expiries = FuturesExpiryRule("WTI",
        Map[Month, Day](Jun / 2014 -> 30 / May / 2014),
        Map[Month, Day](Jul / 2014 -> 29 / May / 2014))
      expiries.toJson.prettyPrint.parseJson.convertTo[FuturesExpiryRule] should equal(expiries)
    }

    it("Should round trip abstract date ranges"){
      val dateRanges : List[DateRange] = List(May / 2014, 15 / May / 2014, SimpleDateRange(15 / May / 2014, 20 / May / 2014))
      dateRanges.foreach{
        case dr =>
            dr.toJson.prettyPrint.parseJson.convertTo[DateRange] should equal(dr)
      }
    }
  }
}
