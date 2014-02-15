package evcel.eventstore.json

import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.daterange._
import evcel.eventstore.json.EventStoreJsonProtocol._
import evcel.quantity.{BDQty, Percent, Qty}
import evcel.quantity.UOM._
import org.scalatest.{FunSpec, Matchers}
import spray.json._
import scala.language.reflectiveCalls
import evcel.referencedata.{FuturesExpiryRule, ReferenceDataTrait}
import evcel.referencedata.market.{Currency, FuturesMarket, VolumeCalcRuleLabel}
import evcel.referencedata.calendar.CalendarData
import evcel.instrument._
import evcel.quantity.Qty._
import evcel.instrument.{NonCommonSwapPricingRule, CommonSwapPricingRule}
import scala.collection.immutable.Nil

class EventStoreJsonProtocolTests extends FunSpec with Matchers {
  describe("EventStoreProtocol must be able to round trip") {

    it("Date ranges") {
      info("Days")
      val day = 10 / Aug / 2014
      day.toJson.prettyPrint.parseJson.convertTo[Day] should equal(day)

      info("Months")
      val m = Jun / 2014
      m.toJson.prettyPrint.parseJson.convertTo[Month] should equal(m)

      info("Simple date ranges")
      val sdr = SimpleDateRange(day, day + 1)
      sdr.toJson.prettyPrint.parseJson.convertTo[SimpleDateRange] should equal(sdr)

      info("Abstract DateRange")
      List(day, m, sdr).foreach{
        case dr =>
            dr.toJson.prettyPrint.parseJson.convertTo[DateRange] should equal(dr)
      }
    }

    it("Holiday maps") {
      val map = Map(
        "UK" -> Set(10 / Aug / 14),
        "US" -> Set[Day](),
        "FR" -> Set(14 / Jul / 2000, 14 / Jul / 2001)
      )
      map.toJson.prettyPrint.parseJson.convertTo[Map[String, Set[Day]]] should equal(map)
    }

    it("Quantities") {
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

    it("Market Data") {
      info("Futures Prices")
      val prices = FuturesPriceData(
        List(
          (Jun / 2014, Qty("100.0", USD / MT))
        )
      )
      prices.toJson.prettyPrint.parseJson.convertTo[FuturesPriceData] should equal(prices)

      info("Fixings")
      val fixings = PriceFixingData(Qty("100.0", USD / MT))
      fixings.toJson.prettyPrint.parseJson.convertTo[PriceFixingData] should equal(fixings)

      info("Zeros")
      val zeros = ZeroRateData(
        Act365,
        List(
          (11 / Aug / 14, Percent("5")),
          (15 / Aug / 14, Percent("50"))
        )
      )
      zeros.toJson.prettyPrint.parseJson.convertTo[ZeroRateData] should equal(zeros)

      info("spotfx")
      val spotfx = SpotFXData(
        Qty("1.6", USD/GBP)
      )
      spotfx.toJson.prettyPrint.parseJson.convertTo[SpotFXData] should equal(spotfx)

      info("Futures Vols")
      val vols = FuturesVolData(
        List(
          (Sep / 2014, Percent("20"), List((0.5, Percent("20")), (0.2, Percent("10")))),
          (Dec / 2014, Percent("20"), List((0.2, Percent("20")))),
          (Jan / 2014, Percent("20"), Nil)
        )
      )
      vols.toJson.prettyPrint.parseJson.convertTo[FuturesVolData] should equal(vols)

      info("Spot prices")
      val spotPrices = SpotPriceData(Map(1 / Jun / 2014  -> Qty("10", USD/MT)))
      spotPrices.toJson.prettyPrint.parseJson.convertTo[SpotPriceData] should equal(spotPrices)

      info("Abstract MarketData")
      val marketData : List[MarketData] = List(prices, fixings, zeros, vols, spotPrices)
      marketData.foreach{
        case md : MarketData => 
          md.toJson.prettyPrint.parseJson.convertTo[MarketData] should equal(md)
      }
    }

    it("Reference Data") {
      info("Futures expiry rule") 
      val expiries = FuturesExpiryRule("WTI",
        Map[Month, Day](Jun / 2014 -> 30 / May / 2014),
        Map[Month, Day](Jul / 2014 -> 29 / May / 2014))
      expiries.toJson.prettyPrint.parseJson.convertTo[FuturesExpiryRule] should equal(expiries)

      info("Futures Markets")
      val market = FuturesMarket("WTI", "CALENDAR", USD/MT, MT, VolumeCalcRuleLabel.Default)
      market.toJson.prettyPrint.parseJson.convertTo[FuturesMarket] should equal (market)

      info("Currency")
      val ccy = Currency(USD, "US Dollar", 2, "USDCAL", spotDayUsesBothCalendars = false)
      ccy.toJson.prettyPrint.parseJson.convertTo[Currency] should equal (ccy)

      info("Calendar Data")
      val calendarData = CalendarData(Set(10 / Jun / 2014, 15 / Oct / 2015))
      calendarData.toJson.prettyPrint.parseJson.convertTo[CalendarData] should equal (calendarData)

      info("Abstract ReferenceData")
      val refData : List[ReferenceDataTrait] = List(expiries, market, calendarData)
      refData.foreach{
        case rd : ReferenceDataTrait => 
          rd.toJson.prettyPrint.parseJson.convertTo[ReferenceDataTrait] should equal (rd)
      }
    }

    it("Instruments"){
      info("Future")
      Future.samples.foreach{
        future => 
          future.toJson.prettyPrint.parseJson.convertTo[Future] should equal (future)
      }

      info("Commodity Swap")
      CommoditySwap.samples.foreach{
        swap => 
          swap.toJson.prettyPrint.parseJson.convertTo[CommoditySwap] should equal (swap)
      }

      info("Commodity Swap Spread")
      CommoditySwapSpread.samples.foreach{
        spread => 
          spread.toJson.prettyPrint.parseJson.convertTo[CommoditySwapSpread] should equal (spread)
      }

      info("Commodity Swap Lookalike")
      CommoditySwapLookalike.samples.foreach{
        swap => 
          swap.toJson.prettyPrint.parseJson.convertTo[CommoditySwapLookalike] should equal (swap)
      }

      info("Abstract Instrument")
      InstrumentType.types.flatMap(_.samples).foreach{
        case inst : Instrument => 
          inst.toJson.prettyPrint.parseJson.convertTo[Instrument] should equal (inst)
      }
      info("Abstract Tradeable")
      TradeableType.types.flatMap(_.samples).foreach{
        case inst : Tradeable => 
          inst.toJson.prettyPrint.parseJson.convertTo[Tradeable] should equal (inst)
      }
    }
  }
}
