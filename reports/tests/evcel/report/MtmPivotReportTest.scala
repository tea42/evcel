package evcel.report

import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, FuturesVolIdentifier, TestFuturesExpiryRules}
import evcel.curve.environment.{MarketDay, TimeOfDay}
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.{DateRange, Day, Month}
import evcel.instrument.valuation.TestInstrumentValuationContext
import evcel.instrument.{CommoditySwap, EuropeanOption, Future, FuturesOption}
import evcel.maths.Call
import evcel.maths.models.BlackScholes
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.quantity.utils.QuantityTestUtils
import evcel.quantity.{Percent, Qty}
import org.scalatest.{FunSuite, ShouldMatchers}

class MtmPivotReportTest extends FunSuite with ShouldMatchers {
  val ivc = TestInstrumentValuationContext.Test

  test("mtm report on european option matches black scholes") {
    val month = Month(2014, 12)
    val market = "Nymex WTI"
    val F = Qty("100", USD / BBL)
    val K = Qty("101", USD / BBL)
    val r = 0.05
    val option = new FuturesOption(
      market, month, K, Qty("1", BBL), Call, EuropeanOption
    )
    val expiryDay = TestFuturesExpiryRules.Test.expiryRule(market).map(_.optionExpiryDayOrThrow(month)).get
    val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
    val T = Act365.timeBetween(marketDay.day, expiryDay)
    val pr = new MtmPivotReport(UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(`market`, `month`) => F
      case DiscountRateIdentifier(USD, day) => math.exp(-r * T)
      case FuturesVolIdentifier(`market`, `month`, K, _) => Percent("20")
    }), ivc)

    val bsValue = new BlackScholes(Call, F.doubleValue, K.doubleValue, .2, T).undiscountedValue * math.exp(-r * T)
    val rows = pr.rows(option)
    rows.size shouldBe 1
    rows.head.value(MtmPivotReportType.MtmField) shouldEqual Qty(bsValue, USD)
  }

  test("mtm report on future") {
    val month = Month(2014, 12)
    val market = "Nymex WTI"
    val K = Qty("91", USD / BBL)
    val future = new Future(
      market, month, K, Qty("1", BBL)
    )
    val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
    val vc = UnitTestingEnvironment.Null(marketDay)
    val F = vc.futuresPrice(market, month)
    val pr = new MtmPivotReport(vc, ivc)
    val rows = pr.rows(future)
    rows.size shouldBe 1
    rows.head.value(MtmPivotReportType.MtmField) shouldEqual (F - K) * Qty("1", BBL)
  }

  test("mtm report on swap") {
    val oct = Month(2014, 10)
    val nov = oct.next
    val dec = nov.next
    val market = "Nymex WTI"
    val index = "Nymex WTI nearby 1"
    val K = Qty("91", USD / BBL)
    val Fnov = Qty("93", USD / BBL)
    val Fdec = Qty("98", USD / BBL)
    val swap = new CommoditySwap(
      index, oct, K, Qty("1", BBL)
    )
    val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
    val vc = UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(`market`, `nov`) => Fnov
      case FuturesPriceIdentifier(`market`, `dec`) => Fdec
      case other => sys.error("invalid req: " + other)
    })
    val pr = new MtmPivotReport(vc, ivc)
    val rows = pr.rows(swap)
    rows.size shouldBe 1

    def observedDaysToMonth(vc: ValuationContext, delivery: DateRange): Map[Day, Month] = {
      val fm = vc.futuresMarketOrThrow(market)
      val calendar = vc.futuresCalendarOrThrow(market)
      val observationDays = oct.days.filter(calendar.isBusinessDay).toIterable
      observationDays.map(d => d -> fm.frontMonth(vc.refData, d)).toMap
    }

    val weights = {
      val observed = observedDaysToMonth(vc, oct).groupBy(_._2).mapValues(_.size)
      observed.mapValues(i => BigDecimal(i) / BigDecimal(observed.values.sum))
    }

    val expected: Qty = ((Fnov * weights(nov) + Fdec * weights(dec)) - K) * Qty("1", BBL)
    val actual: Qty = rows.head.value(MtmPivotReportType.MtmField)

    implicit val equality = new QuantityTestUtils.EssentiallyEqual()
    actual shouldEqual expected
  }
}
