package evcel.report

import evcel.curve.curves.{FuturesVolIdentifier, TestFuturesExpiryRules, DiscountRateIdentifier, FuturesPriceIdentifier}
import evcel.curve.environment.{TimeOfDay, MarketDay}
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.{Day, Month}
import evcel.instrument.{EuropeanOption, FuturesOption}
import evcel.maths.Call
import evcel.maths.models.BlackScholes
import evcel.quantity.{Percent, Qty}
import evcel.quantity.UOM._
import org.scalatest.{ShouldMatchers, FunSuite}


class MtmPivotReportTest extends FunSuite with ShouldMatchers {

  test("mtm report on european option matches black scholes") {
    val month = Month(2014, 12)
    val market = "Nymex WTI"
    val F = Qty("100", USD / BBL)
    val K = Qty("101", USD / BBL)
    val r = 0.05
    val option = new FuturesOption(
      market, month, K, Qty("1", BBL), Call, EuropeanOption
    )
    val expiryDay = TestFuturesExpiryRules.Test.expiryRule(market).map(_.optionExpiryDay(month)).get
    val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
    val T = Act365.timeBetween(marketDay.day, expiryDay)
    val pr = new MtmPivotReport(UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(`market`, `month`) => F
      case DiscountRateIdentifier(USD, day) => math.exp(-r * T)
      case FuturesVolIdentifier(`market`, `month`, K, _) => Percent("20")
      case other => sys.error("invalid req: " + other)
    }))

    val bsValue = new BlackScholes(Call, F.doubleValue, K.doubleValue, .2, T).undiscountedValue * math.exp(-r * T)
    val rows = pr.rows(option)
    rows.size shouldBe 1
    rows.head.value(MtmPivotReportType.MtmField) shouldEqual Qty(bsValue, USD)
  }
}
