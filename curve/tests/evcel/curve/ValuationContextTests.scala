package evcel.curve


import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.daterange.{Month, Day}
import evcel.quantity.UOM._
import evcel.quantity.{Percent, Qty}
import evcel.quantity.Qty._
import evcel.referencedata.market.{TestMarkets, FXPair}
import org.scalatest.{Matchers, FunSuite, FunSpecLike}
import evcel.quantity.utils.QuantityTestUtils._
import scala.language.reflectiveCalls
import evcel.utils.EitherTestPimps

class ValuationContextTests extends FunSuite with MarketDataTest with Matchers with EitherTestPimps{

  val vc = UnitTestingEnvironment.fromMarketData(
    (10 / Sep / 2014).endOfDay,

    "Nymex WTI" -> futuresPrices(Sep / 2014 -> Qty("100", USD / MT)),

    USD -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("5"), (31 / Dec / 2015) -> Percent("8"))),
    GBP -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("1"), (31 / Dec / 2015) -> Percent("2"))),
    EUR -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("3"), (31 / Dec / 2015) -> Percent("4.5"))),

    (USD, GBP) -> SpotFXData(Qty("1.6", USD / GBP)),
    (USD, EUR) -> SpotFXData(Qty("1.25", USD / EUR)),

    "Nymex WTI" -> FuturesVolData(List((Sep / 2014, Percent("20"), Nil)))
  )

  test("spot fx") {
    vc.spotFX(FXPair(GBP, GBP)).R shouldEqual Qty(1, SCALAR)
    vc.spotFX(FXPair(GBP, USD)).R shouldEqual Qty(1.6, USD/GBP)
    vc.spotFX(FXPair(USD, GBP)).R shouldEqual Qty(1.6, USD/GBP).invert
    vc.spotFX(FXPair(GBP, EUR)).R shouldEqual vc.spotFX(FXPair(USD, GBP)).R.invert * vc.spotFX(FXPair(USD, EUR)).R
    vc.spotFX(FXPair(EUR, GBP)).R shouldEqual vc.spotFX(FXPair(GBP, EUR)).R.invert
  }

  test("today fx") {
    vc.todayFX(FXPair(GBP, GBP)).R shouldEqual Qty(1, SCALAR)

    vc.todayFX(FXPair(GBP, USD)).R shouldEqual
      Qty(1.6, USD/GBP) *
        vc.discountRate(GBP, vc.marketDay.day + 2).R / vc.discountRate(USD, vc.marketDay.day + 2).R

    vc.todayFX(FXPair(GBP, EUR)).R should be
      vc.spotFX(FXPair(USD, GBP)).R.invert * vc.spotFX(FXPair(USD, EUR)).R *
        vc.discountRate(GBP, vc.marketDay.day + 2).R / vc.discountRate(EUR, vc.marketDay.day + 2).R +- 1e-9
  }

  test("forward fx") {
    val day = vc.marketDay.day + 150
    vc.forwardFX(FXPair(GBP, GBP), day).R shouldEqual Qty(1, SCALAR)

    vc.forwardFX(FXPair(GBP, USD), day).R shouldEqual
      Qty(1.6, USD/GBP) *
        vc.discountRate(GBP, vc.marketDay.day + 2).R / vc.discountRate(USD, vc.marketDay.day + 2).R *
        vc.discountRate(GBP, day).R / vc.discountRate(USD, day).R

    vc.forwardFX(FXPair(GBP, EUR), day).R should be
      vc.spotFX(FXPair(USD, GBP)).R.invert * vc.spotFX(FXPair(USD, EUR)).R *
        vc.discountRate(GBP, vc.marketDay.day + 2).R / vc.discountRate(EUR, vc.marketDay.day + 2).R *
        vc.discountRate(GBP, day).R / vc.discountRate(EUR, day).R +- 1e-9
  }

  test("forwardState for discount curve") {
    val forwardDate = vc.marketDay.day + 150
    vc.forwardState(forwardDate.endOfDay).discountRate(USD, forwardDate).R.doubleValue shouldEqual 1.0
    vc.forwardState(forwardDate.endOfDay).discountRate(USD, forwardDate + 50).R shouldEqual
      vc.discountRate(USD, forwardDate + 50).R / vc.discountRate(USD, forwardDate).R
  }

  test("forwardstate for spotfx") {
    val pair = FXPair(GBP, USD)
    val forwardDate = vc.marketDay.day + 150
    val forwardSpotDate = vc.refData.fxMarket(pair).spotDate(vc.refData, forwardDate)
    vc.forwardState(forwardDate.endOfDay).spotFX(pair) shouldEqual
      vc.forwardFX(pair, forwardSpotDate)
  }

  test("fixings") {
    val wti = TestMarkets.NYMEX_WTI
    val wti1st = TestMarkets.NYMEX_WTI_1st_MONTH
    val marketDay = Day(2014, 12, 31).endOfDay
    val vc = TestMarketData.valuationContext(md = marketDay)
    vc.fixing(wti1st, Day(2012, 12, 2)) should be('left)
    vc.fixing(wti1st, Day(2016, 12, 2)) should be('left)
    vc.fixing(wti1st, Day(2014, 12, 2)) shouldEqual Right(Qty(66.88, USD/BBL))
    vc.fixing(wti1st, marketDay.day) shouldEqual Right(Qty(53.27, USD/BBL))

    // forwardState shouldn't change things as the day is still in the past
    vc.forwardState(marketDay.nextDay).fixing(wti1st, marketDay.day) shouldEqual Right(Qty(53.27, USD / BBL))

    // if we move past the original environment market day then we're going to have to proxy the fixing
    // with forward prices. for wti on the 1st of Jan that means Feb forward price.
    vc.forwardState(marketDay.nextDay).fixing(wti1st, marketDay.nextDay.day) shouldEqual
      vc.futuresPrice(wti, Month(2015, 2))

    intercept[RuntimeException] {
      vc.forwardState(marketDay.nextDay).fixing(wti1st, marketDay.nextDay.nextDay.day)
    }.getMessage shouldEqual
      "Looking for a fixing for a day that hasn't fixed yet: 2015-01-02/MarketDay(2015-01-01,TimeOfDay(false,true))"
  }

}
