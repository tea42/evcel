package evcel.curve

import evcel.curve.curves.PriceFixingIdentifier
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.quantity.{Percent, Qty}
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.referencedata.market.{TestMarkets, FuturesFrontPeriodIndexLabel, FuturesFrontPeriodIndex, FXPair}
import org.scalatest._
import scala.language.reflectiveCalls
import scala.math._
import evcel.daterange._
import evcel.utils.EitherTestPimps


class UnitTestingEnvironmentTests extends MarketDataTest with FunSpecLike with Matchers with
EitherValues with EitherTestPimps {
  describe("UnitTestingEnvironment") {
    it("Should work") {
      val env = UnitTestingEnvironment.fromMarketData(
        (10 / Sep / 2014).endOfDay,

        "Nymex WTI" -> futuresPrices(Sep / 2014 -> Qty("100", USD / MT)),
        "PORK BELLIES" -> futuresPrices(Sep / 2014 -> Qty("123", USD / MT)),

        USD -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("5"))),
        EUR -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("3"))),

        (USD, GBP) -> SpotFXData(Qty("1.6", USD / GBP)),
        (USD, EUR) -> SpotFXData(Qty("1.25", USD / EUR)),

        "Nymex WTI" -> FuturesVolData(List((Sep / 2014, Percent("20"), Nil)))
      )
      val wti = env.futuresMarket("Nymex WTI").R
      val porkBellies = env.futuresMarket("PORK BELLIES").R
      env.futuresPrice(wti, Sep / 2014).R shouldEqual Qty("100", USD / MT)
      env.futuresPrice(wti, Jul / 2014) should be('left)
      env.futuresPrice(porkBellies, Sep / 2014).R shouldEqual Qty("123", USD / MT)

      env.discountRate(USD, 10 / Sep / 2015).R.doubleValue should be(exp(-0.05) +- 1e-5)

      env.discountRate(GBP, 10 / Sep / 2015) should be('left)

      env.futuresVol(wti, Sep / 2014, Qty("110", USD / MT))

      env.spotFX(FXPair(USD, GBP)).R shouldEqual Qty("0.625", GBP / USD)
      env.spotFX(FXPair(GBP, USD)).R shouldEqual Qty("1.6", USD / GBP)

      env.spotFX(FXPair(CAD, USD)) should be('left)

      env.spotFX(FXPair(GBP, EUR)).R shouldEqual (Qty("1.25", USD / EUR).invert * Qty("1.6", USD / GBP))
      env.spotFX(FXPair(EUR, GBP)).R shouldEqual env.spotFX(FXPair(GBP, EUR)).R.invert

      val forwardDay = 10 / Mar / 2015
      val eurusd = FXPair(EUR, USD)
      env.forwardFX(eurusd, forwardDay).R shouldEqual (
        env.todayFX(eurusd).R *
          // exp^((rd - rf) * T)
          math.exp((0.05 - 0.03) * Act365.timeBetween(env.marketDay.day, forwardDay)).toQty
        )
    }
  }

  it("should load some sample data in csv files") {
    val vc = TestMarketData.valuationContext()
    val wti1st = TestMarkets.NYMEX_WTI_1st_MONTH
    val wti = TestMarkets.NYMEX_WTI
    val singgo = TestMarkets.SING_GO

    vc.fixing(wti1st, Day(2014, 7, 1)) should be('left)
    vc.fixing(wti1st, Day(2014, 8, 1)) shouldEqual Right(Qty("97.88", USD / BBL))
    vc.fixing(wti1st, Day(2014, 12, 31)) shouldEqual Right(Qty("53.27", USD / BBL))

    vc.fixing(singgo, Day(2014, 7, 1)) should be('left)
    vc.fixing(singgo, Day(2014, 8, 1)) shouldEqual Right(Qty("118.90", USD / MT))
    vc.fixing(singgo, Day(2014, 12, 31)) shouldEqual Right(Qty("70.51", USD / MT))

    vc.futuresPrice(wti, Month(2012, 10)) should be('left)
    vc.futuresPrice(wti, Month(2015, 1)) shouldEqual Right(Qty("49.30", USD / BBL))
    vc.futuresPrice(wti, Month(2019, 12)) shouldEqual Right(Qty("65.25", USD / BBL))
  }
}
