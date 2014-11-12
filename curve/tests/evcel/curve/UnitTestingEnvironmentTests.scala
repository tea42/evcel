package evcel.curve

import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.quantity.{Percent, Qty}
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.referencedata.market.FXPair
import org.scalatest.{FunSpecLike, Matchers, ShouldMatchers}

import scala.language.reflectiveCalls
import scala.math._

class UnitTestingEnvironmentTests extends MarketDataTest with FunSpecLike with Matchers with ShouldMatchers{
  describe("UnitTestingEnvironment"){
    it("Should work"){
      val env = UnitTestingEnvironment.fromMarketData(
        (10 / Sep/ 2014).endOfDay,

        "Nymex WTI"          -> futuresPrices(Sep / 2014 -> Qty("100", USD/MT)),
        "PORK BELLIES" -> futuresPrices(Sep / 2014 -> Qty("123", USD/MT)),

        USD -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("5"))),
        EUR -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("3"))),

        (USD, GBP) -> SpotFXData(Qty("1.6", USD/GBP)),
        (USD, EUR) -> SpotFXData(Qty("1.25", USD/EUR)),

        "Nymex WTI"          -> FuturesVolData(List((Sep / 2014, Percent("20"), Nil)))
      )
      env.futuresPrice("Nymex WTI", Sep/2014) shouldEqual Qty("100", USD/MT)
      intercept[RuntimeException]{
        env.futuresPrice("Nymex WTI", Jul/2014) 
      }
      env.futuresPrice("PORK BELLIES", Sep/2014) shouldEqual Qty("123", USD/MT)

      env.discountRate(USD, 10 / Sep / 2015) should be (exp(-0.05) +- 1e-5)
      intercept[RuntimeException]{
        env.discountRate(GBP, 10 / Sep / 2015)
      }

      env.futuresVol("Nymex WTI", Sep / 2014, Qty("110", USD/MT))

      env.spotFX(FXPair(USD, GBP)) shouldEqual Qty("0.625", GBP/USD)
      env.spotFX(FXPair(GBP, USD)) shouldEqual Qty("1.6", USD/GBP)

      intercept[RuntimeException] {
        env.spotFX(FXPair(CAD, USD))
      }.getMessage shouldEqual "No market data provided for BaseFXRateKey(USD,CAD)"

      env.spotFX(FXPair(GBP, EUR)) shouldEqual (Qty("1.25", USD/EUR).invert * Qty("1.6", USD/GBP))
      env.spotFX(FXPair(EUR, GBP)) shouldEqual env.spotFX(FXPair(GBP, EUR)).invert

      val forwardDay = 10 / Mar / 2015
      val eurusd = FXPair(EUR, USD)
      env.forwardFX(eurusd, forwardDay) shouldEqual (
        env.todayFX(eurusd) *
          // exp^((rd - rf) * T)
          math.exp((0.05 - 0.03) * Act365.timeBetween(env.marketDay.day, forwardDay)).toQty
        )
    }
  }
}
