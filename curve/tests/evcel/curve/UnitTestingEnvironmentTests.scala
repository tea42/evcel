package evcel.curve

import org.scalatest.FunSpec
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata.FuturesPriceData
import scala.language.reflectiveCalls
import org.scalatest.Matchers
import evcel.curve.marketdata.ZeroRateData
import evcel.curve.marketdata.Act365
import evcel.quantity.Percent
import scala.math._
import evcel.curve.marketdata.FuturesVolData

class UnitTestingEnvironmentTests extends FunSpec with Matchers{
  describe("UnitTestingEnvironment"){
    it("Should work"){
      val env = UnitTestingEnvironment.fromMarketData(
        (10 / Sep/ 2014).endOfDay,

        "Nymex WTI"          -> FuturesPriceData(Sep / 2014 -> Qty("100", USD/MT)),
        "PORK BELLIES" -> FuturesPriceData(Sep / 2014 -> Qty("123", USD/MT)),

        USD -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("5"))),

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
    }
  }
}
