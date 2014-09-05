package evcel.curve

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.curve.environment.MarketDay
import evcel.curve.environment.TimeOfDay
import evcel.daterange.DateRangeSugar._
import evcel.curve.curves.FuturesPriceIdentifier
import scala.language.reflectiveCalls

class EnvironmentTests extends FunSpec with Matchers {
  describe("When shifting a price") {
    val wti = "WTI"
    val rbob = "RBOB"
    val wtiFlatPrice = 100.0(USD / MT)
    val rbobFlatPrice = 10.0(US_CENT / GAL)
    val env = UnitTestingEnvironment(
      MarketDay(10 / Aug / 2014, TimeOfDay.end),
      {
        case FuturesPriceIdentifier(`wti`, _) => wtiFlatPrice
        case FuturesPriceIdentifier(`rbob`, _) => rbobFlatPrice
      }
    )
    it("Should perturb the price we want") {
      val sep14 = Sep / 2014
      val perturbed = env.shiftFuturesPrice("WTI", Sep / 2014, 1.0(USD / MT))
      perturbed.futuresPrice(wti, sep14).doubleValue should equal(101.0 +- 1e-9)
      perturbed.futuresPrice(rbob, sep14).doubleValue should equal(rbobFlatPrice.doubleValue +- 1e-9)
      perturbed.futuresPrice(wti, sep14.next).doubleValue should equal(100.0 +- 1e-9)
    }
  }
}
