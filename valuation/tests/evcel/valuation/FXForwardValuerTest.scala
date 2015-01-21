package evcel.valuation

import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, SpotPriceIdentifier}
import evcel.instrument.{Cash, CommoditySwap}
import evcel.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.referencedata.market.FXPair
import scalaz.syntax.equal._
import evcel.utils.EitherUtils._
import scala.math.BigDecimal
import scala.collection.immutable.Nil
import evcel.quantity.utils.QuantityTestUtils._
import scala.language.reflectiveCalls
import evcel.utils.EitherTestPimps

class FXForwardValuerTest extends ValuationTest with EitherTestPimps{
  implicit val tol: BigDecimal = BigDecimal("1e-7")
  import scalaz.syntax.equal._
  import evcel.quantity.utils.QuantityTestUtils._

  test("fx forward") {
    val volumes = Qty(1013, GBP) :: Qty(-1013, GBP) :: Nil
    for (volume <- volumes) {
      val forward = fxForward(volume)
      val vc = createVC()

      val valuationCCY = (forward.volume * forward.strike).uom
      val mtm = for {
        d1 <- vc.discountRate(forward.strike.uom.numerator, forward.delivery)
        d2 <- vc.discountRate(forward.strike.uom.denominator, forward.delivery)
        todayFX <- vc.todayFX(FXPair(forward.volume.uom, valuationCCY))
      } yield {
        todayFX * (forward.volume * d2) + (-forward.volume * forward.strike * d1)
      }

      forward.mtm(vc) shouldEqual mtm
      forward.copy(strike = forward.strike.invert).mtm(vc) shouldEqual mtm
    }
  }

  test("fx forward cross") {
    val volumes = Qty(1013, GBP) :: Qty(-1013, GBP) :: Nil
    for (volume <- volumes) {
      val forward = fxForward(volume, strike = Qty("1.2", EUR / GBP))
      val vc = createVC()

      val valuationCCY = (forward.volume * forward.strike).uom
      val mtm = {
        val d1 = vc.discountRate(forward.strike.uom.numerator, forward.delivery).R
        val d2 = vc.discountRate(forward.strike.uom.denominator, forward.delivery).R
        val todayFX = vc.todayFX(FXPair(forward.volume.uom, valuationCCY)).R
        todayFX * (forward.volume * d2) + (-forward.volume * forward.strike * d1)
      }

      val vcEur = vc.withValuationCCY(EUR)
      forward.mtm(vcEur).R shouldEqual mtm
      forward.copy(strike = forward.strike.invert).mtm(vcEur).R shouldEqual mtm

      forward.mtm(vc).R should be (mtm * vc.todayFX(FXPair(EUR, USD)).R +- 1e-9)
      forward.copy(strike = forward.strike.invert).mtm(vc).R should be (mtm * vc.todayFX(FXPair(EUR, USD)).R +- 1e-9)
    }
  }

}
