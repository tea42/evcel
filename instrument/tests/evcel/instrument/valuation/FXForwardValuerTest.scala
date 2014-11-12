package evcel.instrument.valuation

import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, SpotPriceIdentifier}
import evcel.instrument.{Cash, CommoditySwap}
import evcel.instrument.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.referencedata.market.FXPair

class FXForwardValuerTest extends ValuationTest {
  implicit val tol: BigDecimal = BigDecimal("1e-7")
  import scalaz.syntax.equal._
  import evcel.quantity.utils.QuantityTestUtils._

  test("fx forward") {
    val volumes = Qty(1013, GBP) :: Qty(-1013, GBP) :: Nil
    for (volume <- volumes) {
      val forward = fxForward(volume)
      val vc = createVC()

      val valuationCCY = (forward.volume * forward.strike).uom
      val d1 = vc.discountRate(forward.strike.uom.numerator, forward.delivery)
      val d2 = vc.discountRate(forward.strike.uom.denominator, forward.delivery)
      val todayFX = vc.todayFX(FXPair(forward.volume.uom, valuationCCY))
      val mtm = todayFX * (forward.volume * d2.toQty) + (-forward.volume * forward.strike * d1.toQty)

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
      val d1 = vc.discountRate(forward.strike.uom.numerator, forward.delivery)
      val d2 = vc.discountRate(forward.strike.uom.denominator, forward.delivery)
      val todayFX = vc.todayFX(FXPair(forward.volume.uom, valuationCCY))
      val mtm = todayFX * (forward.volume * d2.toQty) + (-forward.volume * forward.strike * d1.toQty)

      val vcEur = vc.withValuationCCY(EUR)
      forward.mtm(vcEur) shouldEqual mtm
      forward.copy(strike = forward.strike.invert).mtm(vcEur) shouldEqual mtm

      forward.mtm(vc) assert_=== mtm * vc.todayFX(FXPair(EUR, USD))
      forward.copy(strike = forward.strike.invert).mtm(vc) assert_=== mtm * vc.todayFX(FXPair(EUR, USD))
    }
  }

}
