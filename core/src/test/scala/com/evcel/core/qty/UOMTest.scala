package com.evcel.core.qty

import org.scalatest.{ShouldMatchers, FunSuite}
import com.evcel.core.qty.UOM._

class UOMTest extends FunSuite with ShouldMatchers {

  test("addition/subtraction") {
    for (add <- List(true, false)) {
      def test(uom1: UOM, uom2: UOM) = if (add)
        uom1.add(uom2)
      else
        uom1.subtract(uom2)
      test(USD, USD) shouldEqual Some(BigDecimal(1))
      test(USD, US_CENT) shouldEqual Some(BigDecimal(100))
      test(US_CENT, USD) shouldEqual Some(BigDecimal(.01))
      test(US_CENT, BBL) shouldEqual None
      test(BBL, USD) shouldEqual None
    }
  }

  test("simple multiplication") {
    (USD * USD * BBL).div(US_CENT * GAL) match {
      case (USD, bd) => bd shouldEqual 100 * BigDecimal(1.0) / 42
    }
    USD.mult(BBL).toString shouldEqual "(USDBBL,1.0)"
    USD.mult(USD).toString shouldEqual "(USDUSD,1.0)"
    USD.div(US_CENT).toString shouldEqual "(,1.0E+2)"
    USD.mult(USD)._1.div(US_CENT).toString shouldEqual "(USD,1.0E+2)"
  }
}
