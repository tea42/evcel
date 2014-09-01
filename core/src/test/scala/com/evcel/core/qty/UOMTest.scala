package com.evcel.core.qty

import org.scalatest.{ ShouldMatchers, FunSuite }
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
      case (USD, bd) => bd shouldEqual 100 * BigDecimal(42)
    }
    USD.mult(BBL).toString shouldEqual "(USDBBL,1.0)"
    USD.mult(USD).toString shouldEqual "(USD^2,1.0)"
    USD.div(US_CENT).toString shouldEqual "(,1.0E+2)"
    USD.mult(USD)._1.div(US_CENT).toString shouldEqual "(USD,1.0E+2)"
  }

  test("toString") {
    NULL.toString shouldEqual "NULL"
    SCALAR.toString shouldEqual ""
    USD.toString shouldEqual "USD"
    (USD * USD).toString shouldEqual "USD^2"
    (BBL / (USD * USD)).toString shouldEqual "BBL/USD^2"
    (SCALAR / USD).toString shouldEqual "USD^-1"
    (SCALAR / (USD * USD)).toString shouldEqual "USD^-2"
    (USD / SCALAR).toString shouldEqual "USD"
  }

  test("test simple conversion") {
    USD in USD shouldEqual Some(BigDecimal(1.0))
    USD in US_CENT shouldEqual Some(BigDecimal(100))
    US_CENT in USD shouldEqual Some(BigDecimal(.01))

    (BBL / USD).in(GAL / USD) shouldEqual Some(BigDecimal(42.0))
    (USD / BBL).in(USD / GAL) shouldEqual Some(1 / BigDecimal(42.0))
    (USD / (BBL * BBL)).in(USD / (GAL * GAL)) shouldEqual Some(1 / BigDecimal(42.0 * 42.0))

    intercept[RuntimeException] {
      BBL * BBL in GAL
    }
    intercept[RuntimeException] {
      USD / BBL in USD
    }
    intercept[RuntimeException] {
      USD / (BBL * BBL) in USD / GAL
    }
  }

  test("test custom conversion") {
    val conv = new QtyConversions(Map((MT, BBL) -> 7.45))
    BBL in MT shouldEqual None
    BBL in (MT, Some(conv)) shouldEqual Some(1 / BigDecimal(7.45))
    MT in (BBL, Some(conv)) shouldEqual Some(BigDecimal(7.45))
  }
}
