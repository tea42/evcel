package evcel.quantity

import org.scalatest.{ Matchers, FunSuite }
import UOM._

class UOMTest extends FunSuite with Matchers {

  test("addition/subtraction") {
    for (add <- List(true, false)) {
      def test(uom1: UOM, uom2: UOM) = if (add)
        uom1.add(uom2)
      else
        uom1.subtract(uom2)
      test(USD, USD) shouldEqual Right(None)
      test(USD, US_CENT) shouldEqual Right(Some(BigDecimal(100)))
      test(US_CENT, USD) shouldEqual Right(Some(BigDecimal(.01)))
      test(US_CENT, BBL) shouldEqual Left("Can't add ¢ and BBL")
      test(BBL, USD) shouldEqual Left("Can't add BBL and USD")
    }
  }

  test("simple multiplication") {
    (USD * USD * BBL).div(US_CENT * GAL) match {
      case (USD, bd) => bd shouldEqual 100 * BigDecimal(42)
    }
    USD.mult(BBL).toString shouldEqual "(BBLUSD,1.0)"
    USD.mult(USD).toString shouldEqual "(USD^2,1.0)"
    USD.div(US_CENT).toString shouldEqual "(,1.0E+2)"
    USD.mult(USD)._1.div(US_CENT).toString shouldEqual "(USD,1.0E+2)"
    (USD / MT).mult(MT) match {
      case (USD, bd) => bd shouldEqual BigDecimal(1.0)
    }
  }

  test("asPrimeMap") {
    val first = USD / MT
    val second = MT
    val newUOM = UOM(first.dimension * second.dimension, first.secondary * second.secondary)
    newUOM.asPrimeMap shouldEqual Map(USD.secondary.num -> 1, MT.secondary.num -> 0)
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

  test("numerator") {
    (USD/BBL).numerator shouldEqual USD
    (BBL/USD).numerator shouldEqual BBL
    (BBL*BBL/USD).numerator shouldEqual BBL*BBL
    (BBL*GAL/USD).numerator shouldEqual BBL*GAL
    SCALAR.numerator shouldEqual SCALAR
  }

  test("denominator") {
    (USD/BBL).denominator shouldEqual BBL
    (BBL/USD).denominator shouldEqual USD
    (BBL*BBL/USD).denominator shouldEqual USD
    (BBL/(USD*USD)).denominator shouldEqual USD*USD
    (BBL/(G*USD)).denominator shouldEqual G*USD
    (SCALAR/USD).denominator shouldEqual USD
  }

  test("pow") {
    USD.pow(0) shouldEqual SCALAR
    USD.pow(1) shouldEqual USD
    USD.pow(2) shouldEqual USD*USD
    USD.pow(-1) shouldEqual SCALAR/USD
    USD.pow(-2) shouldEqual SCALAR/(USD*USD)
  }
}
