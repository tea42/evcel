package evcel.quantity

import evcel.daterange.Month
import evcel.maths.LinearInterpolation
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import org.scalatest._
import evcel.quantity.utils.QuantityTestUtils._
import evcel.utils.EitherUtils._

class QtyTest extends FunSuite with Matchers {

  test("add") {
    Qty(1, USD) + Qty(3, USD) shouldEqual Qty(4, USD)
    Qty(1, USD) - Qty(3, USD) shouldEqual Qty(-2, USD)
    Qty(1, USD) - Qty(2, US_CENT) shouldEqual Qty(.98, USD)
    Qty(1, US_CENT) + Qty(1, USD) shouldEqual Qty(101, US_CENT)
  }

  test("add can fail") {
    intercept[RuntimeException] {
      Qty(1, USD) + Qty(1, BBL)
    }
    intercept[RuntimeException] {
      Qty(1, USD) - Qty(1, BBL)
    }
    intercept[RuntimeException] {
      Qty("1", USD) - Qty(1, BBL)
    }
  }

  test("test equality") {
    import scalaz.syntax.equal._

    (Qty("1", USD): Qty) assert_=== (Qty("1", USD) - Qty("1e-21", USD))
    (Qty("1", USD): Qty) assert_=/= (Qty("1", USD) - Qty("1e-19", USD))
  }

  test("test equality with new tolerance") {
    import scalaz.syntax.equal._

    intercept[RuntimeException] {
      implicit val tol: BigDecimal = BigDecimal("1e-25")
      (Qty("1", USD): Qty) assert_=== (Qty("1", USD) - Qty("1e-19", USD))
    }.getMessage shouldEqual "1 USD ≠ 0.9999999999999999999 USD"

    intercept[RuntimeException] {
      implicit val tol: BigDecimal = BigDecimal("1e-5")
      (Qty("1", USD): Qty) assert_=/= (Qty("1", USD) - Qty("1e-19", USD))
    }.getMessage shouldEqual "1.00000 USD == 1.00000 USD"
  }

  test("checked values") {
    Qty(7, USD).checkedDouble(USD) shouldEqual 7
    Qty(7, USD).checkedBDValue(USD) shouldEqual BigDecimal(7)

    intercept[IllegalArgumentException] {
      Qty(1, USD).checkedDouble(BBL)
    }
    intercept[IllegalArgumentException] {
      Qty(1, USD).checkedBDValue(BBL)
    }
  }

  test("add bigdecimal") {
    Qty(".1", USD) + Qty("3", USD) shouldEqual Qty("3.1", USD).ensuringFixedPoint
    Qty("1", USD) - Qty("3", USD) shouldEqual Qty("-2", USD).ensuringFixedPoint
    assert((Qty(".1", USD) + Qty("3", USD)).isFixedPoint === true)
    assert((Qty(".1", USD) + Qty(3.0, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty(3, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty("3", USD)).isFixedPoint === false)

    intercept[RuntimeException] {
      (Qty(.1, USD) + Qty("3", USD)).ensuringFixedPoint
    }
  }

  test("mult") {
    Qty(2, USD) * Qty(3, USD) shouldEqual Qty(6, USD * USD)
    Qty(2, USD) / Qty(4, BBL) shouldEqual Qty(.5, USD / BBL)
    Qty("2", USD) * Qty("3", USD) shouldEqual Qty("6", USD * USD).ensuringFixedPoint
    Qty("2", USD) / Qty("4", BBL) shouldEqual Qty(".5", USD / BBL).ensuringFixedPoint
    (Qty(1, USD) * Qty(4, GAL)) / Qty(4, BBL) should matchQty (Qty(1.0 / 42, USD))
    Qty(1, BBL) / Qty(1, GAL) should matchQty (Qty(42, SCALAR))
  }

  test("more mult") {
    Qty(1, USD/G) * Qty(1, MT) shouldEqual Qty(1e6, USD)
    Qty(1, USD/MT) * Qty(1, MT) shouldEqual Qty(1, USD)
  }

  test("percentage") {
    Qty(100, USD) / Qty(1, PERCENT) shouldEqual Qty(10000, USD)
    Qty(1, PERCENT) * Qty(100, USD) shouldEqual Qty(1, USD)
    Qty(100, USD) * Qty(1, PERCENT) shouldEqual Qty(1, USD)
    Qty(100, USD) * (Qty(1, PERCENT) * Qty(1, PERCENT)) shouldEqual Qty(.01, USD)

    Qty(100, SCALAR) / Qty(1, PERCENT) shouldEqual Qty(10000, SCALAR)
    Qty(100, SCALAR) * Qty(1, PERCENT) shouldEqual Qty(1, SCALAR)

    Qty(10, PERCENT) * Qty(1, SCALAR) shouldEqual Qty(10, PERCENT)
    Qty(10, PERCENT) * Qty(10, PERCENT) * Qty(1, SCALAR) shouldEqual Qty(1, PERCENT)
    Qty(100, PERCENT) / Qty(1, SCALAR) shouldEqual Qty(100, PERCENT)
    Qty(100, PERCENT) / Qty(2, SCALAR) shouldEqual Qty(50, PERCENT)
    (Qty(100, PERCENT) * Qty(100, PERCENT)) / Qty(2, SCALAR) shouldEqual Qty(50, PERCENT)

    Qty(100, PERCENT) / Qty(1, PERCENT) shouldEqual Qty(10000, PERCENT)
  }

  test("equals and hashcode") {
    Qty(2, USD) shouldEqual Qty(2, USD)
    Qty(2, USD).hashCode() shouldEqual Qty(2, USD).hashCode()
    Qty(2, USD) shouldNot be(Qty(2, BBL))
    Qty(2, USD) shouldNot be(Qty(2.1, USD))
    Qty("2", USD) shouldNot be(Qty(2.1, USD))
    Qty("2", USD) shouldNot be(Qty("2.1", USD))
    Qty("2", USD).hashCode() shouldEqual Qty("2", USD).hashCode()
    Qty(2, USD) shouldNot be(2.0)
    Qty("2", USD) shouldNot be(2.0)
    Qty("2", PERCENT) shouldEqual (Qty(4.0, PERCENT) * Qty(50.0, PERCENT))
    Qty("2", PERCENT) shouldNot be(2.0)
    Qty("2", PERCENT) shouldNot be(Qty(2.0, SCALAR))
  }

  test("to string") {
    Qty(2, USD).toString shouldEqual "2.0 USD"
    Qty(2, USD / BBL).toString shouldEqual "2.0 USD/bbl"
    Qty(2, USD * USD / BBL).toString shouldEqual "2.0 USD^2/bbl"
    Qty(-2, USD * USD / (BBL * BBL)).toString shouldEqual "-2.0 USD^2/bbl^2"
    (Qty(100, PERCENT) * Qty(100, PERCENT)).toString shouldEqual "100.0 %"
    (Qty(100, PERCENT) * Qty(1, PERCENT)).toString shouldEqual "1.0 %"
    (Qty(100, PERCENT) / Qty(1, PERCENT)).toString shouldEqual "10000.0 %"
  }

  test("format") {
    Qty(2, USD).toFormattedString(2) shouldEqual "2.00 USD"
    Qty(2, USD).toFormattedString(0) shouldEqual "2 USD"
    Qty(1.99, USD).toFormattedString(2) shouldEqual "1.99 USD"

    // this test fails in Java 8. it gives 1.9 USD
    // https://github.com/PROSPricing/jdk8patch-halfupround/
//    Qty("1.99", USD).toFormattedString(1) shouldEqual "2.0 USD"

    Qty(1.99, USD).toFormattedString(0) shouldEqual "2 USD"
//    decimal format gives up after 16 decimal places.
//    so after 16 decimal places we just print out the string for the bigdecimal and the uom.
    (Qty("1", USD) - Qty("1e-20", USD)).toFormattedString(300) shouldEqual "0.99999999999999999999 USD"
  }

  test("implicits") {
    BigDecimal("2").ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    BigDecimal(2).ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    Qty(4, USD) * 4 shouldEqual Qty(16, USD)

    4(USD) shouldEqual Qty(4, USD)
  }

  test("conversion") {
    1(USD) in US_CENT shouldEqual Right(100(US_CENT))
    (1(USD) in US_CENT).flatMap(_ in USD) shouldEqual Right(1(USD))
    100(US_CENT) in USD shouldEqual Right(1(USD))
    1(BBL) in GAL shouldEqual Right(42(GAL))
    val conv = new QtyConversions(Map((MT, BBL) -> 7.45))
    1(MT) in BBL should be ('left)
    1(MT).in(BBL, conv) shouldEqual Right(7.45(BBL))
  }

  test("abs") {
    1(USD).abs shouldEqual 1(USD)
    Qty("-1", USD).abs shouldEqual 1(USD)
  }

  test("compare") {
    import Qty._
    (1(USD) < 2.0(USD)) shouldBe true
    (2(USD) > 1(USD)) shouldBe true
    List(1(SCALAR), Qty.NULL, 0(BBL)).foreach{
      q => 
        intercept[RuntimeException] {
          2(USD) > q
        }
    }
    intercept[RuntimeException]{
      2(USD) > 1
    }

    (2(USD) > 0) shouldBe true
    (2(USD) >= 0) shouldBe true
    (0.0(USD) >= 0) shouldBe true
    (2.0(USD) < 0) shouldBe false
  }

  test("match interpolation request for type class conversion") {
    val months = Vector(Month(2015, 1), Month(2015, 2), Month(2015, 4), Month(2015, 5))
    val prices: Vector[Qty] = Vector(10.0, 20.0, 40.0, 50.0).map(Qty(_, USD/BBL))
    LinearInterpolation.interpolate(months, prices, Month(2015, 3)) shouldBe Qty(30.0, USD/BBL)
  }
}
