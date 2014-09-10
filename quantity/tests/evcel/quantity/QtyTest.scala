package evcel.quantity

import org.scalatest._
import UOM._
import Qty._

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
    assert((Qty(".1", USD) + Qty(3, USD)).isFixedPoint === false)
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
    (Qty(1, USD) * Qty(4, GAL)) / Qty(4, BBL) shouldEqual Qty(1.0 / 42, USD)
    Qty(1, BBL) / Qty(1, GAL) shouldEqual Qty(42, SCALAR)
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
    Qty(2, USD / BBL).toString shouldEqual "2.0 USD/BBL"
    Qty(2, USD * USD / BBL).toString shouldEqual "2.0 USD^2/BBL"
    Qty(-2, USD * USD / (BBL * BBL)).toString shouldEqual "-2.0 USD^2/BBL^2"
    (Qty(100, PERCENT) * Qty(100, PERCENT)).toString shouldEqual "100.0 %"
    (Qty(100, PERCENT) * Qty(1, PERCENT)).toString shouldEqual "1.0 %"
    (Qty(100, PERCENT) / Qty(1, PERCENT)).toString shouldEqual "10000.0 %"
  }

  test("implicits") {
    BigDecimal("2").ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    BigDecimal(2).ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    Qty(4, USD) * 4 shouldEqual Qty(16, USD)

    4(USD) shouldEqual Qty(4, USD)
  }

  test("conversion") {
    1(USD) in US_CENT shouldEqual Some(100(US_CENT))
    (1(USD) in US_CENT).flatMap(_ in USD) shouldEqual Some(1(USD))
    100(US_CENT) in USD shouldEqual Some(1(USD))
    1(BBL) in GAL shouldEqual Some(42(GAL))
    val conv = new QtyConversions(Map((MT, BBL) -> 7.45))
    1(MT) in BBL shouldEqual None
    1(MT).in(BBL, Some(conv)) shouldEqual Some(7.45(BBL))
  }

  test("abs") {
    1(USD).abs shouldEqual 1(USD)
    Qty("-1", USD).abs shouldEqual 1(USD)
  }

  test("compare") {
    1(USD) shouldBe < (2(USD))
    2(USD) shouldBe > (1(USD))
    2(USD) shouldBe > (1(SCALAR))
    2(USD) shouldBe > (Qty.NULL: Qty)
    intercept[RuntimeException] {
      2(USD) shouldBe > (0(BBL))
    }
  }
}
