package com.evcel.core.qty

import org.scalatest._
import com.evcel.core.qty.UOM._

class QtyTest extends FunSuite with ShouldMatchers {

  test("test add") {
    Qty(1, USD) + Qty(3, USD) shouldEqual Qty(4, USD)
    Qty(1, USD) - Qty(3, USD) shouldEqual Qty(-2, USD)
    Qty(1, USD) - Qty(2, US_CENT) shouldEqual Qty(.98, USD)
    Qty(1, US_CENT) + Qty(1, USD) shouldEqual Qty(101, US_CENT)
  }

  test("test add bigdecimal") {
    Qty(".1", USD) + Qty("3", USD) shouldEqual Qty("3.1", USD)
    Qty("1", USD) - Qty("3", USD) shouldEqual Qty("-2", USD)
    assert((Qty(".1", USD) + Qty("3", USD)).isFixedPoint === true)
    assert((Qty(".1", USD) + Qty(3, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty(3, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty("3", USD)).isFixedPoint === false)
  }

  test("test mult") {
    Qty(2, USD) * Qty(3, USD) shouldEqual Qty(6, USD*USD)
    Qty(2, USD) / Qty(4, BBL) shouldEqual Qty(.5, USD/BBL)
    (Qty(1, USD) * Qty(4, GAL)) / Qty(4, BBL) shouldEqual Qty(42, USD)
  }
}
