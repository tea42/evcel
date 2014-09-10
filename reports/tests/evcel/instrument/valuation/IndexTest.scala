package evcel.instrument.valuation

import org.scalatest.{ShouldMatchers, FunSuite}

class IndexTest extends FunSuite with ShouldMatchers {

  test("test index parsing") {
    val indexes = List(
      new FuturesFrontPeriodIndex("Nymex WTI", 1, 0),
      new FuturesFrontPeriodIndex("Nymex WTI", 2, 3),
      new FuturesFrontPeriodIndex("Nymex WTI", 2, 0),
      new FuturesFrontPeriodIndex("Nymex WTI", 1, 3))
    indexes.foreach(i => FuturesFrontPeriodIndex.unapply(i.toString) shouldEqual Some(i))
    FuturesFrontPeriodIndex.unapply("Nymex WTI") shouldEqual None
  }

  test("test to string") {
    new FuturesFrontPeriodIndex("Nymex WTI", 1, 0).toString shouldEqual "Nymex WTI nearby 1"
    new FuturesFrontPeriodIndex("Nymex WTI", 2, 3).toString shouldEqual "Nymex WTI nearby 2 roll 3"
    new FuturesFrontPeriodIndex("Nymex WTI", 2, 0).toString shouldEqual "Nymex WTI nearby 2"
    new FuturesFrontPeriodIndex("Nymex WTI", 1, 3).toString shouldEqual "Nymex WTI nearby 1 roll 3"
  }
}
