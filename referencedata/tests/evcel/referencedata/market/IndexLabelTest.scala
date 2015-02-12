package evcel.referencedata.market

import org.scalatest.{FunSuite, Matchers}

class IndexLabelTest extends FunSuite with Matchers {

  test("test index parsing") {
    val indexes = List(
      new FuturesFrontPeriodIndexLabel("Nymex WTI", 1, 0),
      new FuturesFrontPeriodIndexLabel("Nymex WTI", 2, 3),
      new FuturesFrontPeriodIndexLabel("Nymex WTI", 2, 0),
      new FuturesFrontPeriodIndexLabel("Nymex WTI", 1, 3))
    indexes.foreach(i => IndexLabel.parse(i.toString) shouldEqual i)
  }

  test("test to string") {
    new FuturesFrontPeriodIndexLabel("Nymex WTI", 1, 0).toString shouldEqual "Nymex WTI nearby 1"
    new FuturesFrontPeriodIndexLabel("Nymex WTI", 2, 3).toString shouldEqual "Nymex WTI nearby 2 roll 3"
    new FuturesFrontPeriodIndexLabel("Nymex WTI", 2, 0).toString shouldEqual "Nymex WTI nearby 2"
    new FuturesFrontPeriodIndexLabel("Nymex WTI", 1, 3).toString shouldEqual "Nymex WTI nearby 1 roll 3"
  }
}
