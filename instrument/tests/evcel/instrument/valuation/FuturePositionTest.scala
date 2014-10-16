package evcel.instrument.valuation

import evcel.daterange.PeriodLabel
import evcel.instrument.valuation.Valuer._

class FuturePositionTest extends ValuationTest {

  test("future position") {
    val vc = createVC()
    val future = createFuture()

    future.positions(vc) shouldEqual
      FutureHedgeInfo(future.market, PeriodLabel(future.delivery), future.volume) :: Nil
  }
}
