package evcel.instrument.valuation

import evcel.daterange.PeriodLabel
import evcel.instrument.valuation.Valuer._
import evcel.quantity.UOM._

class FuturePositionTest extends ValuationTest {

  test("future position") {
    val vc = createVC()
    val future = createFuture()

    future.positions(vc) shouldEqual
      HedgeInfo(future.market, PeriodLabel(future.period), future.volume) :: Nil
  }

  test("future position - per day valuation") {
    val vc = createVC()
    val future = createFutureNBP(nbp, oct)
    future.volume.uom shouldBe THM/DAY
    future.positions(vc) shouldEqual
      HedgeInfo(future.market, PeriodLabel(future.period), future.volume) :: Nil
  }
}
