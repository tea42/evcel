package evcel.valuation

import evcel.curve.RichFuturesMarket
import evcel.daterange.PeriodLabel
import evcel.valuation.Valuer._
import evcel.quantity.UOM._
import scala.collection.immutable.Nil
import evcel.quantity.Qty
import scala.util.Right

class FuturePositionTest extends ValuationTest {

  test("future position") {
    val vc = createVC()
    val future = createFuture()
    val market = RichFuturesMarket(vc.refData, wti).R


    future.positions(vc).R shouldEqual
      Map(market.unitHedge(future.period) -> Right(future.volume.doubleValue))
  }

  test("future position - per day valuation") {
    val vc = createVC()
    val market = RichFuturesMarket(vc.refData, nbp).R
    val future = createFutureNBP(nbp, oct)
    future.volume.uom shouldBe THM/DAY
    val positions = future.positions(vc).R
    positions.keySet shouldEqual(Set(market.unitHedge(oct)))
    positions.head._2.right.get should be (future.volume.doubleValue +- 1e-9)
  }
}
