package evcel.instrument.valuation
import evcel.instrument.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.quantity.utils.QuantityTestUtils._

class FutureValuationTest extends ValuationTest {

  test("future - per day valuation") {
    val vc = createVC().withValuationCCY(GBP)
    val future = createFutureNBP(nbp, oct)

    val F = vc.futuresPrice(nbp, oct)
    val K = future.strike
    val numDays = oct.size

    val mtm = (F - K) * (future.volume * Qty(numDays, DAY))

    future.mtm(vc).uom shouldBe GBP
    future.mtm(vc) should matchQty(mtm.inBaseCcy)
  }

}
