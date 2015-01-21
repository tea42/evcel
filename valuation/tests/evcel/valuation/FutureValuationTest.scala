package evcel.valuation

import evcel.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.quantity.utils.QuantityTestUtils._
import evcel.utils.EitherTestPimps

class FutureValuationTest extends ValuationTest with EitherTestPimps{

  test("future - per day valuation") {
    val vc = createVC().withValuationCCY(GBP)
    val nbpMarket = vc.futuresMarket(nbp).R
    val future = createFutureNBP(nbp, oct)

    val F = vc.futuresPrice(nbpMarket, oct).R
    val K = future.strike
    val numDays = oct.size

    val mtm = (F - K) * (future.volume * Qty(numDays, DAY))

    future.mtm(vc).R.uom shouldBe GBP
    future.mtm(vc).R should matchQty(mtm.inBaseCcy.R)
  }

}
