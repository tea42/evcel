package evcel.instrument.valuation

import evcel.quantity.Qty
import evcel.quantity.UOM._

class PositionTest extends ValuationTest {

  test("simple test svd"){
    val vc = createVC()
    val obDays = SwapLikeValuer(vc, createSwap()).observationDays
    val swap = createSwap(volume = Qty((obDays.size * 13).toString, BBL))
    val observationDays = SwapLikeValuer(vc, swap).observationDays
    val hedges = observationDays.map(
      d => createSwap(period = d, volume = Qty("1", BBL))
    )
    val scaled = Position.scaleHedges(vc, List(swap), hedges)
    val expected = hedges.map(h => h -> Qty("13", BBL))
    scaled.toList.map{case (h, v) => h -> v.round(9)} shouldEqual expected.toList
  }
}
