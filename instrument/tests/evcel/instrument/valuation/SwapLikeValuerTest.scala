package evcel.instrument.valuation

import evcel.curve.curves.{SpotPriceIdentifier, FuturesPriceIdentifier}
import evcel.daterange.DateRangeSugar.Oct
import evcel.instrument.CommoditySwap
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.quantity.utils.QuantityTestUtils._

class SwapLikeValuerTest extends ValuationTest {
  val K = Qty("100", USD / BBL)
  val V = Qty("1000", BBL)
  val oct = Oct / 14
  val nov = oct.next
  val dec = nov.next
  val jan = dec.next
  val wti = "Nymex WTI"
  val sing = "Singapore Gasoil 0.05"

  test("swap on futures contract index") {
    val swap = new CommoditySwap(wti, oct, K, V)
    val vc = createVC()
    val mtm = valuer.value(vc, swap)
    mtm shouldEqual (vc.futuresPrice("Nymex WTI", oct) - K) * V
    valuer.keys(vc, swap) shouldEqual Set(FuturesPriceIdentifier(wti, oct))
  }

  test("swap on spot market") {
    val K = Qty("100", USD / MT)
    val V = Qty("1000", MT)
    val swap = new CommoditySwap(sing, oct, K, V)
    val vc = createVC()
    val obDays = SpotMarketIndex(sing).observationDays(vc, oct)
    val F = Qty.average(obDays.map(d => vc.spotPrice(sing, d)))
    val mtm = valuer.value(vc, swap)
    mtm shouldEqual (F - K) * V
    valuer.keys(vc, swap) shouldEqual obDays.map(SpotPriceIdentifier(sing, _)).toSet
  }

  test("swap on futures front period index") {
    val index = "Nymex WTI nearby 2"
    val swap = new CommoditySwap(index, oct, K, V)
    val vc = createVC()
    val mtm = valuer.value(vc, swap)
    val keys = valuer.keys(vc, swap)
    val expected = Set(// oct observes dec and jan (because nearby 2)
      FuturesPriceIdentifier(wti, dec),
      FuturesPriceIdentifier(wti, jan)
    )
    keys shouldEqual expected

    val Fdec = vc.futuresPrice(wti, dec)
    val Fjan = vc.futuresPrice(wti, jan)
    val weightings = futuresMonthWeightings(vc, index, oct)

    mtm should matchQty (((Fdec * weightings(dec) + Fjan * weightings(jan)) - K) * V)
  }
}
