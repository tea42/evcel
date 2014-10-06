package evcel.instrument.valuation

import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, SpotPriceIdentifier}
import evcel.instrument.{CommoditySwapLookalike, CommoditySwap}
import evcel.quantity.Qty
import evcel.instrument.valuation.Valuer._
import evcel.quantity.UOM._
import evcel.quantity.utils.QuantityTestUtils._
import evcel.quantity.Qty._

class SwapLikeValuerTest extends ValuationTest {

  test("lookalike swap on futures contract index") {
    val swap = createLookalike(bizDaysSett = Some(5))
    val vc = createVC()
    val settDay = vc.futuresCalendarOrThrow(wti).addBusinessDays(
      swap.asCommoditySwap(vc.refData).averagingPeriod.lastDay, 5
    )
    val mtmUndisc = valuer.value(vc.undiscounted, swap)
    val mtm = valuer.value(vc, swap)
    mtmUndisc shouldEqual (vc.futuresPrice("Nymex WTI", oct) - swap.strike) * swap.volume
    mtm shouldEqual mtmUndisc * vc.discountRate(USD, settDay).toQty
    mtmUndisc should not be mtm
    valuer.priceKeys(vc, swap) shouldEqual Set(FuturesPriceIdentifier(wti, oct))
    valuer.keys(vc, swap) shouldEqual Set(FuturesPriceIdentifier(wti, oct), DiscountRateIdentifier(USD,settDay))
  }

  test("swap on spot market") {
    val K = Qty("100", USD / MT)
    val V = Qty("1000", MT)
    val swap = new CommoditySwap(sing, oct, K, V, bizDaysToSettlement = Some(5))
    val vc = createVC()
    val settDay = vc.spotCalendarOrThrow(sing).addBusinessDays(oct.lastDay, 5)
    val obDays = SpotMarketIndex(sing).observationDays(vc, oct)
    val F = Qty.average(obDays.map(d => vc.spotPrice(sing, d)))
    val mtmUndisc = valuer.value(vc.undiscounted, swap)
    val mtm = valuer.value(vc, swap)
    mtmUndisc shouldEqual (F - K) * V
    mtm shouldEqual mtmUndisc * vc.discountRate(USD, settDay).toQty
    mtmUndisc should not be mtm
    valuer.keys(vc, swap) shouldEqual
      (obDays.map(SpotPriceIdentifier(sing, _)).toSet + DiscountRateIdentifier(USD,settDay))
  }

  test("swap on futures front period index") {
    val index = "Nymex WTI nearby 2"
    val swap = createSwap(index)
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

    mtm should matchQty (((Fdec * weightings(dec) + Fjan * weightings(jan)) - swap.strike) * swap.volume)
  }


  test("swap on spread index") {
    val vc = createVC()
    val observationDays = SwapLikeValuer(vc, createSingSpreadSwap()).observationDays
    val K = Qty("10", USD/MT)
    val singSwap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT), strike = K)
    val bblVol = singSwap.volume.in(BBL, vc.marketConversions(wti)).get
    val wtiSwap = createSwap(volume = -bblVol, strike = Qty("0", USD/BBL))
    val spread = createSingSpreadSwap(volume = Qty((observationDays.size * 13).toString, MT), strike= K)

    spread.keys(vc) shouldEqual (singSwap.keys(vc) ++ wtiSwap.keys(vc))
    spread.mtm(vc) should matchQty (singSwap.mtm(vc) + wtiSwap.mtm(vc))
  }
}
