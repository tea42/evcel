package evcel.instrument.valuation

import evcel.daterange.{Day, Month, PeriodLabel}
import evcel.instrument.{CommoditySwapLookalike, CommoditySwap}
import evcel.quantity.Qty
import evcel.instrument.valuation.Position._
import evcel.quantity.UOM._

class SwapPositionTest extends ValuationTest {
  val K = Qty("100", USD / BBL)
  val V = Qty("1000", BBL)

  test("swap on futures contract index") {
    val swap = new CommoditySwapLookalike(wti, oct, K, V)
    val vc = createVC()
    val ltd = vc.futureExpiryDayOrThrow(wti, oct)
    val positions = swap.positions(vc)
    positions shouldEqual List(SwapHedgeInstrument("Nymex WTI nearby 1", PeriodLabel(ltd), V))
    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    positionsEqF.toList shouldEqual List(FutureHedgeInstrument(wti, PeriodLabel(oct), V))

    // expired is broken at the moment as we don't have fixings
//    val fs = vc.forwardState(ltd.endOfDay)
//    position.positions(fs, swap) shouldEqual Nil
  }

  test("swap on published index") {
    val vc = createVC()
    val observationDays = SwapLikeValuer(vc, createSingSwap()).observationDays
    val swap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT))
    val positions = swap.positions(vc)
    positions shouldEqual List(SwapHedgeInstrument(sing, PeriodLabel(oct), swap.volume))
    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    positionsEqF shouldEqual positions
    swap.positions(vc.copy(params = vc.params.withTenor(Some(Month)))) shouldEqual positions

    swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).toSet shouldEqual observationDays.map{
      d => SwapHedgeInstrument(sing, PeriodLabel(d), swap.volume / observationDays.size)
    }.toSet
  }

  test("swap on futures front period index") {
    val vc = createVC()
    val observationDays = SwapLikeValuer(vc, createSwap()).observationDays
    val V = Qty((observationDays.size * 13).toString, BBL)
    val swap = createSwap(volume = V)
    val positions = swap.positions(vc)
    positions shouldEqual SwapHedgeInstrument(wti1st, PeriodLabel(oct), V) :: Nil

    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    val market = vc.futuresMarketOrThrow(wti)
    val weights = observationDays.map(
      d => market.frontMonth(vc.refData, d)
    ).groupBy(identity).map{case (m, instances) => m -> instances.size / BigDecimal(observationDays.size)}

    val expectedPositionsEqF = weights.map { case (m, w) =>
      FutureHedgeInstrument(s"Nymex WTI", PeriodLabel(m), (V * w).round(9))
    }
    positionsEqF shouldEqual expectedPositionsEqF

    val expectedDaily = observationDays.map { d =>
      SwapHedgeInstrument(wti1st, PeriodLabel(d), V / observationDays.size)
    }.toSet
    swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).toSet shouldEqual expectedDaily
  }

  test("swap on spread index") {
    val valContext = createVC()
    val observationDays = SwapLikeValuer(valContext, createSingSpreadSwap()).observationDays
    val singSwap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT))
    val wtiSwap = createSwap(volume = -singSwap.volume.in(BBL, valContext.marketConversions(wti)).get)
    val spread = createSingSpreadSwap(volume = Qty((observationDays.size * 13).toString, MT))

    for (eqFutures <- List(false, true); tenor <- List(Some(Day), None, Some(Month))) {
      val vc = valContext.copy(params = valContext.params.withShowEqFutures(eqFutures))
        .copy(params = valContext.params.withTenor(tenor))
      spread.positions(vc).toList.sortWith(_.toString < _.toString) shouldEqual
        (singSwap.positions(vc) ++ wtiSwap.positions(vc)).toList.sortWith(_.toString < _.toString)
    }
  }
}
