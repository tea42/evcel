package evcel.instrument.valuation

import evcel.daterange._
import evcel.instrument.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.Qty._
import evcel.quantity.UOM._

class SwapPositionTest extends ValuationTest {
  implicit val tol: BigDecimal = BigDecimal("1e-7")
  import scalaz.syntax.equal._
  import scalaz.std.iterable._
  import evcel.quantity.utils.QuantityTestUtils._

  test("swap on futures contract index") {
    val swap = createLookalike(bizDaysSett = Some(5))
    val vc = createVC()
    val undiscVC = vc.undiscounted
    val ltd = vc.futureExpiryDayOrThrow(wti, oct)
    val settDay = vc.futuresCalendarOrThrow(wti).addBusinessDays(ltd, 5)
    val positions = swap.positions(undiscVC)
    positions shouldEqual List(SwapHedgeInfo("Nymex WTI nearby 1", PeriodLabel(ltd), swap.volume))
    val positionsEqF = swap.positions(undiscVC.copy(params = vc.params.withShowEqFutures(b = true)))
    positionsEqF.toList shouldEqual List(FutureHedgeInfo(wti, PeriodLabel(oct), swap.volume))

    val discountRate = vc.discountRate(USD, settDay).toQty
    val positionsDisc = swap.positions(vc)
    positionsDisc assert_=== List(SwapHedgeInfo("Nymex WTI nearby 1", PeriodLabel(ltd), swap.volume * discountRate))
    val positionsEqFDisc = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    positionsEqFDisc assert_=== List(FutureHedgeInfo(wti, PeriodLabel(oct), swap.volume * discountRate))

    // expired is broken at the moment as we don't have fixings
//    val fs = vc.forwardState(ltd.endOfDay)
//    position.positions(fs, swap) shouldEqual Nil
  }

  test("swap on published index") {
    val vc = createVC()
    val observationDays = SwapLikeValuer(vc, createSingSwap()).observationDays
    val swap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT))
    val positions = swap.positions(vc)
    positions shouldEqual List(SwapHedgeInfo(sing, PeriodLabel(oct), swap.volume))
    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    positionsEqF shouldEqual positions
    swap.positions(vc.copy(params = vc.params.withTenor(Some(Month)))) shouldEqual positions

    swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).toSet shouldEqual observationDays.map{
      d => SwapHedgeInfo(sing, PeriodLabel(d), swap.volume / observationDays.size)
    }.toSet
  }

  test("swap on futures front period index") {
    val vc = createVC()
    val observationDays = SwapLikeValuer(vc, createSwap()).observationDays
    val V = Qty((observationDays.size * 13).toString, BBL)
    val swap = createSwap(volume = V)
    val positions = swap.positions(vc)
    positions shouldEqual SwapHedgeInfo(wti1st, PeriodLabel(oct), V) :: Nil

    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true)))
    val market = vc.futuresMarketOrThrow(wti)
    val weights = observationDays.map(
      d => market.frontMonth(vc.refData, d)
    ).groupBy(identity).map{case (m, instances) => m -> instances.size / BigDecimal(observationDays.size)}

    val expectedPositionsEqF = weights.map { case (m, w) =>
      FutureHedgeInfo("Nymex WTI", PeriodLabel(m), (V * w).round(9))
    }
    positionsEqF shouldEqual expectedPositionsEqF

    val expectedDaily = observationDays.map { d =>
      SwapHedgeInfo(wti1st, PeriodLabel(d), V / observationDays.size)
    }.toSet
    swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).toSet shouldEqual expectedDaily
  }

  test("nbp swap - thm/day") {
    val vc = createVC()
    val vcWithFuturesEq = vc.copy(params = vc.params.withShowEqFutures())

    val vcEnergy = vc.copy(params = vc.params.withPositionAsEnergy)
    val vcEnergyWithFuturesEq = vc.copy(params = vc.params.withPositionAsEnergy.withShowEqFutures())

    val periods = List(oct.firstDay, SimpleDateRange(oct.firstDay, oct.firstDay + 1),
      SimpleDateRange(Day(2014, 10, 3), Day(2014, 10, 6)), oct)

    for(period <- periods) {
      val swap = createSwapNBP(period = period, volume = Qty(123, THM/DAY))

      val totalEnergy = Qty(period.size, DAY) * swap.volume
      val positions = swap.positions(vc)
      val positionsEnergy = swap.positions(vcEnergy)
      positions shouldEqual SwapHedgeInfo(nbp1st, PeriodLabel(period), swap.volume) :: Nil
      positionsEnergy shouldEqual SwapHedgeInfo(nbp1st, PeriodLabel(period), totalEnergy) :: Nil

      val market = vc.futuresMarketOrThrow(nbp)
      val observationDays = period.days
      val weights = observationDays.map(
        d => market.frontMonth(vc.refData, d)
      ).groupBy(identity).map{case (m, instances) => m -> instances.size / BigDecimal(observationDays.size)}

      val energyNov = weights.get(nov).map(_ * totalEnergy)
      val energyDec = weights.get(dec).map(_ * totalEnergy)
      val powerNov = energyNov.map(_ / Qty(nov.size, DAY))
      val powerDec = energyDec.map(_ / Qty(dec.size, DAY))

      val positionsEqF = swap.positions(vcWithFuturesEq)
      positionsEqF shouldEqual List(
        powerNov.map(q => FutureHedgeInfo(nbp, PeriodLabel(nov), q.round(9))),
        powerDec.map(q => FutureHedgeInfo(nbp, PeriodLabel(dec), q.round(9)))
      ).flatten

      val positionsEqFe = swap.positions(vcEnergyWithFuturesEq)
      positionsEqFe shouldEqual List(
        energyNov.map(e => FutureHedgeInfo(nbp, PeriodLabel(nov), e.round(9))),
        energyDec.map(e => FutureHedgeInfo(nbp, PeriodLabel(dec), e.round(9)))
      ).flatten

    }
  }

  test("nbp swap - thm/day - daily tenor") {
    val vc = createVC().withParam(_.withTenor(Some(Day)))
    val vcWithFuturesEq = vc.withParam(_.withShowEqFutures())

    val vcEnergy = vc.withParam(_.withPositionAsEnergy)
    val vcEnergyWithFuturesEq = vc.withParam(_.withPositionAsEnergy.withShowEqFutures())

    val period = SimpleDateRange(Day(2014, 10, 3), Day(2014, 10, 6)) // Friday -> Monday
    val observationDays = period.days

    val swap = createSwapNBP(period = period, volume = Qty(123, THM / DAY))

    val totalEnergy = Qty(period.size, DAY) * swap.volume
    val dailyEnergy = totalEnergy / period.size
    val positions = swap.positions(vc)
    val positionsEnergy = swap.positions(vcEnergy)
    positions.toSet shouldEqual observationDays.map(d => SwapHedgeInfo(nbp1st, PeriodLabel(d), swap.volume)).toSet
    positionsEnergy.toSet shouldEqual observationDays.map(d => SwapHedgeInfo(nbp1st, PeriodLabel(d), dailyEnergy)).toSet
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
