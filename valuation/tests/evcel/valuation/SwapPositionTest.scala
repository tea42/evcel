package evcel.valuation

import evcel.curve.{RichFuturesMarket, RichIndex}
import evcel.daterange._
import evcel.referencedata.Level
import evcel.referencedata.market.{FuturesFrontPeriodIndexLabel, IndexLabel}
import evcel.valuation.Valuer._
import evcel.quantity.Qty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import scala.math.BigDecimal
import scala.collection.immutable.Nil
import evcel.instrument._
import scala.util.{Right, Either}
import evcel.utils.EvcelFail

class SwapPositionTest extends ValuationTest {
  implicit val tol: BigDecimal = BigDecimal("1e-7")
  import scalaz.syntax.equal._
  import scalaz.std.iterable._
  import evcel.quantity.utils.QuantityTestUtils._

  private def checkPositionsMatch(
    expected : Map[HedgeInstrument, Either[EvcelFail, Double]], 
    actual : Map[HedgeInstrument, Either[EvcelFail, Double]]
  ){
    assertResult(actual.keySet, "Hedge instruments differ")(expected.keySet)
    expected.keys.foreach{
      hedgeInstrument => 
        (expected(hedgeInstrument), actual(hedgeInstrument)) match {
          case (Right(expectedPosition), Right(actualPosition)) => 
            expectedPosition should be (actualPosition +- 1e-9)
          case other => 
            fail(s"Unexpected positions $other")
        }
    }
  }

  def checkPositionsMatch(
    actual : Map[HedgeInstrument, Either[EvcelFail, Double]],
    expected : (HedgeInstrument, Double)*
  ){
    checkPositionsMatch(
      expected.toList.toMap.filter{case (_, v) => v != 0}.mapValues(Right(_)),
      actual
    )
  }


  test("swap on futures contract index") {
    val swap = createLookalike(bizDaysSett = Some(5))
    val vc = createVC()
    val undiscVC = vc.undiscounted
    val mkt = RichFuturesMarket(vc.refData, wti).R
    val ltd = mkt.lastTradingDay(oct).R
    val index = FuturesFrontPeriodIndexLabel(wti, nearby = 1, rollEarlyDays = 0)
    val richIndex = RichIndex(vc.refData, index, Level.Close).R
    val settDay = mkt.calendar.addBusinessDays(ltd, 5)
    val positions = swap.positions(undiscVC).R
    positions shouldEqual Map(richIndex.unitHedge(ltd) -> Right(swap.quotedVolume.doubleValue))
    val positionsEqF = swap.positions(undiscVC.copy(params = vc.params.withShowEqFutures(b = true))).R
    positionsEqF shouldEqual Map(mkt.unitHedge(oct) -> Right(swap.quotedVolume.doubleValue))

    val discountRate = vc.discountRate(USD, settDay).R
    val positionsDisc = swap.positions(vc).R
    checkPositionsMatch(
      positionsDisc,
      richIndex.unitHedge(ltd) -> swap.quotedVolume.doubleValue * discountRate.doubleValue
    )

    val positionsEqFDisc = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true))).R
    checkPositionsMatch(
      positionsEqFDisc,
      mkt.unitHedge(oct) -> swap.quotedVolume.doubleValue * discountRate.doubleValue
    )

    // expired is broken at the moment as we don't have fixings
//    val fs = vc.forwardState(ltd.endOfDay)
//    position.positions(fs, swap) shouldEqual Nil
  }

  test("swap on published index") {
    val vc = createVC()
    var swap = createSingSwap()
    val index = RichIndex(vc.refData, swap.index, Level.Close).R
    val observationDays = index.observationDays(swap.averagingPeriod)
    swap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT))
    val positions = swap.positions(vc).R
    checkPositionsMatch(
      positions,
      index.unitHedge(oct) -> swap.volume.doubleValue
    )
    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true))).R
    positionsEqF shouldEqual positions
    checkPositionsMatch(
      positions,
      swap.positions(vc.copy(params = vc.params.withTenor(Some(Month)))).R
    )

    val expectedDailyPositions : Map[HedgeInstrument, Either[EvcelFail, Double]] = observationDays.map{
      d => 
        index.unitHedge(d) -> Right((swap.volume / observationDays.size).round(9).doubleValue)
    }.toMap

    checkPositionsMatch(
      expectedDailyPositions,
      swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).R
    )
  }

  test("swap on futures front period index") {
    val vc = createVC()
    var swap = createSwap()
    val index = RichIndex(vc.refData, swap.index, Level.Close).R
    val observationDays = index.observationDays(swap.averagingPeriod)
    val V = Qty((observationDays.size * 13).toString, BBL)
    swap = createSwap(volume = V)
    val positions = swap.positions(vc).R
    checkPositionsMatch(
      positions,
      index.unitHedge(swap.averagingPeriod) -> swap.volume.doubleValue
    )


    val positionsEqF = swap.positions(vc.copy(params = vc.params.withShowEqFutures(b = true))).R
    val market = RichFuturesMarket(vc.refData, wti).R
    val weights = observationDays.map(
      d => market.frontMonth(d).R
    ).groupBy(identity).map{case (m, instances) => m -> instances.size / BigDecimal(observationDays.size)}

    val expectedPositionsEqF : Map[HedgeInstrument, Either[EvcelFail, Double]] = weights.map { case (m, w) =>
      market.unitHedge(m) -> Right((V * w).round(9).doubleValue)
    }.toMap
    checkPositionsMatch(
      expectedPositionsEqF,
      positionsEqF
    )

    val expectedDaily : Map[HedgeInstrument, Either[EvcelFail, Double]] = observationDays.map { d =>
      index.unitHedge(d) -> Right((V / observationDays.size).round(9).doubleValue)
    }.toMap
    val actual = swap.positions(vc.copy(params = vc.params.withTenor(Some(Day)))).R
    checkPositionsMatch(
      expectedDaily, actual
    )
  }

  test("nbp swap - thm/day") {
    val vc = createVC()
    val vcWithFuturesEq = vc.copy(params = vc.params.withShowEqFutures())


    val periods = List(oct.firstDay, SimpleDateRange(oct.firstDay, oct.firstDay + 1),
      SimpleDateRange(Day(2014, 10, 3), Day(2014, 10, 6)), oct)

    val market = RichFuturesMarket(vc.refData, nbp).R
    val index = RichIndex(vc.refData, IndexLabel.parse(nbp1st), Level.Close).R
    for(period <- periods) {
      val swap = createSwapNBP(period = period, volume = Qty(123, THM/DAY))

      val totalEnergy = Qty(period.size, DAY) * swap.volume
      val positions = swap.positions(vc).R
      checkPositionsMatch(
        positions,
        index.unitHedge(period) -> swap.volume.doubleValue
      )

      val observationDays = period.days
      val weights = observationDays.map(
        d => market.frontMonth(d).R
      ).groupBy(identity).map{case (m, instances) => m -> instances.size / BigDecimal(observationDays.size)}

      val energyNov = weights.get(nov).map(_ * totalEnergy).getOrElse(Qty.NULL)
      val energyDec = weights.get(dec).map(_ * totalEnergy).getOrElse(Qty.NULL)
      val powerNov = energyNov / Qty(nov.size, DAY)
      val powerDec = energyDec / Qty(dec.size, DAY)

      val positionsEqF = swap.positions(vcWithFuturesEq).R
      checkPositionsMatch(
        positionsEqF,
        market.unitHedge(nov) -> powerNov.doubleValue,
        market.unitHedge(dec) -> powerDec.doubleValue
      )
        
    }
  }

  test("nbp swap - thm/day - daily tenor") {
    val vc = createVC().withParam(_.withTenor(Some(Day)))
    val index = RichIndex(vc.refData, IndexLabel.parse(nbp1st), Level.Close).R

    val period = SimpleDateRange(Day(2014, 10, 3), Day(2014, 10, 6))  //Friday -> Monday
    val observationDays = period.days

    val swap = createSwapNBP(period = period, volume = Qty(123, THM / DAY))

    val totalEnergy = Qty(period.size, DAY) * swap.volume
    val dailyEnergy = totalEnergy / period.size
    val positions = swap.positions(vc).R
    checkPositionsMatch(
      positions,
      observationDays.map{
        d => 
          index.unitHedge(d) -> swap.volume.doubleValue
      } : _*
    )
  }

  test("swap on spread index") {
    val valContext = createVC()
    var singSwap = createSingSwap()
    val index = RichIndex(valContext.refData, singSwap.index, Level.Close).R
    val observationDays = index.observationDays(singSwap.averagingPeriod)
    singSwap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT))
    val wtiSwap = createSwap(volume = -singSwap.volume.in(BBL, index.marketConversions).R)
    val spread = createSingSpreadSwap(volume = Qty((observationDays.size * 13).toString, MT))

    for (eqFutures <- List(false, true); tenor <- List(Some(Day), None, Some(Month))) {
      val vc = valContext.copy(params = valContext.params.withShowEqFutures(eqFutures))
        .copy(params = valContext.params.withTenor(tenor))
      spread.positions(vc).R.toList.sortWith(_.toString < _.toString) shouldEqual
        (singSwap.positions(vc).R ++ wtiSwap.positions(vc).R).toList.sortWith(_.toString < _.toString)
    }
  }
}
