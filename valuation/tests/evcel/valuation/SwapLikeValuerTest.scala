package evcel.valuation

import evcel.curve.{RichSpotMarket, RichFuturesMarket, RichIndexSpread, RichIndex}
import evcel.curve.curves.{DiscountRateIdentifier, FuturesPriceIdentifier, SpotPriceIdentifier}
import evcel.instrument.{CommoditySwapLookalike, CommoditySwap}
import evcel.quantity.Qty
import evcel.referencedata.Level
import evcel.referencedata.market.IndexSpread
import evcel.valuation.Valuer._
import evcel.quantity.UOM._
import evcel.quantity.Qty._
import scala.math.BigDecimal
import evcel.utils.EitherTestPimps

class SwapLikeValuerTest extends ValuationTest with EitherTestPimps{
  implicit val tol: BigDecimal = BigDecimal("1e-7")
  import scalaz.syntax.equal._
  import evcel.quantity.utils.QuantityTestUtils._

  test("lookalike swap on futures contract index") {
    val swap = createLookalike(bizDaysSett = Some(5))
    val vc = createVC()
    val mkt = RichFuturesMarket(vc.refData, wti).R
    val settDay = mkt.calendar.addBusinessDays(
      swap.asCommoditySwap(vc.refData).right.get.averagingPeriod.lastDay, 5
    )
    val mtmUndisc = swap.mtm(vc.undiscounted)(valuer).R
    val mtm = swap.mtm(vc)(valuer).R
    mtmUndisc shouldEqual (vc.futuresPrice(mkt.market, oct).R - swap.strike) * swap.quotedVolume
    mtm shouldEqual mtmUndisc * vc.discountRate(USD, settDay).R
    mtmUndisc should not be mtm
    val wtiMarket = vc.futuresMarket("Nymex WTI").R
    valuer.priceKeys(vc, swap) shouldEqual Set(FuturesPriceIdentifier(wtiMarket, oct))
    valuer.keys(vc, swap) shouldEqual Set(FuturesPriceIdentifier(wtiMarket, oct), DiscountRateIdentifier(USD,settDay))
  }

  test("swap on spot market") {
    val K = Qty("100", USD / MT)
    val V = Qty("1000", MT)
    val swap = new CommoditySwap(sing, oct, K, V, bizDaysToSettlement = Some(5))
    val vc = createVC()
    val spotIndex = RichIndex(vc.refData, sing, Level.Close).R
    val spotMarket = RichSpotMarket(vc.refData, sing.indexName).R
    val settDay = spotIndex.calendar.addBusinessDays(oct.lastDay, 5)
    val obDays = spotIndex.observationDays(oct)
    val F = Qty.average(obDays.map(d => vc.spotPrice(spotMarket.market, d).R))
    val mtmUndisc = swap.mtm(vc.undiscounted)(valuer).R
    val mtm = swap.mtm(vc)(valuer).R
    mtmUndisc assert_=== (F - K) * V
    mtm shouldEqual mtmUndisc * vc.discountRate(USD, settDay).R
    mtmUndisc should not be mtm
    valuer.keys(vc, swap) shouldEqual
      (obDays.map(SpotPriceIdentifier(spotMarket.market, _)).toSet + DiscountRateIdentifier(USD,settDay))
  }

  test("swap on futures front period index") {
    val index = "Nymex WTI nearby 2"
    val swap = createSwap(index)
    val vc = createVC()
    val mtm = swap.mtm(vc)(valuer).R
    val keys = valuer.keys(vc, swap)
    val wtiMarket = vc.futuresMarket("Nymex WTI").R
    val expected = Set(// oct observes dec and jan (because nearby 2)
      FuturesPriceIdentifier(wtiMarket, dec),
      FuturesPriceIdentifier(wtiMarket, jan)
    )
    keys shouldEqual expected

    val Fdec = vc.futuresPrice(wtiMarket, dec).R
    val Fjan = vc.futuresPrice(wtiMarket, jan).R
    val weightings = futuresMonthWeightings(vc, index, oct)

    mtm should matchQty (((Fdec * weightings(dec) + Fjan * weightings(jan)) - swap.strike) * swap.volume)
  }


  test("swap on spread index") {
    val vc = createVC()
    val singSpreadSwap = createSingSpreadSwap()
    val ndx = IndexSpread(singSpreadSwap.indexSpread, singSpreadSwap.index1Level, singSpreadSwap.index2Level)
    val richIndex = RichIndexSpread(vc.refData, ndx).R
    val observationDays = singSpreadSwap.averagingPeriod.days.filter(richIndex.commonCalendar.isBusinessDay)
    val K = Qty("10", USD/MT)
    val singSwap = createSingSwap(volume = Qty((observationDays.size * 13).toString, MT), strike = K)
    val mkt = RichFuturesMarket(vc.refData, wti).R
    val bblVol = singSwap.volume.in(BBL, mkt.conversions).R
    val wtiSwap = createSwap(volume = -bblVol, strike = Qty("0", USD/BBL))
    val spread = createSingSpreadSwap(volume = Qty((observationDays.size * 13).toString, MT), strike= K)

    spread.keys(vc) shouldEqual (singSwap.keys(vc) ++ wtiSwap.keys(vc))
    spread.mtm(vc).R should matchQty (singSwap.mtm(vc).R + wtiSwap.mtm(vc).R)
  }
}
