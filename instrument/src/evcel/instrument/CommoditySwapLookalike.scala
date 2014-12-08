package evcel.instrument

import evcel.referencedata.ReferenceData
import evcel.daterange.Month
import evcel.instrument.valuation.FuturesFrontPeriodIndex
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._
import scala.language.reflectiveCalls

/**
 * A swap that behaves the same as a future on the given market and month.
 *
 * CommoditySwapLookalike("nymex wti", Feb) is the same as
 *  CommoditySwap("nymex wti 1st month", averagingPeriod = lastTradingDay(Feb))
 */
case class CommoditySwapLookalike(futuresMarket: String, month: Month, strike: BDQty, volume: BDQty,
                                  bizDaysToSettlement: Option[Int] = None)
  extends SingleInstrumentTradeable {

  def tradeableType = CommoditySwapLookalike

  def asCommoditySwap(refData: ReferenceData) = {
    val ltd = refData.markets.futuresMarketOrThrow(futuresMarket).lastTradingDay(refData, month)
    val ndx = new FuturesFrontPeriodIndex(futuresMarket, 1, 0)
    new CommoditySwap(ndx.indexName, ltd, strike, volume, bizDaysToSettlement = bizDaysToSettlement)
  }

  def instrumentType = CommoditySwapLookalike
}

object CommoditySwapLookalike extends InstrumentType with TradeableType{
  val name = "Commodity Swap Lookalike"
  def samples = Vector(
    CommoditySwapLookalike(
      "WTI",
      Jun / 2014,
      100(USD/BBL),
      50(BBL),
      bizDaysToSettlement = Some(10)
    ),
    CommoditySwapLookalike(
      "WTI",
      Jun / 2014,
      100(USD/BBL),
      50(BBL),
      bizDaysToSettlement = Some(10)
    )
  )
}



