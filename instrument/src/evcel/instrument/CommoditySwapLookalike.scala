package evcel.instrument

import evcel.referencedata.{Level, ReferenceData}
import evcel.daterange.Month
import evcel.quantity.BDQty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.daterange.DateRangeSugar._
import evcel.referencedata.market.FuturesFrontPeriodIndexLabel
import scala.language.reflectiveCalls
import scala.util.Either
import evcel.utils.EvcelFail
import evcel.utils.EitherUtils._

/**
 * A swap that behaves the same as a future on the given market and month.
 *
 * CommoditySwapLookalike("nymex wti", Feb) is the same as
 *  CommoditySwap("nymex wti 1st month", averagingPeriod = lastTradingDay(Feb))
 */
case class CommoditySwapLookalike(futuresMarket: String, month: Month, strike: BDQty, quotedVolume: BDQty,
                                  bizDaysToSettlement: Option[Int] = None, level: Level = Level.Close)
  extends SingleInstrumentTradeable {

  def tradeableType = CommoditySwapLookalike

  def asCommoditySwap(refData: ReferenceData) : Either[EvcelFail, CommoditySwap] = {
    for {
      expiryRule <- refData.futuresExpiryRule(futuresMarket)
      ltd <- expiryRule.futureExpiryDay(month)
    } yield {
      val ndx = new FuturesFrontPeriodIndexLabel(futuresMarket, 1, 0)
      new CommoditySwap(ndx, ltd, strike, quotedVolume, bizDaysToSettlement = bizDaysToSettlement)
    }

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



