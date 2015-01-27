package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.referencedata.Level
import evcel.referencedata.market.{IndexLabelSpread, FuturesFrontPeriodIndexLabel}

case class CommoditySwapSpread(
  indexSpread : IndexLabelSpread,
  averagingPeriod: DateRange, 
  strike: BDQty, volume: BDQty,
  pricingRule: SwapSpreadPricingRule,
  bizDaysToSettlement: Option[Int] = None,
  index1Level: Level = Level.Close,
  index2Level: Level = Level.Close
)
  extends SingleInstrumentTradeable 
{
  def instrumentType = CommoditySwapSpread
  def tradeableType = CommoditySwapSpread
}

object CommoditySwapSpread extends InstrumentType with TradeableType{
  def name = "Commodity Swap Spread"
  def samples = Vector(
    CommoditySwapSpread(
      IndexLabelSpread(
        FuturesFrontPeriodIndexLabel("Nymex WTI", nearby = 1, rollEarlyDays = 0),
        FuturesFrontPeriodIndexLabel("ICE Brent", nearby = 1, rollEarlyDays = 0)
      ),
      Jun / 2014,
      100(USD/MT),
      10(MT),
      CommonSwapPricingRule
    )
  )

}



