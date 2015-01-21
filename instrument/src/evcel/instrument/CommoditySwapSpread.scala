package evcel.instrument

import evcel.daterange.DateRange
import evcel.quantity.BDQty
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._

case class CommoditySwapSpread(
  indexSpread : IndexSpread,
  averagingPeriod: DateRange, 
  strike: BDQty, volume: BDQty,
  pricingRule: SwapSpreadPricingRule,
  bizDaysToSettlement: Option[Int] = None
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
      IndexSpread(
        FuturesFrontPeriodIndex("Nymex WTI", nearby = 1, rollEarlyDays = 0),
        FuturesFrontPeriodIndex("ICE Brent", nearby = 1, rollEarlyDays = 0)
      ),
      Jun / 2014,
      100(USD/MT),
      10(MT),
      CommonSwapPricingRule
    )
  )

}



