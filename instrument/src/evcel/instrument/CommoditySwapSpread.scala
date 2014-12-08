package evcel.instrument

import evcel.daterange.DateRange
import evcel.instrument.valuation.SwapPricingRule
import evcel.quantity.BDQty
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.instrument.valuation.CommonSwapPricingRule

case class CommoditySwapSpread(
  spreadName: String, averagingPeriod: DateRange, 
  strike: BDQty, volume: BDQty,
  pricingRule: SwapPricingRule,
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
      "WTI vs Brent",
      Jun / 2014,
      100(USD/MT),
      10(MT),
      CommonSwapPricingRule
    )
  )

}



