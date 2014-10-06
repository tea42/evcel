package evcel.instrument

import evcel.daterange.DateRange
import evcel.instrument.valuation.SwapPricingRule
import evcel.quantity.BDQty

case class CommoditySwapSpread(spreadName: String, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  pricingRule: SwapPricingRule,
  bizDaysToSettlement: Option[Int] = None)
  extends Instrument {
}



