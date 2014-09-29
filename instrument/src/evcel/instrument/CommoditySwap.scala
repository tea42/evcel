package evcel.instrument

import evcel.daterange.{Day, DateRange}
import evcel.instrument.valuation.{SingleUnderlyingSwapPricingRule, SwapPricingRule}
import evcel.quantity.BDQty

case class CommoditySwap(market: String, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  pricingRule: SwapPricingRule = SingleUnderlyingSwapPricingRule)
  extends Instrument {
}



