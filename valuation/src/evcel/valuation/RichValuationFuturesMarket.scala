package evcel.valuation

import evcel.curve.RichFuturesMarket
import evcel.daterange.Month
import evcel.instrument._
import evcel.quantity.Qty

trait RichValuationFuturesMarket {
  implicit class RichValuationFuturesMarket(richMarket: RichFuturesMarket) {

    def unitHedge(month : Month) = {
      HedgeInstrument.intern(
        Future(
          richMarket.market.name,
          month,
          strike = Qty(0, richMarket.priceUOM),
          volume = Qty(1, richMarket.quotedVolumeUOM)
        )
      )
    }
  }
}
