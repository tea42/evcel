package evcel.valuation

import evcel.curve.{RichIndexSpread, ValuationContext, RichIndex}
import evcel.daterange.DateRange
import evcel.instrument._
import evcel.quantity.{UOM, Qty}
import evcel.utils.EvcelFail
import evcel.utils.EitherUtils._
import scala.util.Either

trait RichValuationIndex {
  implicit class RichValuationIndex(richIndex: RichIndex) {
    def unitHedge(averagingPeriod : DateRange) = {
      val swap = CommoditySwap(
        richIndex.index.label, averagingPeriod,
        Qty(0, richIndex.priceUOM),
        Qty(1, richIndex.quotedVolumeUOM),
        // TODO - should default biz days be reference data?
        bizDaysToSettlement = None
      )
      HedgeInstrument.intern(swap)
    }
  }

  implicit class RichValuationSpreadIndex(richIndex: RichIndexSpread) {
    def index1 = richIndex.index1
    def index2 = richIndex.index2

    def spreadPrice(
        vc : ValuationContext,
        rule : SwapSpreadPricingRule,
        period : DateRange,
        expectedUOM : UOM) : Either[EvcelFail, Qty] = {
        val List(index1Calendar, index2Calendar) = rule match {
          case CommonSwapPricingRule =>
            val cal = CommonSwapPricingRule.calendar(index1.calendar, index2.calendar)
            List(cal, cal)
          case NonCommonSwapPricingRule =>
            List(index1.calendar, index2.calendar)
        }
        val (obsDays1, obsDays2) = (
          period.days.filter(index1Calendar.isBusinessDay),
          period.days.filter(index2Calendar.isBusinessDay)
        )

        for {
          p1 <- index1.averagePrice(vc, obsDays1)
          p1_converted <- p1.in(expectedUOM, index1.marketConversions)
          p2 <- index2.averagePrice(vc, obsDays2)
          p2_converted <- p2.in(expectedUOM, index2.marketConversions)
        }
          yield
            p1_converted - p2_converted
      }
  }
}
