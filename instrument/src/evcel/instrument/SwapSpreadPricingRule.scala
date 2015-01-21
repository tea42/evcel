package evcel.instrument

import evcel.referencedata.calendar.Calendar
import evcel.daterange.Day

sealed trait SwapSpreadPricingRule 

case object CommonSwapPricingRule extends SwapSpreadPricingRule {
  def calendar(c1: Calendar, c2: Calendar) = new Calendar {
    override def isHoliday(day: Day) = c1.isHoliday(day) || c2.isHoliday(day)
  }
}

case object NonCommonSwapPricingRule extends SwapSpreadPricingRule 
