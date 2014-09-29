package evcel.instrument.valuation

import evcel.calendar.Calendar
import evcel.daterange.Day

sealed trait SwapPricingRule {
  def calendar(c1: Calendar, c2: Calendar): Calendar
}

case object SingleUnderlyingSwapPricingRule extends SwapPricingRule {
  override def calendar(c1: Calendar, c2: Calendar) =
    sys.error("SingleUnderlyingSwapPricingRule, can't have multiple calendars")
}

case object CommonSwapPricingRule extends SwapPricingRule {
  override def calendar(c1: Calendar, c2: Calendar) = new Calendar {
    override def isHoliday(day: Day) = c1.isHoliday(day) || c2.isHoliday(day)
  }
}

case object NonCommonSwapPricingRule extends SwapPricingRule {
  override def calendar(c1: Calendar, c2: Calendar) = new Calendar {
    override def isHoliday(day: Day) = c1.isHoliday(day) && c2.isHoliday(day)
  }
}
