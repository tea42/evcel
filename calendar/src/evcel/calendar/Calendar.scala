package evcel.calendar

import evcel.daterange.Day

trait Calendar {
  def isHoliday(day: Day)
}

case class SimpleCalendar(holidays: Set[Day]) extends Calendar {
  def isHoliday(day: Day) = holidays.contains(day)
}
