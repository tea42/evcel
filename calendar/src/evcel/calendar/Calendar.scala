package evcel.calendar

import evcel.daterange.Day

trait Calendar {
  def isHoliday(day: Day): Boolean
  def weekendsAreHolidays: Boolean = true
  def isBusinessDay(day: Day) = !((day.isWeekend && weekendsAreHolidays) || isHoliday(day))

  def addBusinessDays(day: Day, n: Int): Day = {
    var d = day
    var n_ = n.abs
    while (n_ > 0) {
      d = d + n/n.abs
      if (isBusinessDay(d))
        n_ = n_ - 1
    }
    d
  }
}

case class SimpleCalendar(holidays: Set[Day]) extends Calendar {
  def isHoliday(day: Day) = holidays.contains(day)
}

class Calendars(map: Map[String, Calendar]) {
  def calendar(name: String) = map.get(name)
}