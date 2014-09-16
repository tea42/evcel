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
object Calendar{
  case class CalendarName(name : String) extends AnyVal
  case class Holidays(days : Set[Day]) extends AnyVal
}

case class SimpleCalendar(holidays: Calendar.Holidays) extends Calendar {
  def isHoliday(day: Day) = holidays.days.contains(day)
}

class Calendars(map: Map[String, Calendar]) {
  def calendar(name: String) = map.get(name)
}
