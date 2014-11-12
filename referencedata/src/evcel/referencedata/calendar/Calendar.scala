package evcel.referencedata.calendar

import evcel.daterange.{DateRange, Day}
import evcel.referencedata.ReferenceDataTrait

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

  def &&(other: Calendar) = new Calendar {
    override def isHoliday(day: Day): Boolean = Calendar.this.isHoliday(day) || other.isHoliday(day)
  }
}
object Calendar{
  case class CalendarName(name : String) extends AnyVal
  case class Holidays(days : Set[Day]) extends AnyVal
}

case class CalendarData(days : Set[Day]) extends ReferenceDataTrait
case class SimpleCalendar(holidays: Calendar.Holidays) extends Calendar {
  def isHoliday(day: Day) = holidays.days.contains(day)
}
case object SimpleGasCalendar extends Calendar {
  override def weekendsAreHolidays: Boolean = false
  def isHoliday(day: Day) = false
}

class Calendars(map: Map[String, Calendar]) {
  def calendar(name: String) = map.get(name)
}
