package evcel.referencedata.calendar

import evcel.daterange.Day


// this should read from the same resource file that populates the main calendar store
object TestCalendars {
  val Empty = new Calendars(Map.empty) {
    override def calendar(name: String) = name match {
      case "NBP" => Some(SimpleGasCalendar)
      case _ => Some(new SimpleCalendar(Calendar.Holidays(Set.empty)))
    }
  }
}
