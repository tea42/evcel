package evcel.referencedata.calendar

import evcel.daterange.Day


// this should read from the same resource file that populates the main calendar store
object TestCalendars {
  val Empty = new Calendars(Map.empty) {
    override def calendar(name: String) = name match {
      case "NBP" => Right(SimpleGasCalendar)
      case _ => Right(new SimpleCalendar(Calendar.Holidays(Set.empty)))
    }
  }
}
