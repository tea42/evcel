package evcel.calendarstore

import evcel.daterange.Day

object CalendarTypeAliases {
  type Version = Int
  type Name = String
  type Holidays = Set[Day]
}
