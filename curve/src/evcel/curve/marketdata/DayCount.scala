package evcel.curve.marketdata
import evcel.daterange.Day
import evcel.curve.environment.MarketDay

sealed trait DayCount {
  def name: String
  def daysBetween(d1: Day, d2: Day): Int
  def timeBetween(d1: Day, d2: Day): Double
  def optionTimeBetween(d1: MarketDay, d2: MarketDay): Double = {
    timeBetween(d1.day, d2.day) + d1.timeOfDay.optionDaysUntil(d2.timeOfDay) / 365.0
  }
}

object DayCount {
  def fromName(name: String) = {
    name match {
      case Act365.name => Act365
      case _ => throw new RuntimeException(s"Unrecognized day count name $name")
    }
  }
}

case object Act365 extends DayCount {
  val name = "Act365"

  def daysBetween(d1: Day, d2: Day) = d2.julianDayNumber - d1.julianDayNumber
  def timeBetween(d1: Day, d2: Day) = daysBetween(d1, d2) / 365.0
}
