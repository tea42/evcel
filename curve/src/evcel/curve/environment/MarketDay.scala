package evcel.curve.environment
import evcel.daterange.Day

case class MarketDay(day: Day, timeOfDay: TimeOfDay)

object MarketDay {
  implicit class DayToMarketDay(day: Day) {
    def endOfDay = MarketDay(day, TimeOfDay.end)
    def startOfDay = MarketDay(day, TimeOfDay.start)
  }
}

case class TimeOfDay(
    pricesCanMove: Boolean,
    fixingsShouldExist: Boolean) {
  def optionDaysUntil(rhs: TimeOfDay) = {
    (pricesCanMove, rhs.pricesCanMove) match {
      case (true, false) => 1.0
      case (false, true) => -1.0
      case _ => 0.0
    }
  }
}

object TimeOfDay {
  val start = TimeOfDay(pricesCanMove = true, fixingsShouldExist = false)
  val end = TimeOfDay(pricesCanMove = false, fixingsShouldExist = true)
}
