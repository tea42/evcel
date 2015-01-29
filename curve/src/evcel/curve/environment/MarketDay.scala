package evcel.curve.environment
import evcel.daterange.Day

case class MarketDay(day: Day, timeOfDay: TimeOfDay) extends Ordered[MarketDay] {
  override def compare(that: MarketDay) = day.compare(that.day) match {
    case 0 => timeOfDay.compare(that.timeOfDay)
    case o => o
  }

  def nextDay = copy(day = day.next)
}

object MarketDay {
  implicit class DayToMarketDay(day: Day) {
    def endOfDay = MarketDay(day, TimeOfDay.end)
    def startOfDay = MarketDay(day, TimeOfDay.start)
  }
}

case class TimeOfDay(
    pricesCanMove: Boolean,
    fixingsShouldExist: Boolean)  extends Ordered[TimeOfDay] {
  def optionDaysUntil(rhs: TimeOfDay) = {
    (pricesCanMove, rhs.pricesCanMove) match {
      case (true, false) => 1.0
      case (false, true) => -1.0
      case _ => 0.0
    }
  }

  override def compare(that: TimeOfDay) = optionDaysUntil(that).toInt
}

object TimeOfDay {
  val start = TimeOfDay(pricesCanMove = true, fixingsShouldExist = false)
  val end = TimeOfDay(pricesCanMove = false, fixingsShouldExist = true)
}
