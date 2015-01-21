package evcel.daterange

/**
 * N.B.
 * When implementing a new DateRange, remember to add the appropriate
 * json formatter to EventStoreJsonProtocol.DateRangeFormat
 */
trait DateRange {
  def firstDay: Day
  def lastDay: Day

  def firstMonth: Month = firstDay.containingMonth
  def lastMonth: Month = lastDay.containingMonth

  def days: Seq[Day] = firstDay to lastDay

  def size = (lastDay - firstDay) + 1

  def contains(day: Day) = day >= firstDay && day <= lastDay

  def remainder(inclusive: Day): Option[DateRange] = if(inclusive <= firstDay) {
    Some(this)
  } else if(inclusive > lastDay) {
    None
  } else {
    Some(SimpleDateRange(inclusive, lastDay))
  }

  def normalise(f: Day => Boolean): DateRange = {
    lazy val bDays = firstMonth.days.filter(f)
    if (firstMonth == lastMonth && bDays.nonEmpty && firstDay <= bDays.head && lastDay >= bDays.last) {
      firstMonth
    } else {
      this
    }
  }

  def isDay = firstDay == lastDay
  def asDay = {
    require(isDay, s"$this is not a Day")
    firstDay
  }
}

object DateRange{
  val DAY = "Day"
  val MONTH = "Month"
  val SIMPLE = "Simple"
}

case class SimpleDateRange(firstDay: Day, lastDay: Day) extends DateRange

/**
 * Allows ordering between the same type of DateRange. So you can compare a Day to another Day
 * or a Month to another Month.
 */
trait Chronological[T <: DateRange] {
  def ordinal(t: T): Int
}

object Chronological {
  implicit def chronologicalIsOrdered[T <: DateRange](implicit c: Chronological[T]): Ordering[T] = {
    Ordering.by(c.ordinal)
  }
}
