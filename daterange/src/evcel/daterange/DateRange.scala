package evcel.daterange

/**
 * N.B.
 * When implementing a new DateRange, remember to add the appropriate
 * json formatter to EventStoreJsonProtocol.DateRangeFormat
 */
trait DateRange {
  def firstDay: Day
  def lastDay: Day
  def days: Iterator[Day] = Iterator.tabulate(lastDay - firstDay + 1)(firstDay + _)
}

object DateRange{
  val DAY = "Day"
  val MONTH = "Month"
  val SIMPLE = "Simple"
}

case class SimpleDateRange(firstDay: Day, lastDay: Day) extends DateRange