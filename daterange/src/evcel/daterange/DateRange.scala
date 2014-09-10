package evcel.daterange

trait DateRange {
  def firstDay: Day
  def lastDay: Day
  def days: Iterator[Day] = Iterator.tabulate(lastDay - firstDay + 1)(firstDay + _)
}

case class SimpleDateRange(firstDay: Day, lastDay: Day) extends DateRange