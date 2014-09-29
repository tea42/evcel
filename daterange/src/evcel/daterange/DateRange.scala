package evcel.daterange

/**
 * N.B.
 * When implementing a new DateRange, remember to add the appropriate
 * json formatter to EventStoreJsonProtocol.DateRangeFormat
 */
trait DateRange extends Ordered[DateRange] {
  def firstDay: Day
  def lastDay: Day

  def firstMonth: Month = firstDay.containingMonth
  def lastMonth: Month = lastDay.containingMonth

  def days: Seq[Day] = firstDay to lastDay

  def contains(day: Day) = day >= firstDay && day <= lastDay

  def remainder(inclusive: Day): Option[DateRange] = if(inclusive <= firstDay) {
    Some(this)
  } else if(inclusive > lastDay) {
    None
  } else {
    Some(SimpleDateRange(inclusive, lastDay))
  }

  override def compare(that: DateRange) = {
    firstDay.compare(that.firstDay) match {
      case 0 => lastDay.compare(that.lastDay) match {
        case 0 => this.toString compare that.toString
        case n => n
      }
      case n => n
    }
  }

  def normalise(f: Day => Boolean): DateRange = {
    lazy val bDays = firstMonth.days.filter(f)
    if (firstMonth == lastMonth && bDays.nonEmpty && firstDay <= bDays.head && lastDay >= bDays.last) {
      firstMonth
    } else {
      this
    }
  }
}

object DateRange{
  val DAY = "Day"
  val MONTH = "Month"
  val SIMPLE = "Simple"
}

case class SimpleDateRange(firstDay: Day, lastDay: Day) extends DateRange