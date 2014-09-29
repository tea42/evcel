package evcel.daterange

trait PeriodLabel

case class DateRangePeriodLabel(dr: DateRange) extends PeriodLabel {
  override def toString = dr.toString
}

object PeriodLabel {
  def apply(dr: DateRange) = new DateRangePeriodLabel(dr)
}
