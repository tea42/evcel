package evcel.daterange

trait PeriodLabel 

case class DateRangePeriodLabel(dr: DateRange) extends PeriodLabel {
  override def toString = dr.toString
}

object PeriodLabel {
  val ordering = Ordering.by[PeriodLabel, Day] { case DateRangePeriodLabel(dr) => dr.firstDay}

  def apply(dr: DateRange) = new DateRangePeriodLabel(dr)
}
