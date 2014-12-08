package evcel.daterange

trait PeriodLabel 

case class DateRangePeriodLabel(dr: DateRange) extends PeriodLabel {
  override def toString = dr.toString
}

object PeriodLabel {
  val ordering = new Ordering[PeriodLabel]{
    def compare(l : PeriodLabel, r : PeriodLabel) = {
      (l, r) match {
        case (DateRangePeriodLabel(l_dr), DateRangePeriodLabel(r_dr)) => l_dr.compare(r_dr)
        // When we have more implementations we can specify an order across them here
      }
    }
  }
  def apply(dr: DateRange) = new DateRangePeriodLabel(dr)
}
