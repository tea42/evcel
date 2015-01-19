package evcel.daterange

import scala.language.implicitConversions

case class Month(year: Int, monthNumber: Int) extends DateRange with Ordered[Month] {
  require(year > 0 && year < 3000, s"Invalid year: $year")
  require(monthNumber > 0 && monthNumber <= 12, s"Invalid month: $monthNumber")

  def firstDay = Day(year, monthNumber, 1)
  def lastDay = next.firstDay.previous
  def next = {
    if (monthNumber == 12)
      Month(year + 1, 1)
    else
      Month(year, monthNumber + 1)
  }
  def previous = {
    if (monthNumber == 1)
      Month(year - 1, 12)
    else
      Month(year, monthNumber - 1)
  }

  override def toString = f"$year%4d-$monthNumber%02d"

  override def compare(that: Month): Int =
    Month.MonthChronological.ordinal(this).compare(Month.MonthChronological.ordinal(that))

  def +(n: Int) = {
    var m = this
    var n_ = n.abs
    while (n_ > 0) {
      m = if(n < 0) m.previous else m.next
      n_ -= 1
    }
    m
  }

  def to(rhs: Month): List[Month] = {
    require(rhs >= this, s"End of month range $rhs before $this")
    var m: Month = this
    var acc: List[Month] = Nil
    while (m <= rhs) {
      acc = m :: acc
      m = m.next
    }
    acc.reverse
  }
}

object Month extends TenorType {
  val Format = """(\d{4})\-(\d{1,2})""".r

  def unapply(str: String): Option[Month] = str match {
    case Format(year, month) => Some(Month(year.toInt, month.toInt))
    case _ => None
  }

  def parse(str: String) = unapply(str).getOrElse(sys.error("Invalid month: " + str))

  implicit object MonthChronological extends Chronological[Month] {
    override def ordinal(t: Month): Int = t.year * 12 + (t.monthNumber - 1)
  }
}
