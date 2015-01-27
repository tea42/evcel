package evcel.daterange

import scala.language.implicitConversions

case class Month(year: Int, monthNumber: Int) extends DateRange with Ordered[Month] {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(monthNumber > 0 && monthNumber <= 12, s"Invalid month: $monthNumber")

  def firstDay = Day(year, monthNumber, 1)
  def lastDay = next.firstDay.previous
  def next = this + 1
  def previous = this - 1

  override def toString = f"$year%4d-$monthNumber%02d"

  override def compare(that: Month): Int = ordinal.compare(ordinal)

  def -(n: Int) = this + -n

  def +(n: Int) = Month.fromOrdinal(ordinal + n)

  def to(rhs: Month): IndexedSeq[Month] = {
    require(rhs >= this, s"End of month range $rhs before $this")
    (ordinal to rhs.ordinal).map(Month.fromOrdinal)
  }

  @transient private lazy val ordinal = year * 12 + (monthNumber - 1)
}

object Month extends TenorType {
  val Format = """(\d{4})\-(\d{1,2})""".r
  val FormatMMMYY = """([a-zA-Z]{3})\-*([\d]{2})""".r

  val months = Vector("January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December").map(_.toLowerCase)

  def unapply(str: String): Option[Month] = str match {
    case Format(year, month) => Some(Month(year.toInt, month.toInt))
    case FormatMMMYY(mmm, yy) =>
      val mIndex = months.indexWhere(_.startsWith(mmm.toLowerCase))
      require(mIndex >= 0, s"Invalid month: '$mmm'")
      Some(Month(2000 + yy.toInt, mIndex + 1))
    case _ => None
  }

  def parse(str: String) = unapply(str).getOrElse(sys.error("Invalid month: " + str))

  private def fromOrdinal(ordinal: Int) = Month(ordinal / 12, (ordinal % 12) + 1)

  implicit object MonthChronological extends Chronological[Month] {
    override def ordinal(t: Month): Int = t.ordinal
  }
}
