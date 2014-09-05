package evcel.daterange
import scala.util.matching.Regex

case class Month(year: Int, monthNumber: Int) extends DateRange with Ordered[Month] {
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

  def compare(rhs: Month) = {
    if (year == rhs.year)
      monthNumber - rhs.monthNumber
    else
      year - rhs.year
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
