package evcel.daterange

class Day private (val year: Int, val month: Int, val dayNumber: Int) extends DateRange {
  def firstDay = this
  def lastDay = this

  def containingMonth = Month(year, month)
  val julianDayNumber = Day.julianDayNumber(year, month, dayNumber)
  override def hashCode = julianDayNumber
  override def equals(that: Any) = that match {
    case other: Day =>
      other.getClass == getClass && other.julianDayNumber == julianDayNumber
    case _ => false
  }

  /**
   * returns the number of days between this and another day. Is negative if
   * 	other day1 is in the future
   */
  def -(d: Day): Int = julianDayNumber - d.julianDayNumber
  def next: Day = Day.dayFromJulianDayNumber(julianDayNumber + 1)
  def previous: Day = Day.dayFromJulianDayNumber(julianDayNumber - 1)

  override def compare(rhs: DateRange): Int = rhs match {
    case d: Day => julianDayNumber - d.julianDayNumber
    case o => super.compare(o)
  }


  def +(n: Int) = Day.dayFromJulianDayNumber(julianDayNumber + n)
  def -(n: Int): Day = this.+(-n)

  def nextWeekday = {
    val dayOfWeek = julianDayNumber % 7
    if (dayOfWeek == 4) // Friday
      this + 3
    else if (dayOfWeek == 5) // Saturday
      this + 2
    else
      this + 1
  }

  def addWeekdays(n: Int) = {
    var d = this
    var n_ = n.abs
    while (n_ > 0) {
      d = d + n / n.abs
      if(d.isWeekday)
        n_ = n_ - 1
    }
    d
  }

  def isWeekday = !isWeekend
  def isWeekend = (julianDayNumber % 7) > 4

  override def toString = f"$year%4d-$month%02d-$dayNumber%02d"

  def to(day: Day): Seq[Day] = {
    require(day >= this, s"Invalid: $this, $day")
    (0 to (day - this)).map(this + _)
  }

  def min(other: Day) = if(this < other) this else other
  def max(other: Day) = if(this > other) this else other
}

object Day extends TenorType {

  private val firstPermittedJuliandayNumber = julianDayNumber(1890, 1, 1) // before excel day 0 and epoch
  /** Flyweights to limit memory usage of Day objects */
  private val dayArray = {
    val start = firstPermittedJuliandayNumber
    val end = julianDayNumber(2200, 12, 31)
    val array = new Array[Day](end - start + 1)
    for (j <- start to end) {
      val (y, m, d) = julianDayNumberToYearMonthDay(j)
      array(j - firstPermittedJuliandayNumber) = new Day(y, m, d)
    }
    array
  }

  def dayFromJulianDayNumber(jdn: Int): Day = {
    val dayIndex = jdn - firstPermittedJuliandayNumber
    require(dayIndex < dayArray.length && dayIndex >= 0, "Julian day number " + jdn + " our of range")
    dayArray(dayIndex)
  }

  def apply(y: Int, m: Int, d: Int): Day = {
    dayFromJulianDayNumber(
      julianDayNumber(y, m, d)
    )
  }

  /**
   * think this algorithm comes from Numerical Recipes
   */
  private def julianDayNumber(year: Int, month: Int, d: Int): Int = {
    var y = year
    var m = month
    if (m > 2) {
      m -= 3 // wash out the leap day
    } else {
      m += 9
      y -= 1
    }
    val c: Int = y / 100
    val ya: Int = y - 100 * c
    ((146097 * c) >> 2) + ((1461 * ya) >> 2) + (153 * m + 2) / 5 + d + 1721119
  }

  /**
   * This should be the only place where a Day instance is actually allocated.
   */
  private def julianDayNumberToYearMonthDay(jdn: Int): (Int, Int, Int) = {
    var j = jdn - 1721119
    var year = ((j << 2) - 1) / 146097
    j = (j << 2) - 1 - 146097 * year
    var d = j >> 2
    j = ((d << 2) + 3) / 1461
    d = (d << 2) + 3 - 1461 * j
    d = (d + 4) >> 2
    var month = (5 * d - 3) / 153
    d = 5 * d - 3 - 153 * month
    val day = (d + 5) / 5
    year = 100 * year + j
    if (month < 10) {
      month += 3
    } else {
      month -= 9
      year += 1
    }
    (year, month, day)
  }
}

