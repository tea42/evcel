package evcel.maths

import evcel.daterange.{DateRange, Chronological}

trait Interpolation {
  /**
   * takes a sorted seq of `dates` and their corresponding `prices` and interpolates for `point`.
   *
   * Note: dates must be sorted.
   */
  def interpolate[T <: DateRange, V](dates: IndexedSeq[T], prices: IndexedSeq[V], point: T)
                                    (implicit c: Chronological[T], ev: Numberlike[V]): V
}

/**
 * Linear interpolation which uses first price for dates before the start and last price
 * for dates after the end.
 */
object LinearInterpolation extends Interpolation {
  // Dates must be sorted.
  def interpolate[T <: DateRange, V](dates: IndexedSeq[T], prices: IndexedSeq[V], point: T)
                                    (implicit c: Chronological[T], ev: Numberlike[V]): V = {
    require(dates.size == prices.size, s"dates and prices have to be the same size: $dates, $prices")

    val pointOrdinal = c.ordinal(point)
    val points = dates.map(d => c.ordinal(d))

    val possibleIndex = points.indexWhere(_ >= pointOrdinal)
    possibleIndex match {
      case -1 => prices.last
      case 0 => prices.head
      case i if points(i) == pointOrdinal => prices(i)
      case i =>
        val value0 = prices(i - 1)
        val value1 = prices(i)
        val dist = (points(i - 1) - pointOrdinal).toDouble / (points(i - 1) - points(i)).toDouble
        ev.add(value0,
          ev.multiply(ev.subtract(value1, value0), dist)
        )
    }
  }
}

/**
 * Returns the point that matches, or the price for the previous point. If point is before the start
 * then return the first price, or if it is after the end then return the last price.
 */
object LookBackInterpolation extends Interpolation {
  override def interpolate[T <: DateRange, V](dates: IndexedSeq[T], prices: IndexedSeq[V], point: T)
                                             (implicit c: Chronological[T], ev: Numberlike[V]): V = {
    val pointOrdinal = c.ordinal(point)
    val points = dates.map(d => c.ordinal(d))

    val possibleIndex = points.indexWhere(_ >= pointOrdinal)
    possibleIndex match {
      case -1 => prices.last
      case 0 => prices.head
      case i if points(i) == pointOrdinal => prices(i)
      case i => prices(i - 1)
    }
  }
}