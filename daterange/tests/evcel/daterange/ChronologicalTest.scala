package evcel.daterange

import org.scalatest.{Matchers, FunSuite}

class ChronologicalTest extends FunSuite with Matchers {

  test("ordering") {
    // we can sort Seq[DateRange] even though DateRange is not ordered
    // because we have an implicit conversion for Chronological available
    def sort[T <: DateRange](dates: Seq[T])(implicit c: Chronological[T]) = {
      // not sure why I have to explicitly declare ord. Scala should look in Chronological
      // companion object for conversions.
      implicit val ord = Chronological.chronologicalIsOrdered
      dates.sorted
    }

    val months = List(Month(2012, 2), Month(2011, 1), Month(2012, 1), Month(2010, 1))
    sort(months) shouldBe List(Month(2010, 1), Month(2011, 1), Month(2012, 1), Month(2012, 2))
  }
}
