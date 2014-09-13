package evcel.daterange

import evcel.daterange.DateRangeSugar.Mar
import org.scalatest.{FunSuite, ShouldMatchers}

import scala.language.reflectiveCalls

class DateRangeTests extends FunSuite with ShouldMatchers {
  test("days") {
    SimpleDateRange(1 / Mar / 2014, 1 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: Nil
    SimpleDateRange(1 / Mar / 2014, 2 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: 2 / Mar / 2014 :: Nil
  }
}
