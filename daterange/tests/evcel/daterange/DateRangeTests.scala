package evcel.daterange

import evcel.daterange.DateRangeSugar.{Jul, Jan, Mar}
import org.scalatest.{FunSuite, ShouldMatchers}

import scala.language.reflectiveCalls

class DateRangeTests extends FunSuite with ShouldMatchers {
  test("days") {
    SimpleDateRange(1 / Mar / 2014, 1 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: Nil
    SimpleDateRange(1 / Mar / 2014, 2 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: 2 / Mar / 2014 :: Nil
  }

  test("remainder") {
    Month(2010, 1).remainder(15 / Jan / 2011) shouldEqual None
    Month(2010, 1).remainder(15 / Jan / 2010) shouldEqual Some(SimpleDateRange(15 / Jan / 2010, 31 / Jan / 2010))
    Month(2010, 1).remainder(15 / Jan / 2009) shouldEqual Some(Month(2010, 1))
  }

  test("from day to day") {
    Day(2012, 1, 1) to Day(2012, 1, 1) shouldEqual Vector(Day(2012, 1, 1))
    Day(2012, 1, 1) to Day(2012, 1, 3) shouldEqual Vector(Day(2012, 1, 1),Day(2012, 1, 2),Day(2012, 1, 3))
    intercept[IllegalArgumentException] {
      Day(2012, 1, 2) to Day(2012, 1, 1)
    }
  }

  test("range size") {
    (Day(2012, 1, 1) to Day(2012, 1, 1)).size shouldEqual 1
    (Day(2012, 1, 1) to Day(2012, 1, 3)).size shouldEqual 3
  }

  test("parse month") {
    Month.unapply("2010-1") shouldEqual Some(Month(2010, 1))
    Month.unapply("2010-01") shouldEqual Some(Month(2010, 1))
    Month.unapply("2010-12") shouldEqual Some(Month(2010, 12))
    Month.unapply("10-12") shouldEqual None
    Month.unapply("20102-12") shouldEqual None
    Month.unapply("2010-121") shouldEqual None
    intercept[IllegalArgumentException] {
      Month.unapply("3500-12")
    }
    intercept[IllegalArgumentException] {
      Month.unapply("2010-13")
    }
  }
  
  test("normaliseDateRangeIfPossible") {
    def calendar(day: Day) = {
      !(day == 1 / Jul / 2014 || day == 31 / Jul / 2014)
    }

    for(dr <- List(
      SimpleDateRange(1 / Jul / 2014, 31 / Jul / 2014),
      SimpleDateRange(1 / Jul / 2014, 30 / Jul / 2014),
      SimpleDateRange(2 / Jul / 2014, 30 / Jul / 2014),
      SimpleDateRange(2 / Jul / 2014, 31 / Jul / 2014))) {
      dr.normalise(calendar) shouldEqual Month(2014, 7)
    }
    SimpleDateRange(3 / Jul / 2014, 30 / Jul / 2014).normalise(calendar) shouldEqual
      SimpleDateRange(3 / Jul / 2014, 30 / Jul / 2014)
  }
}
