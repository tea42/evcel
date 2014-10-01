package evcel.referencedata.calendar

import evcel.daterange.DateRangeSugar._
import evcel.daterange.DateRangeSugar.Jul
import evcel.daterange.{Month, SimpleDateRange, Day}
import org.scalatest.{Matchers, FunSuite}

import scala.language.reflectiveCalls

class CalendarTests extends FunSuite with Matchers  {

  test("add business days") {
    val monday = 21 / Jul / 2014
    val tuesday = 22 / Jul / 2014
    val wednesday = 23 / Jul / 2014
    val thursday = 24 / Jul / 2014
    val friday = 25 / Jul / 2014
    val saturday = 26 / Jul / 2014
    val sunday = 27 / Jul / 2014

    val cal = new Calendar {
      override def isHoliday(day: Day) = day == friday || day == wednesday
    }
    val calWeekendsNotHols = new Calendar {
      override def isHoliday(day: Day) = day == friday || day == wednesday

      override def weekendsAreHolidays = false
    }

    cal.addBusinessDays(thursday, -2) shouldEqual monday
    cal.addBusinessDays(thursday, -1) shouldEqual tuesday
    cal.addBusinessDays(thursday, 1) shouldEqual 28 / Jul / 2014
    cal.addBusinessDays(thursday, 2) shouldEqual 29 / Jul / 2014

    calWeekendsNotHols.addBusinessDays(thursday, -2) shouldEqual monday
    calWeekendsNotHols.addBusinessDays(thursday, -1) shouldEqual tuesday
    calWeekendsNotHols.addBusinessDays(thursday, 1) shouldEqual saturday
    calWeekendsNotHols.addBusinessDays(thursday, 2) shouldEqual sunday
  }
}
