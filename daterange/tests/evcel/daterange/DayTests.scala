package evcel.daterange

import org.scalatest.FunSpec
import org.scalatest.Matchers
import DateRangeSugar._
import scala.language.reflectiveCalls

class DayTests extends FunSpec with Matchers {
  describe("Day") {
    it("Should have working next/previous") {
      (1 / Jan / 2010).next should equal(2 / Jan / 10)
      (1 / Jan / 2010).previous should equal(31 / Dec / 2009)
    }

    it("Should return the next weekday") {
      val friday = 25 / Jul / 2014
      val saturday = friday.next
      val sunday = saturday.next
      val monday = sunday.next
      val tuesday = monday.next

      friday.nextWeekday should equal(monday)
      saturday.nextWeekday should equal(monday)
      sunday.nextWeekday should equal(monday)
      monday.nextWeekday should equal(tuesday)
    }

    it("should add weekdays") {
      val friday = 25 / Jul / 2014
      friday.addWeekdays(- 1) shouldEqual(24 / Jul / 2014)
      friday.addWeekdays(- 2) shouldEqual(23 / Jul / 2014)
      friday.addWeekdays(1) shouldEqual(28 / Jul / 2014)
      friday.addWeekdays(2) shouldEqual(29 / Jul / 2014)
    }

    it("Should format days according to http://en.wikipedia.org/wiki/ISO_8601"){
      (1 / Jan / 2000).toString should equal ("2000-01-01")
      (31 / Dec / 1999).toString should equal ("1999-12-31")
      (25 / Sep / 14).toString should equal ("2014-09-25")
    }

    it("should support excel day representation") {
      Day.fromExcel(42429.00) shouldBe Day(2016, 2, 29)
      Day(2016, 2, 29).toExcel shouldBe 42429.00

      intercept[RuntimeException] {
        Day.fromExcel(0.0)
      }.getMessage shouldBe "requirement failed: Excel represents days from 1.0, 0.0 is not a valid day."
    }
  }
}
