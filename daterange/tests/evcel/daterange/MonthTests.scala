package evcel.daterange

import org.scalatest.FunSpec
import org.scalatest.Matchers
import DateRangeSugar._

class MonthTests extends FunSpec with Matchers {
  describe("Month") {
    it("should get previous/next") {
      (Jan / 14).previous should equal(Dec / 13)
      (Jan / 14).next should equal(Feb / 14)
      (Dec / 13).next should equal(Jan / 14)
      (Dec / 13).previous should equal(Nov / 13)
    }

    it("should return a seq of months") {
      (Jan / 14) to (Mar / 14) shouldEqual Vector(Jan / 14, Feb / 14, Mar / 14)
    }

    it("should add and subtract to match next and prev") {
      val month = Jan / 14
      val next = Iterator.iterate(month)(_.next)
      (0 to 100).map(i => month + i shouldEqual next.next)
      val prev = Iterator.iterate(month)(_.previous)
      (0 to 100).map(i => month - i shouldEqual prev.next)
    }

    it("should parse MMM-YY months") {
      Seq(("Dec-14", Dec / 14), ("Jan-11", Jan / 11)).foreach{
        case (str, month) => Month.unapply(str) shouldEqual Some(month)
      }
    }

    it("Should format months according to http://en.wikipedia.org/wiki/ISO_8601"){
      (Jan / 14).toString should equal ("2014-01")
      (Nov / 2001).toString should equal ("2001-11")
    }

    it("should have the right month names") {
      val names =
        Vector(
          1 -> "January",
          2 -> "February",
          3 -> "March",
          4 -> "April",
          5 -> "May",
          6 -> "June",
          7 -> "July",
          8 -> "August",
          9 -> "September",
          10 -> "October",
          11 -> "November",
          12 -> "December")
      names.foreach {
        case (i, name) =>
          Month.months(i - 1) shouldEqual name.toLowerCase
      }
    }
  }
}
