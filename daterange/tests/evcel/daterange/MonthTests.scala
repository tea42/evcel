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

    it("Should format months according to http://en.wikipedia.org/wiki/ISO_8601"){
      (Jan / 14).toString should equal ("2014-01")
      (Nov / 2001).toString should equal ("2001-11")
    }
  }
}
