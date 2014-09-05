package evcel.daterange

import org.scalatest.FunSpec

import DateRangeSugar._
import org.scalatest.Matchers
import scala.language.reflectiveCalls

class DateRangeSugarTests extends FunSpec with Matchers {
  describe("DateRangeSugar") {
    it("Should construct days") {
      (1 / Jan / 2014) should equal(Day(2014, 1, 1))
    }
    it("Should construct months") {
      Jan / 2014 should equal(Month(2014, 1))
    }
  }
}
