package evcel.maths

import evcel.daterange.{Chronological, Day, Month}
import org.scalatest.{FunSuite, ShouldMatchers}

class InterpolationTest extends FunSuite with ShouldMatchers {

  test("linear interpolation for days") {
    val days = Vector(Day(2015, 1, 1), Day(2015, 1, 2), Day(2015, 1, 6), Day(2015, 1, 7), Day(2015, 1, 8))
    val prices = Vector(10.0, 20.0, 30.0, 40.0, 50.0)

    LinearInterpolation.interpolate(days, prices, Day(2014, 1, 1)) shouldBe 10.0
    LinearInterpolation.interpolate(days, prices, Day(2015, 1, 1)) shouldBe 10.0
    LinearInterpolation.interpolate(days, prices, Day(2015, 1, 4)) shouldBe 25.0
    LinearInterpolation.interpolate(days, prices, Day(2015, 1, 7)) shouldBe 40.0
    LinearInterpolation.interpolate(days, prices, Day(2016, 1, 1)) shouldBe 50.0
  }

  test("linear interpolation for months") {
    val months = Vector(Month(2015, 1), Month(2015, 2), Month(2015, 4), Month(2015, 5))
    val prices = Vector(10.0, 20.0, 40.0, 50.0)

    LinearInterpolation.interpolate(months, prices, Month(2014, 1)) shouldBe 10.0
    LinearInterpolation.interpolate(months, prices, Month(2015, 1)) shouldBe 10.0
    LinearInterpolation.interpolate(months, prices, Month(2015, 3)) shouldBe 30.0
    LinearInterpolation.interpolate(months, prices, Month(2016, 1)) shouldBe 50.0
  }

  test("linear interpolation for months with custom chronological") {
    val months = Vector(Month(2015, 1), Month(2015, 2), Month(2015, 4), Month(2015, 5))
    val prices = Vector(10.0, 20.0, 40.0, 50.0)

    implicit val order = {
      new Chronological[Month] {
        override def ordinal(t: Month): Int = t.firstDay.julianDayNumber
      }
    }

    LinearInterpolation.interpolate(months, prices, Month(2014, 1)) shouldBe 10.0
    LinearInterpolation.interpolate(months, prices, Month(2015, 1)) shouldBe 10.0

    val ratio = (Month(2015, 2).firstDay - Month(2015, 3).firstDay).toDouble /
      (Month(2015, 2).firstDay - Month(2015, 4).firstDay).toDouble
    val interPrice = 20 + (40 - 20) * ratio
    LinearInterpolation.interpolate(months, prices, Month(2015, 3)) shouldBe interPrice

    LinearInterpolation.interpolate(months, prices, Month(2016, 1)) shouldBe 50.0
  }

  test("look back interpolation") {
    val months = Vector(Month(2015, 1), Month(2015, 2), Month(2015, 4), Month(2015, 5))
    val prices = Vector(10.0, 20.0, 40.0, 50.0)

    LookBackInterpolation.interpolate(months, prices, Month(2014, 1)) shouldBe 10.0
    LookBackInterpolation.interpolate(months, prices, Month(2015, 1)) shouldBe 10.0
    LookBackInterpolation.interpolate(months, prices, Month(2015, 3)) shouldBe 20.0
    LookBackInterpolation.interpolate(months, prices, Month(2016, 1)) shouldBe 50.0
  }
}
