package evcel.calendarstore

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import scala.concurrent.Await
import scala.concurrent.duration._
import evcel.daterange.Day
import spray.json._
import CalendarTypeAliases._
import evcel.calendar.SimpleCalendar
import evcel.eventstore.json.EventStoreJsonProtocol._
import scala.concurrent.Future
import scala.util.control.Breaks._
import scala.language.reflectiveCalls
import scala.language.postfixOps

class CalendarStoreTests extends FunSpec with Matchers {
  describe("Calendar store") {
    ignore("When empty") {
      val cs = CalendarStore(topic = "CalendarStoreTests.empty")

      info("It's latest version should be -1")
      cs.latestVersion() should equal(-1)

      info("Should throw an exception when asked for data")
      intercept[CalendarStoreUsageException] {
        cs.read(0, "UK")
      }
      intercept[IllegalArgumentException] {
        cs.read(-1, "UK")
      }
    }

    ignore("with a single write") {
      val cs = CalendarStore(topic = "CalendarStoreTests.oneWrite")
      val holidays = Set(10 / Aug / 2014)

      info("It should be readable")
      Await.result(cs.write(Map("UK" -> holidays)), 2 seconds) should equal(0)
      cs.read(0, "UK") should equal(SimpleCalendar(holidays))
    }

    ignore("With several writes") {
      val cs = CalendarStore(topic = "CalendarStoreTests.severalWrites")
      val day0 = 10 / Aug / 2014
      val holidays = (0 until 10).toList.map {
        i =>
          Map(
            "UK" -> Set(day0 + i, day0 + i + 1),
            "US" -> Set(day0 + i + 10, day0 + i + 11),
            "EU" -> Set[Day]()
          )
      }
      holidays.foreach(cs.write _)
      Await.result(cs.write(Map("UK" -> Set.empty, "US" -> Set.empty)), 5 seconds) should equal(10)
      for (i <- 0 until 10)
        cs.read(i, "UK") should equal(SimpleCalendar(holidays(i)("UK")))
    }
  }
}
