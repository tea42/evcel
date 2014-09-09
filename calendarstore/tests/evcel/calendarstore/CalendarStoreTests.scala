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
import evcel.eventstore.kafka.KafkaTestUtils
import kafka.admin.AdminUtils
import org.slf4j.LoggerFactory
import kafka.server.KafkaServer

class CalendarStoreTests extends FunSpec with Matchers {
  val log = LoggerFactory.getLogger(this.getClass)

  def withCalendarStore(fn : CalendarStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        if (!AdminUtils.topicExists(server.zkClient, "Calendars"))
          AdminUtils.createTopic(server.zkClient, "Calendars", partitions = 1, replicationFactor = 1)
        val cs = CalendarStore(kafkaPort = server.config.port)
        try{
          Thread.sleep(100) // Prevents 'Failed to collate messages by topic' error message from Kafka,
                            // which is just noise, as Kafka invariably retries successfully
          fn(cs)
        } finally {
          cs.shutdown()
        }
    }
  }

  describe("Calendar store") {
    it("with a single write") {
      withCalendarStore{
        cs => 
          val holidays = Set(10 / Aug / 2014)

          info("It should be readable")
          Await.result(cs.write(Map("UK" -> holidays)), 2 seconds) should equal(0)
          cs.read(0, "UK") should equal(SimpleCalendar(holidays))
      }
    }

    it("An empty calendar store"){
      withCalendarStore{
        cs => 
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
    }


    it("With several writes") {
      withCalendarStore{
        cs => 

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
}
