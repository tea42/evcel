package evcel.calendarstore

import evcel.calendar.SimpleCalendar
import evcel.calendar.Calendar._
import evcel.daterange.DateRangeSugar._
import evcel.daterange.Day
import evcel.eventstore.json.EventStoreJsonProtocol._
import evcel.eventstore.kafka.KafkaTestUtils
import kafka.admin.AdminUtils
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.slf4j.LoggerFactory
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.{postfixOps, reflectiveCalls}
import spray.json._
import evcel.eventstore.EventStore._
import evcel.eventstore.EventStore
import CalendarStore._

class CalendarStoreTests extends FunSpec with Matchers {
  val log = LoggerFactory.getLogger(this.getClass)

  def withCalendarStore(fn : CalendarStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        AdminUtils.createTopic(server.zkClient, CalendarStore.TOPIC, partitions = 1, replicationFactor = 1)
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
          val holidays = Holidays(Set(10 / Aug / 2014))

          info("It should be readable")
          Await.result(cs.write(CalendarName("UK"), holidays), 2 seconds) should equal(Offset(0))
          cs.read(Offset(0), CalendarName("UK")) should equal(Right(holidays))
      }
    }

    it("An empty calendar store"){
      withCalendarStore{
        cs => 
          info("It's latest version should be -1")
          cs.latestVersion() should equal(Offset(-1))

          info("Should throw an exception when asked for data")
          intercept[RuntimeException] {
            cs.read(Offset(0), CalendarName("UK"))
          }
          intercept[IllegalArgumentException] {
            cs.read(Offset(-1), CalendarName("UK"))
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
                CalendarName("UK") -> Holidays(Set(day0 + i, day0 + i + 1)),
                CalendarName("US") -> Holidays(Set(day0 + i + 10, day0 + i + 11)),
                CalendarName("EU") -> Holidays(Set[Day]())
              )
          }
          holidays.foreach(cs.write _)
          Await.result(cs.write(Map(CalendarName("UK") -> Holidays(Set.empty), CalendarName("US") -> Holidays(Set.empty))), 5 seconds) should equal(Offset(10))
          for (i <- 0 until 10){
            cs.readCalendar(Offset(i), CalendarName("UK")) should equal(Right(SimpleCalendar(holidays(i)(CalendarName("UK")))))
            cs.readCalendar(Offset(i), CalendarName("BLAH")) should equal (Left(EventStore.NotFound(CalendarStore.TOPIC, CalendarName("BLAH"), Offset(i))))
          }
      }
    }
  }
}
