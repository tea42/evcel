package evcel.calendarstore

import CalendarTypeAliases._
import java.util.concurrent.ConcurrentSkipListMap
import evcel.daterange.Day
import evcel.calendar.SimpleCalendar
import scala.concurrent.Future
import scala.concurrent.Promise
import java.util.concurrent.ConcurrentHashMap
import spray.json._
import java.util.UUID
import evcel.eventstore.EventStoreConsumer
import evcel.daterange.DateRangeSugar._
import evcel.eventstore.EventStoreProducer
import evcel.eventstore.json.EventStoreJsonProtocol._
import java.util.concurrent.LinkedBlockingQueue
import evcel.eventstore.EventStoreTypeAliases._

case class CalendarStoreUsageException(msg: String) extends RuntimeException

case class CalendarStore(topic: String = "Calendars", groupID: String = "0", kafkaPort: Int = 9092) {
  val writer = EventStoreProducer(topic, kafkaPort)
  private val updates = new ConcurrentSkipListMap[Offset, Map[Name, Holidays]]()
  private val promisesToWrite = new ConcurrentHashMap[UUID_String, Promise[Offset]]()

  private val monitor = new Object
  val eventStoreReader = new Thread() {
    val consumer = new EventStoreConsumer(topic, groupID, kafkaPort)
    override def run() {
      var lastReadEmpty = false
      monitor.synchronized {
        while (true) {
          if (lastReadEmpty)
            monitor.wait(1000)
          val messages = consumer.readMessages()
          lastReadEmpty = messages.isEmpty
          messages.foreach {
            case (offset, message) =>
              val json = message.parseJson
              val (uuid, holidayMap) = json.convertTo[(UUID_String, Map[Name, Holidays])]
              updates.put(offset, holidayMap)
              if (promisesToWrite.containsKey(uuid)) {
                promisesToWrite.get(uuid).success(offset)
                promisesToWrite.remove(uuid)
              }

          }

        }
      }
    }
  }
  eventStoreReader.setName("CalendarStoreReader")
  eventStoreReader.setDaemon(true)
  eventStoreReader.start

  def write(data: Map[Name, Holidays]): Future[Offset] = {
    val uuid = UUID.randomUUID.toString
    val json = (uuid, data).toJson.prettyPrint
    val promise = Promise[Long]()
    promisesToWrite.put(uuid, promise)
    // Promise is sent back to this after DB write
    writer.send(json)
    monitor.synchronized {
      monitor.notify()
    }
    promise.future
  }

  def latestVersion(): Offset = {
    if (updates.size() == 0) -1
    else updates.lastKey()
  }

  def read(version: Offset, name: String) = {
    if (version > latestVersion())
      throw new CalendarStoreUsageException(
        s"CalendarStore asked for version/name $version/$name when latest is $latestVersion()")
    require(version >= 0, "Calendar version must be non-negative")
    var holidays = Set[Day]()
    var found = false
    val iter = updates.headMap(version, true).descendingKeySet.iterator
    while (iter.hasNext && !found) {
      val map: Map[Name, Holidays] = updates.get(iter.next)
      map.get(name) match {
        case Some(hols) =>
          holidays = hols
          found = true
        case None =>
      }
    }
    SimpleCalendar(holidays)
  }
}
