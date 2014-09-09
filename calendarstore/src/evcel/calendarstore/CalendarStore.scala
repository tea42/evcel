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
import org.slf4j.LoggerFactory
import java.net.ConnectException
import java.util.concurrent.atomic.AtomicBoolean

case class CalendarStoreUsageException(msg: String) extends RuntimeException

/**
  * Threading model
  *
  * Writes happen on the main thread. These return a future, which completes only
  * once the written data has been read back form the Kafka store and put into 
  * memory. The future contains the kafka offset of the data written.
  *
  * The reader thread was designed to
  *   a) React as quickly as possible to any writes
  *   b) Check periodically whether shutdown is required
  *
  */

case class CalendarStore(kafkaPort: Int) {
  private val topic = CalendarStore.TOPIC
  private val log = LoggerFactory.getLogger(this.getClass)
  private val writer = EventStoreProducer(topic, kafkaPort)
  private val updates = new ConcurrentSkipListMap[Offset, Map[Name, Holidays]]()

  private val promisesToWrite = new ConcurrentHashMap[UUID_String, Promise[Offset]]()

  private val monitor = new Object
  private var isLive = new AtomicBoolean(true)

  private val eventStoreReader = new Thread() {

    private val consumer = new EventStoreConsumer(topic, kafkaPort)
    /** 
      * Completes (sucessfully) any waiting write Futures
      */
    def fulfillPromise(uuid : UUID_String, offset : Long){
      if (promisesToWrite.containsKey(uuid)) {
        promisesToWrite.get(uuid).success(offset)
        promisesToWrite.remove(uuid)
      }
    }

    override def run() {
      var lastReadEmpty = false
      var numberOfFailedConnections = 0
      monitor.synchronized {
        while (isLive.get()) {
          // If the last read was non-empty there is a good chance there is more waiting to be read
          // Otherwise yield the monitor lock
          if (lastReadEmpty) monitor.wait(1000)

          try {
            val messages = consumer.readMessages()
            lastReadEmpty = messages.isEmpty
            messages.foreach {
              case (offset, message) =>
                val (uuid, holidayMap) = message.parseJson.convertTo[(UUID_String, Map[Name, Holidays])]
                updates.put(offset, holidayMap)
                fulfillPromise(uuid, offset)
            }
          } catch {

            case e : ConnectException if numberOfFailedConnections < 5 => 
              log.error("CalendarStore reader got failed connection to Kafka - will retry")
              numberOfFailedConnections += 1
              monitor.wait(1000)

            case other : Exception => 
              log.error("Unexpected exception in CalendarStore reader - will shut down")
              throw other
          }

        }
        consumer.close
      }
    }
  }
  eventStoreReader.setName("CalendarStoreReader")
  eventStoreReader.setDaemon(true)
  eventStoreReader.start

  private def notifyReaderThread(){
    monitor.synchronized {
      monitor.notify()
    }
  }

  def shutdown(){
    isLive.set(false)
    notifyReaderThread()
    eventStoreReader.join
    log.info("Calendar store shut down")
  }

  def write(data: Map[Name, Holidays]): Future[Offset] = {
    val uuid = UUID.randomUUID.toString
    val json = (uuid, data).toJson.prettyPrint
    val promise = Promise[Long]()
    promisesToWrite.put(uuid, promise)
    writer.send(json)
    notifyReaderThread()
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

object CalendarStore{
  val TOPIC = "Calendars"
}
