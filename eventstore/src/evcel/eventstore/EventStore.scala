package evcel.eventstore

import java.net.ConnectException
import java.util.{TreeMap => JavaTreeMap, UUID}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.concurrent.{Future, Promise}
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference



abstract class EventStore[K, V](topic : String, kafkaPort : Int){
  import EventStore._

  /**
    * History is updated only after writes have successfully been written to the kafka store
    */
  private val history = new ConcurrentHashMap[K, JavaTreeMap[Offset, V]]()
  /**
    * Whereas latestVersions is updated immediately upon a write. It's purpose
    * is only to prevent storing of duplicates
    */
  private val latestVersions = new ConcurrentHashMap[K, V]()

  /**
    * Defines the latest surface stored in memory. May be less than the
    * last offset in kafka if we are in the middle of a write
    */
  private val latestOffsetReference = new AtomicReference(Offset(-1))

  private val SYNC_POINT = "SYNC_POINT"
  private val log = LoggerFactory.getLogger(this.getClass)
  private val writer = EventStoreProducer(topic, kafkaPort)

  private val promisesToWrite = new ConcurrentHashMap[UUID_String, Promise[Offset]]()

  private val monitor = new Object
  private var isLive = new AtomicBoolean(true)

  private val consumer = new EventStoreConsumer(topic, kafkaPort)
  syncWithKafka()


  def write(map : Map[K, V]): Future[Offset] = {
    val promise = Promise[Offset]()
    val updates = map.filterNot{
      case (key, value) => 
        latestVersions.get(key) == value
    }
    if (updates.isEmpty){
      promise.success(latestOffset())
    } else {
      latestVersions ++= updates
      val uuid = UUID.randomUUID.toString
      val message = createMessage(uuid, updates)
      promisesToWrite.put(uuid, promise)
      writer.send(message)
      notifyReaderThread()
    }
    promise.future
  }

  def write(key : K, value : V) : Future[Offset] = write(Map(key -> value))

  def shutdown(){
    isLive.set(false)
    notifyReaderThread()
    reader.join
    log.info("Calendar store shut down")
  }


  def latestOffset(): Offset = latestOffsetReference.get()

  def allVersions(key : K) : SortedMap[Offset, V] = {
    TreeMap[Offset, V]() ++ Option(history.get(key)).getOrElse(new JavaTreeMap[Offset, V])
  }


  def read(offset: Offset, key : K) : Either[EventStore.NotFound[K], V] = {
    if (offset > latestOffset())
      throw new RuntimeException(
        s"$topic event store asked for offset/key $offset/$key when latest is $latestOffset()")
    require(offset.value >= 0, s"$topic offset must be non-negative")


    val maybeValue = Option(history.get(key)).flatMap{
      treeMap => 
        Option(treeMap.headMap(offset, true).lastEntry).map(_.getValue)
    }
    maybeValue.toRight(left = EventStore.NotFound(topic, key, offset))
  }

  protected def parseMessage(message : String) : (UUID_String, Map[K, V])
  protected def createMessage(uuid : UUID_String, map : Map[K, V]) : String



  private def processMessage(offset : Offset, message : String){
    /** 
      * Completes (sucessfully) any waiting write Futures
      */
    def fulfillPromise(uuid : UUID_String){
      if (promisesToWrite.containsKey(uuid)) {
        promisesToWrite.get(uuid).success(offset)
        promisesToWrite.remove(uuid)
      }
    }
    def updateHistory(updates : Map[K, V]){
      updates.foreach{
        case (key, value) => 
          history.putIfAbsent(key, new JavaTreeMap[Offset, V]())
          history.get(key).put(offset, value)
      }
    }
    def incrementOffset(){
      require(
        offset.value == latestOffset().value + 1, 
        s"Received a message out of order. Got $offset when latest is ${latestOffset()}"
      )
      latestOffsetReference.set(offset)
    }

    incrementOffset()

    if (message.startsWith(SYNC_POINT + ":"))
      return

    val (uuid, updates) = parseMessage(message)
    updateHistory(updates)
    fulfillPromise(uuid)
  }

  def syncWithKafka() : Unit = {
    val syncMessage = SYNC_POINT + ":" + UUID.randomUUID

    writer.send(syncMessage)
    var haveSynced = false
    while (! haveSynced){
      val messages = consumer.readMessages()
      messages.foreach{
        case (offset, message) => 
          if (message == syncMessage)
            haveSynced = true
          processMessage(Offset(offset), message)
      }
    }
  }

  private val reader : Thread = new Thread() {


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
                processMessage(Offset(offset), message)
            }
          } catch {

            case e : ConnectException if numberOfFailedConnections < 5 => 
              log.error(topic + " reader got failed connection to Kafka - will retry")
              numberOfFailedConnections += 1
              monitor.wait(1000)

            case other : Exception => 
              log.error(s" unexpected exception in $topic reader - will shut down")
              throw other
          }

        }
        consumer.close
      }
    }
  }
  reader.setName(topic)
  reader.setDaemon(true)
  reader.start

  private def notifyReaderThread(){
    monitor.synchronized {
      monitor.notify()
    }
  }
}

object EventStore{
  type UUID_String = String
  case class Offset(value : Long) extends AnyVal with Ordered[Offset]{
    def compare(rhs : Offset) = value.compare(rhs.value)
  }
  case class NotFound[K](topic : String, key : K, offset : Offset)
}


