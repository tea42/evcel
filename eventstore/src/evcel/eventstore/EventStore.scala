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



abstract class EventStore[K, V](topic : String, kafkaPort : Int){
  import EventStore._

  def write(map : Map[K, V]): Future[Offset] = {
    val uuid = UUID.randomUUID.toString
    val message = createMessage(uuid, map)
    val promise = Promise[Offset]()
    promisesToWrite.put(uuid, promise)
    writer.send(message)
    notifyReaderThread()
    promise.future
  }

  def write(key : K, value : V) : Future[Offset] = write(Map(key -> value))

  def shutdown(){
    isLive.set(false)
    notifyReaderThread()
    reader.join
    log.info("Calendar store shut down")
  }


  def latestVersion(): Offset = {
    var latest = -1
    if (history.size() == 0) Offset(-1)
    else history.map{
      case (_, treeMap) => treeMap.lastKey
    }.max
  }

  def read(offset: Offset, key : K) : Either[EventStore.NotFound[K], V] = {
    if (offset > latestVersion())
      throw new RuntimeException(
        s"$topic event store asked for offset/key $offset/$key when latest is $latestVersion()")
    require(offset.value >= 0, s"$topic offset must be non-negative")


    val maybeValue = Option(history.get(key)).flatMap{
      treeMap => 
        Option(treeMap.headMap(offset, true).lastEntry).map(_.getValue)
    }
    maybeValue.toRight(left = EventStore.NotFound(topic, key, offset))
  }

  protected def parseMessage(message : String) : (UUID_String, Map[K, V])
  protected def createMessage(uuid : UUID_String, map : Map[K, V]) : String

  private val log = LoggerFactory.getLogger(this.getClass)
  private val writer = EventStoreProducer(topic, kafkaPort)
  private val history = new ConcurrentHashMap[K, JavaTreeMap[Offset, V]]()

  private val promisesToWrite = new ConcurrentHashMap[UUID_String, Promise[Offset]]()

  private val monitor = new Object
  private var isLive = new AtomicBoolean(true)


  private val reader : Thread = new Thread() {

    /** 
      * Completes (sucessfully) any waiting write Futures
      */
    def fulfillPromise(uuid : UUID_String, offset : Offset){
      if (promisesToWrite.containsKey(uuid)) {
        promisesToWrite.get(uuid).success(offset)
        promisesToWrite.remove(uuid)
      }
    }
    private val consumer = new EventStoreConsumer(topic, kafkaPort)

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
                val (uuid, map) = parseMessage(message)
                map.foreach{
                  case (key, value) => 
                    history.putIfAbsent(key, new JavaTreeMap[Offset, V]())
                    history.get(key).put(Offset(offset), value)
                }
                fulfillPromise(uuid, Offset(offset))
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
  //case class UUID_String(value : String) extends AnyVal
  //implicit val uuidStringFormat = jsonFormat1(UUID_String)
  case class NotFound[K](topic : String, key : K, offset : Offset)
}


