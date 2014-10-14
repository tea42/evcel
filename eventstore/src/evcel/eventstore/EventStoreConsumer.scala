package evcel.eventstore
import kafka.consumer.SimpleConsumer
import kafka.api.FetchRequestBuilder
import java.util.concurrent.atomic.AtomicBoolean
import java.net.ConnectException
import org.slf4j.LoggerFactory

abstract class EventStoreConsumer(
  topic: String, 
  kafkaPort: Int,
  isLive : AtomicBoolean
) {
  private val log = LoggerFactory.getLogger(this.getClass)
  var offset: Long = 0
  val partition = 0
  val groupID = "0"
  val monitor = new Object
  private val consumer = new SimpleConsumer(
    "localhost", kafkaPort,
    soTimeout = 100000,
    bufferSize = 64 * 1024,
    clientId = groupID
  )

  def onMessage(offset : Long, message : String)
  def close() = consumer.close()
  def notifyReaderThread(){
    monitor.synchronized {
      monitor.notify()
    }
  }

  def readMessages(): List[(Long, String)] = {
    var collected: List[(Long, String)] = Nil
    val req = new FetchRequestBuilder()
      .clientId(groupID)
      // Note: this fetchSize of 100000 might need to be increased if large batches are written to Kafka
      .addFetch(topic, partition, offset, fetchSize = 100000)
      .build()
    val fetchResponse = consumer.fetch(req)
    val messageSet = fetchResponse.messageSet(topic, partition)
    messageSet.foreach {
      case messageAndOffset if messageAndOffset.nextOffset > offset =>
        offset = messageAndOffset.nextOffset
        val payload = messageAndOffset.message.payload
        val bytes = new Array[Byte](payload.limit)
        payload.get(bytes)
        val msg = new String(bytes)
        collected ::= (messageAndOffset.offset, msg)
      case _ =>
    }
    collected.reverse
  }

  val thread = new Thread() {
    override def run() {
      var lastReadEmpty = false
      var numberOfFailedConnections = 0
      try {
        monitor.synchronized{
          while (isLive.get()) {
            // If the last read was non-empty there is a good chance there is more waiting to be read
            // Otherwise yield the monitor lock
            if (lastReadEmpty) 
              monitor.wait(1000)

            try {
              val messages = readMessages()
              lastReadEmpty = messages.isEmpty
              messages.foreach {
                case (offset, message) =>
                  onMessage(offset, message)
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
        }
      } finally {
        close()
      }
    }
  }
  thread.setName(topic)
  thread.setDaemon(true)
  thread.start
}
