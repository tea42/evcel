package evcel.eventstore
import kafka.consumer.SimpleConsumer
import kafka.api.FetchRequestBuilder

case class EventStoreConsumer(topic: String, kafkaPort: Int) {
  var offset: Long = 0
  val partition = 0
  val groupID = "0"
  private val consumer = new SimpleConsumer(
    "localhost", kafkaPort,
    soTimeout = 100000,
    bufferSize = 64 * 1024,
    clientId = groupID
  )

  def close = consumer.close()

  def read(): List[String] = {
    var collected: List[String] = Nil
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
        collected ::= msg
      case _ =>
    }
    collected.reverse
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
}
