package evcel.eventstore

import kafka.producer.{ Producer, ProducerConfig, KeyedMessage }
import java.util.Properties

case class EventStoreProducer(topic: String, kafkaPort: Int) {

  private val producer = EventStoreProducer.buildProducer(kafkaPort)

  def send(msg: String) {
    val data = new KeyedMessage[String, String](topic, msg);
    producer.send(data)
  }
  def close(){
    producer.close()
  }
}

object EventStoreProducer {
  private def buildProducer(kafkaPort: Int) = {
    val props = new Properties()

    props.put("metadata.broker.list", s"localhost:$kafkaPort")
    props.put("serializer.class", "kafka.serializer.StringEncoder")
    props.put("request.required.acks", "1")

    new Producer[String, String](new ProducerConfig(props))
  }
}
