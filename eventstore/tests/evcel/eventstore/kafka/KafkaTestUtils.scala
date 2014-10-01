package evcel.eventstore.kafka

import java.net.ServerSocket
import java.util.Properties
import java.io.File
import kafka.server.KafkaServer
import kafka.server.KafkaConfig
import org.apache.zookeeper.server.ZooKeeperServer
import org.apache.zookeeper.server.NIOServerCnxn
import java.net.InetSocketAddress
import evcel.utils.FileUtils
import kafka.admin.AdminUtils

/**
  * Mostly copied from Kafka's own unit tests
  */
object KafkaTestUtils{

  def withTestKafka(fn : KafkaServer => Unit){
    val zookeeperPort = choosePort
    val zookeeper = new EmbeddedZookeeper(zookeeperPort)
    val kafkaLogDir = tempDir("kafka-test")
    val server = new KafkaServer(brokerConfig(kafkaLogDir, zookeeperPort))
    try {
      server.startup()
      // This pause prevents some of the harmless but noisy log error messages that Kafka produces
      // before it's quite ready to do its thing
      Thread.sleep(100)
      fn(server)
    } finally {
      server.shutdown()
      zookeeper.shutdown()
      FileUtils.recursiveDelete(kafkaLogDir)
    }
  }

  def createTopic(server : KafkaServer, topic : String){
    AdminUtils.createTopic(server.zkClient, topic, partitions = 1, replicationFactor = 1)
    Thread.sleep(200) // Prevents 'Failed to collate messages by topic' error message from Kafka,
                      // which is just noise, as Kafka invariably retries successfully
  }

  private def choosePort : Int = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort
    socket.close
    port
  }

  private def brokerConfig(logDir : File, zookeeperPort : Int) = {
    val props = new Properties
    props.put("broker.id", "0")
    props.put("host.name", "localhost")
    props.put("port", s"$choosePort")
    props.put("log.dir", logDir.getAbsolutePath)
    props.put("zookeeper.connect", s"127.0.0.1:$zookeeperPort")
    props.put("replica.socket.timeout.ms", "1500")
    props.put("log.retention.hours", Int.MaxValue.toString)
    new KafkaConfig(props)
  }
 
  def tempDir(name : String) = {
    val dir = File.createTempFile(name, "")
    dir.delete
    dir.mkdirs
    dir.deleteOnExit
    dir
  }

  class EmbeddedZookeeper(port : Int) {
    val snapshotDir = tempDir("zookeeper-snapshot")
    val logDir = tempDir("zookeeper-log")
    val tickTime = 500
    val zookeeper = new ZooKeeperServer(snapshotDir, logDir, tickTime)
    val factory = new NIOServerCnxn.Factory(new InetSocketAddress("127.0.0.1", port))
    factory.startup(zookeeper)

    def shutdown() {
      zookeeper.shutdown()
      factory.shutdown()
      FileUtils.recursiveDelete(snapshotDir)
      FileUtils.recursiveDelete(logDir)
    }

  }
}

