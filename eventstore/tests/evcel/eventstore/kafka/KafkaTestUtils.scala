package evcel.eventstore.kafka

import java.net.ServerSocket
import java.util.Properties
import java.io.File
import kafka.api.Request
import kafka.producer.{DefaultPartitioner, Producer, ProducerConfig}
import kafka.serializer.DefaultEncoder
import kafka.server.KafkaServer
import kafka.server.KafkaConfig
import kafka.utils.ZkUtils
import org.I0Itec.zkclient.ZkClient
import org.apache.zookeeper.server.{NIOServerCnxnFactory, ZooKeeperServer, NIOServerCnxn}
import java.net.InetSocketAddress
import evcel.utils.FileUtils
import kafka.admin.AdminUtils

/**
 * Mostly copied from Kafka's own unit tests
 */
object KafkaTestUtils {

  def withTestKafka(fn: KafkaServer => Unit) {
    val zookeeperPort = choosePort
    val zookeeper = new EmbeddedZookeeper(zookeeperPort)
    val kafkaLogDir = tempDir("kafka-test")
    val server = new KafkaServer(brokerConfig(kafkaLogDir, zookeeperPort))
    try {
      server.startup()
      fn(server)
    } finally {
      server.shutdown()
      server.awaitShutdown()
      zookeeper.shutdown()
      FileUtils.recursiveDelete(kafkaLogDir)
    }
  }

  def createTopic(server: KafkaServer, topic: String, numPartitions: Int = 1) {
    AdminUtils.createTopic(server.zkClient, topic, partitions = numPartitions, replicationFactor = 1)
    (0 until numPartitions).map { case i =>
      waitUntilMetadataIsPropagated(server :: Nil, topic, i)
      i -> waitUntilLeaderIsElectedOrChanged(server.zkClient, topic, i)
    }.toMap
  }

  /**
   * Wait until a valid leader is propagated to the metadata cache in each broker.
   * It assumes that the leader propagated to each broker is the same.
   * @param servers The list of servers that the metadata should reach to
   * @param topic The topic name
   * @param partition The partition Id
   * @param timeout The amount of time waiting on this condition before assert to fail
   * @return The leader of the partition.
   */
  private def waitUntilMetadataIsPropagated(servers: Seq[KafkaServer], topic: String, partition: Int, timeout: Long = 5000L): Int = {
    var leader: Int = -1
    waitUntilTrue(() =>
      servers.foldLeft(true) {
        (result, server) =>
          val partitionStateOpt = server.apis.metadataCache.getPartitionInfo(topic, partition)
          partitionStateOpt match {
            case None => false
            case Some(partitionState) =>
              leader = partitionState.leaderIsrAndControllerEpoch.leaderAndIsr.leader
              result && Request.isValidBrokerId(leader)
          }
      },
      "Partition [%s,%d] metadata not propagated after %d ms".format(topic, partition, timeout),
      waitTime = timeout)

    leader
  }

  /**
   * Wait until the given condition is true or throw an exception if the given wait time elapses.
   */
  private def waitUntilTrue(condition: () => Boolean, msg: String, waitTime: Long = 5000L): Boolean = {
    val startTime = System.currentTimeMillis()
    while (true) {
      if (condition())
        return true
      if (System.currentTimeMillis() > startTime + waitTime)
        sys.error(msg)
      Thread.sleep(waitTime.min(100L))
    }
    // should never hit here
    throw new RuntimeException("unexpected error")
  }

  /**
   * If neither oldLeaderOpt nor newLeaderOpt is defined, wait until the leader of a partition is elected.
   * If oldLeaderOpt is defined, it waits until the new leader is different from the old leader.
   * If newLeaderOpt is defined, it waits until the new leader becomes the expected new leader.
   * @return The new leader or assertion failure if timeout is reached.
   */
  private def waitUntilLeaderIsElectedOrChanged(zkClient: ZkClient, topic: String, partition: Int, timeoutMs: Long = 5000L,
                                                oldLeaderOpt: Option[Int] = None, newLeaderOpt: Option[Int] = None): Option[Int] = {
    require(!(oldLeaderOpt.isDefined && newLeaderOpt.isDefined), "Can't define both the old and the new leader")
    val startTime = System.currentTimeMillis()
    var isLeaderElectedOrChanged = false

    var leader: Option[Int] = None
    while (!isLeaderElectedOrChanged && System.currentTimeMillis() < startTime + timeoutMs) {
      // check if leader is elected
      leader = ZkUtils.getLeaderForPartition(zkClient, topic, partition)
      leader match {
        case Some(l) =>
          if (newLeaderOpt.isDefined && newLeaderOpt.get == l) {
            isLeaderElectedOrChanged = true
          } else if (oldLeaderOpt.isDefined && oldLeaderOpt.get != l) {
            isLeaderElectedOrChanged = true
          } else if (!oldLeaderOpt.isDefined) {
            isLeaderElectedOrChanged = true
          } else {
          }
        case None =>
      }
      Thread.sleep(timeoutMs.min(100L))
    }
    if (!isLeaderElectedOrChanged)
      sys.error("Timing out after %d ms since leader is not elected or changed for partition [%s,%d]"
        .format(timeoutMs, topic, partition))

    leader
  }

  def choosePort: Int = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort
    socket.close()
    port
  }

  def brokerConfig(logDir: File, zookeeperPort: Int) = {
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

  def tempDir(name: String) = {
    val dir = File.createTempFile(name, "")
    dir.delete
    dir.mkdirs
    dir.deleteOnExit()
    dir
  }

  class EmbeddedZookeeper(port: Int) {
    val connectString = "127.0.0.1:" + port
    val snapshotDir = tempDir("zookeeper-snapshot")
    val logDir = tempDir("zookeeper-log")
    val tickTime = 500
    val zookeeper = new ZooKeeperServer(snapshotDir, logDir, tickTime)
    val factory = new NIOServerCnxnFactory()
    factory.configure(new InetSocketAddress("127.0.0.1", port), 10)
    factory.startup(zookeeper)

    def shutdown() {
      factory.shutdown()
      zookeeper.shutdown()
      FileUtils.recursiveDelete(snapshotDir)
      FileUtils.recursiveDelete(logDir)
    }

  }

}

