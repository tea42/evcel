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
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.Await
import java.util.Collections
import scala.language.reflectiveCalls
import scala.language.postfixOps

/**
  * Multi-threaded code tends to be resistent to making the underlying intent clear, hence the
  * extensive comment.
  *
  * API
  *
  *   [ Note that this API description is for the purpose of explaining the design. It ignores
  *     error handling, helper functions etc ]
  *
  *   The EventStore contains an in memory store, conceptually of type
  *   
  *       Map[K, SortedMap[Offset, V]].
  *  
  *    The write API is
  *  
  *       def write(entities : Map[K, V]) : Promise[Offset].
  *  
  *    The read API is (ignoring error handling)
  *  
  *       def read(offset : Offset, key : Key) : V.
  *  
  *     The promises returned from writes will not complete until the entity has been written to Kafka, 
  *     read back out again, and inserted into memory.
  *  
  * Design Requirements
  *
  *   For reasons of performance and sanity, the EventStore has been designed to have the
  *   following properties.
  *   
  *   1. Writes aren't synchronised, instread all write requests are immediately queued. This
  *      grants an order to the writes that is preserved. Messages are sent to Kafka in this order, 
  *      and promises are completed in the same order. 
  *   2. Writes are deduplicated - any identical entities in consecutive writes are
  *      silently dropped. If this results in an empty write then the Promise completes
  *      with the offset of the nearest preceding non-empty write.
  *   3. Serialization and deserialization is done in parallel
  *
  * How writes are performed
  *
  *   Work is passed along a number of queues. 
  *  
  *   Immediately following a write request, the map of entities being written is added to 
  *   a `deduplicator` queue. This queue determines an order to the write requests. Its job is to 
  *   remove from the map any entities that would already have been in the latest surface as at the point 
  *   of the previous request.
  *   [ Note that deduplication can result in _all_ entities being dropped from that request, in which case 
  *     nothing is written to Kafka, however the resulting promise is _still_ completed in order. Its completed 
  *     value will be that of the previous non-empty  write. ]
  *   The deduplicator passes work along to the 'serializer' and 'publisher' queues. 
  *  
  *   The `serializer` queue is a queue of Futures. These futures just do the JSON serialization.
  *   Whenever the head Future is complete the serializer then writes the json entity to Kafka, thus
  *   guaranteeing that write order is preserved.
  *   
  *   The `reader` is another thread - without its own queue. Its job is to poll Kafka, passing the 
  *   serialized entities read to the `deserializer` queue. Similarly to the `serializer`, the deserializer 
  *   achieves order preserving parallelization with a queue of Futures. On completion of the head Future
  *   work is passed to the `publisher` - in order to finally complete the promise that was returned from the 
  *   original write. The publisher's work could in theory be done by the deserializer, while satisfying the
  *   above requirements, however the publisher handles a complexity due to empty writes (post deduplication) 
  *   that would have been more confusing if it were combined.
  */
abstract class EventStore[K, V](topic : String, kafkaPort : Int){
  import EventStore._
  import scala.concurrent.ExecutionContext.Implicits.global
  private val log = LoggerFactory.getLogger(this.getClass)

  /**
    * History is updated only after writes have successfully been written to the Kafka store
    */
  private val history = new ConcurrentHashMap[K, JavaTreeMap[Offset, V]]()

  private val haveCompletedInitialLoad = new AtomicBoolean(false)

  /**
    * Defines the latest surface stored in memory. May be less than the
    * last offset in Kafka if we are in the middle of a write
    */
  private val latestOffsetInHistory = new AtomicReference(Offset(0))
  def lastOffsetInHistory(): Offset = latestOffsetInHistory.get()


  /**
    * When switched to false all the associated threads will 
    * terminate
    */
  private val isLive = new AtomicBoolean(true)

  /*
   * Before any writes occur, we must first bring the history up to date 
   * with the Kafka store - this is done by writing a message, and reading
   * from Kafka until it is returned
   */
  private val publisher = makePublisher()
  private val deserializer = makeDeserializer()
  private val reader = new EventStoreConsumer(topic, kafkaPort, isLive){
    def onMessage(offset : Long, message : String) {
      deserializer.enqueueFuture(offset, message)
    }
  }
  val writer = EventStoreProducer(topic, kafkaPort)
  private val syncMessage = SYNC_POINT + UUID.randomUUID
  writer.send(syncMessage)

  while(! haveCompletedInitialLoad.get){
    Thread.sleep(100)
  }

  /*
   * At this point we are up to date with Kafka, so writes
   * can begin.
   * Anything that was passed to the publisher during initialization was
   * associated with writes to Kafka that pre-date this process, and so
   * should be removed.
   */
  publisher.lastOffsetPublished.set(latestOffsetInHistory.get)
  publisher.writesToBePublished.clear

  private val serializer = makeSerializer()
  private val deduplicator = makeDeduplicator()
    
  protected def parseMessage(message : String) : (UUID_String, Map[K, V])
  protected def createMessage(uuid : UUID_String, map : Map[K, V]) : String

  def write(map : Map[K, V]): Future[Offset] = {
    val promise = Promise[Offset]()
    deduplicator.enqueue((promise, map))
    promise.future
  }

  def write(key : K, value : V) : Future[Offset] = write(Map(key -> value))
    
  def read(offset: Offset, key : K) : Either[EventStore.NotFound[K], V] = {
    if (offset > lastOffsetInHistory())
      throw new RuntimeException(
        s"$topic event store asked for offset/key $offset/$key when latest is $lastOffsetInHistory()")
    require(offset.value >= 0, s"$topic offset must be non-negative")


    val maybeValue = Option(history.get(key)).flatMap{
      treeMap => 
        Option(treeMap.headMap(offset, true).lastEntry).map(_.getValue)
    }
    maybeValue.toRight(left = EventStore.NotFound(topic, key, offset))
  }

  def allVersions(key : K) : SortedMap[Offset, V] = {
    TreeMap[Offset, V]() ++ Option(history.get(key)).getOrElse(new JavaTreeMap[Offset, V])
  }

  def shutdown(){
    if(isLive.getAndSet(false)){
      publisher.failOutstandingPromises()

      val queues = List(publisher, deserializer, serializer, deduplicator)
      queues.foreach(_.notifyQueue)
      reader.notifyReaderThread()
      reader.close()

      queues.foreach(_.thread.join)
      reader.thread.join

      writer.close()
      log.info("Event store " + topic + ": shut down")
    }
  }


  /**
    * Boilerplate. 
    * Constructs a thread that polls a queue, performing some action.
    */
  abstract class QueueProcessor[T](name : String){
    protected val queue = new ConcurrentLinkedQueue[T]()

    def canProcessHead(t : T) : Boolean
    def processHead(t : T) : Unit


    def notifyQueue() = queue.synchronized{
      queue.notify
    }

    def enqueue(a : AnyRef){
      queue.add(a.asInstanceOf[T])
      notifyQueue()
    }

    val thread = namedDaemonThread(
      "EventStore " + topic + ": " + name, 
      new Thread{
        override def run(){
          while (isLive.get()){
            queue.synchronized{
              queue.wait(1000)
            }
            try {
              while (! queue.isEmpty && canProcessHead(queue.peek))
                processHead(queue.poll())
            } catch {
              case e : Exception =>
                log.error(s"Error processing queue $name, shutting down", e)
                shutdown()
            }
          }
        }
      }
    )
  }

  /**
    * The deduplicator 
    *   1. Determines an order to all write requests
    *   2. Removes from writes any entities that are superfluous, i.e.
    *      that are identical to those in the surface corresponding to the
    *      previous write
    *   3. Passes deduplicated data to the serializer, and the promise to be completed 
    *      to the publisher
    */
  private def makeDeduplicator() = new QueueProcessor[(Promise[Offset], Map[K, V])]("Deduplicator"){
     // `latestVersions` are with respect to write requests. Other than during construction,
     // they will not be identical to the current latest versions in `history`, nor in Kafka.
    private val latestVersions : scala.collection.mutable.Map[K, V] = history.map{
      case (key, versions) => (key, versions.lastEntry.getValue)
    }(scala.collection.breakOut)

    def canProcessHead(head : (Promise[Offset], Map[K, V])) = true

    def processHead(head : (Promise[Offset], Map[K, V])){
      val (promise, map) = head  

      val updates = map.filterNot{
        case (key, value) => 
          latestVersions.get(key) == Some(value)
      }
      latestVersions ++= updates

      if (updates.nonEmpty){
        // Attach a UUID to the message written. This is eventually used by the 
        // publisher to determine which Offset to complete the promise with
        val uuid = UUID.randomUUID.toString
        serializer.enqueueFuture(uuid, updates)
        // Also pass the entities written so the publisher can check the values read back are identical
        publisher.enqueue((promise, Some((uuid, updates))))
      } else {
        // Here we will not be writing to Kafka, so put the promise onto the
        // publisher queue so that it is still completed in order.
        publisher.enqueue((promise, None))
      }
    }
  }

  /**
    * The serializer converts entities to be written into JSON messages - in parallel. 
    * The serializer writes these messages to Kafka in the same order the entities were received.
    */ 
  private def makeSerializer() = new QueueProcessor[Future[String]]("Serializer"){
    def enqueueFuture(uuid : UUID_String, updates : Map[K, V]){
      enqueue(
        Future{
          createMessage(uuid, updates)
        }.andThen{case _ => notifyQueue()})
    }
    
    def canProcessHead(message : Future[String]) = {
      message.isCompleted
    }
    def processHead(head : Future[String]){
      val message = Await.result(head, 1 second)
      writer.send(message)
      reader.notifyReaderThread()
    }
  }

  /**
    * The deserializer receives messages, via the `reader`, from the Kafka server. These messages
    * are deserialized in parallel. The deserialized entities are inserted into the in-memory history,
    * sequentially - in the same order the original writes occurred.
    * Once the history is updated the publisher is told the corresponding Offset, so it can complete the 
    * promise returned from the original write.
    */
  private def makeDeserializer() = 
    new QueueProcessor[(Offset, Future[Either[String, (UUID_String, Map[K, V])]])]("Deserializer"){

    def enqueueFuture(offset : Long, message : String){
      enqueue((
        Offset(offset), 
        Future{
          if (message.startsWith(SYNC_POINT))
            Left(message)
          else
            Right(parseMessage(message))
        }.andThen{case _ => notifyQueue()}
      ))
    }
    def canProcessHead(head : (Offset, Future[Either[String, (UUID_String, Map[K, V])]])) = { 
        head._2.isCompleted
    }
    def processHead(head : (Offset, Future[Either[String, (UUID_String, Map[K, V])]])) = {  
      val (offset, future) = head
      Await.result(future, 1 second) match {
        case Left(string) if (string == syncMessage) =>
          haveCompletedInitialLoad.set(true)
        case Left(_) =>
          // An old sync message - can be ignored
        case Right((uuid, updates)) => 
          updates.foreach{
            case (key, value) => 
              history.putIfAbsent(key, new JavaTreeMap[Offset, V]())
              history.get(key).put(offset, value)
          }
          latestOffsetInHistory.set(offset)
          publisher.writesToBePublished.put(uuid, (offset.value, updates))
          publisher.notifyQueue()
      }
    }
  }

  /**
    * The `publisher` completes the promise returned from the original write.
    * Items on the queue will normally have a UUID, creating an association
    * with a particular write request. 
    * Those that don't have a UUID correspond to requests that resulted in an empty 
    * write after de-duplication. Their promises will be completed with the previously
    * published offset.
    */
  private def makePublisher() = new QueueProcessor[(Promise[Offset], Option[(UUID_String, Map[K, V])])]("Publisher"){
  
    // Maintains a map of UUID to the Offset of the message read back from Kafka.
    //   [ Long should be replaced by Offset when we move to 2.11 ]
    val writesToBePublished = new ConcurrentHashMap[UUID_String, (Long, Map[K, V])]()

    val lastOffsetPublished = new AtomicReference[Offset](Offset(-1))

    /*
     * Called in the event of a shutdown
     */
    def failOutstandingPromises(){
      val e = new RuntimeException(s"EventStore $topic has been shut down")
      queue.synchronized{
        queue.notify
        while (! queue.isEmpty){
          val (promise, _) = queue.poll
          promise.failure(e)
        }
      }
    }

    def canProcessHead(head : (Promise[Offset], Option[(UUID_String, Map[K, V])])) = {
      head match {
        case (_, Some((uuid, _))) => writesToBePublished.containsKey(uuid)
        case (_, None) => true
      }
    }

    def checkUpdatesReadBackMatch(promise : Promise[Offset], preWrite : Map[K, V], postWrite : Map[K, V]){
      if (preWrite != postWrite){
        
        var errors : List[String] = Nil
        (preWrite.keys ++ postWrite.keys).foreach{
          key =>   
            if (preWrite.get(key) != postWrite.get(key))
              errors ::= s"Pre/Post values of key $key mismatch. ${preWrite.get(key)}/${postWrite.get(key)}"
        }
        val e = new RuntimeException(
          s"""|${errors.size} differences between entities written and read back. First is 
              |  ${errors.headOption.getOrElse("THIS SHOULD NEVER HAPPEN")}""".stripMargin
        )
        promise.failure(e)
        throw(e)
      }
    }

    def processHead(head : (Promise[Offset], Option[(UUID_String, Map[K, V])])){
      head match {
        case (promise, Some((uuid, updatesBeforeWriting))) => 
          val (offset_, updatesAsReadBack) = writesToBePublished.remove(uuid)
          checkUpdatesReadBackMatch(promise, updatesBeforeWriting, updatesAsReadBack)
          val offset = Offset(offset_)
          lastOffsetPublished.set(offset)
          promise.success(offset)
        case (promise, None) => 
          promise.success(lastOffsetPublished.get)
      }
    }
  }


}

object EventStore{
  type UUID_String = String
  case class Offset(value : Long) extends AnyVal with Ordered[Offset]{
    def compare(rhs : Offset) = value.compare(rhs.value)
  }
  case class NotFound[K](topic : String, key : K, offset : Offset)

  private val SYNC_POINT = "SYNC_POINT"
  def namedDaemonThread(name : String, thread : Thread) = {
    thread.setName(name)
    thread.setDaemon(true)
    thread.start
    thread
  }
}


