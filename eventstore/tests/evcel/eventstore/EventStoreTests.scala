package evcel.eventstore

import org.scalatest.FunSpec
import spray.json._
import evcel.eventstore.json.EventStoreJsonProtocol._
import evcel.eventstore.kafka.KafkaTestUtils
import EventStore._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import org.scalatest.Matchers
import scala.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.LinkedBlockingQueue
import java.util.Collections
import java.util.LinkedList
import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory
import org.slf4j.Logger

class EventStoreTests extends FunSpec with Matchers{
  
  import scala.concurrent.ExecutionContext.Implicits.global
  def makeEventStore(port : Int) = new EventStore[String, String]("EVENTS", port){
    protected def createMessage(uuid : UUID_String, map : Map[String, String]) = {
      (uuid, map).toJson.prettyPrint
    }
    protected def parseMessage(message : String) : (UUID_String, Map[String, String]) = {
      message.parseJson.convertTo[(UUID_String, Map[String, String])]
    }
  }
  def withEventStore(fn : EventStore[String, String] => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        KafkaTestUtils.createTopic(server, "EVENTS")
        val store = makeEventStore(server.config.port)
        try{
          fn(store)
        } finally {
          store.shutdown()
        }
    }
  }
  describe("EventStore"){

    it("Should have the correct offset after initialisation"){
      KafkaTestUtils.withTestKafka{
        server => 
          KafkaTestUtils.createTopic(server, "EVENTS")
          val (key, value) = ("Key", "Value")
          val store1 = makeEventStore(server.config.port)

          store1.lastOffsetInHistory() should equal (Offset(0))
          val promise = store1.write(key, value)
          Await.result(promise, 10 seconds) should equal(Offset(1))
          store1.lastOffsetInHistory() should equal (Offset(1))
          store1.shutdown()

          val store2 = makeEventStore(server.config.port)
          store2.lastOffsetInHistory() should equal (Offset(1))
          store2.shutdown()
          
      }
    }

    it("Shouldn't store duplicates"){
      withEventStore{
        store => 
          Await.result(store.write("key1", "value1"), 6 seconds) should equal(Offset(1))
          Await.result(store.write("key2", "value2"), 6 seconds) should equal(Offset(2))

          // Write a duplicate - offset should not increase
          Await.result(store.write("key1", "value1"), 6 seconds) should equal(Offset(2))
          // latest version should also not change
          store.lastOffsetInHistory() should equal (Offset(2))
      }
    }

    it("Should be able to write things in parallel"){

      KafkaTestUtils.withTestKafka{
        server => 
          val numEvents = 500
          KafkaTestUtils.createTopic(server, "EVENTS")
          val store1 = makeEventStore(server.config.port)
          val futures : List[Future[Offset]] = (1 to numEvents).toList.map(i => 
            Future{
              var map = Map[String, String]()
              for (j <- 1 to (math.random * 10).toInt){
                val k = "Key " + (math.random * 10).toInt
                val v = "Value " + (math.random * 10).toInt
                map += (k -> v)
              }
              store1.write(map)
            }.flatMap(identity)
          )
          
          val maxOffset = Future.fold(futures)(Offset(0)){
            case (Offset(l), Offset(r)) => 
              Offset (l max r)
          }
          val result = Await.result(maxOffset, 20 seconds)
          store1.shutdown()
          
          val store2 = makeEventStore(server.config.port)
          store2.lastOffsetInHistory() should equal (result)
          store2.shutdown()
      }
    }
  }

  it("Should throw an exception if what is read back doesn't match what was written"){
    KafkaTestUtils.withTestKafka{
      server => 
        import spray.json._
        import spray.json.RootJsonFormat
        import spray.json.JsNumber
        case class Broken(i : Int)
        implicit val brokenFormat = new RootJsonFormat[Broken]{
          def write(b : Broken) = JsNumber(b.i)
          def read(value : JsValue)  = value match {
            case JsNumber(i) => Broken(i.toInt * 2)
            case _ => throw new RuntimeException("Unexpected type")
          }
        }
        val store = new EventStore[String, Broken](
          "Broken", server.config.port
        ){
          protected def createMessage(uuid : UUID_String, map : Map[String, Broken]) = {
            (uuid, map).toJson.prettyPrint
          }
          protected def parseMessage(message : String) : (UUID_String, Map[String, Broken]) = {
            message.parseJson.convertTo[(UUID_String, Map[String, Broken])]
          }
        }

        val testWaitTime = 5 seconds
        // Broken(0) will be unchanged as 2 * 0 = 0
        val offset1 = Await.result(store.write("0", Broken(0)), testWaitTime)
        offset1 should equal (Offset(1))


        // Broken(1) will throw an error as the deserializer will return Broken(2)
        intercept[RuntimeException]{
          Await.result(store.write("1", Broken(1)), testWaitTime)
        }
        store.shutdown()
    }
  }
}
