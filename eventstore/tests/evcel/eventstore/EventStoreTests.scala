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

class EventStoreTests extends FunSpec with Matchers{
  
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

          store1.latestOffset() should equal (Offset(0))
          Await.result(store1.write(key, value), 2 seconds) should equal(Offset(1))
          store1.latestOffset() should equal (Offset(1))
          store1.shutdown

          val store2 = makeEventStore(server.config.port)
          // note: Is incremented as the initial sync writes a NOP kafka message
          store2.latestOffset() should equal (Offset(2))
          store2.shutdown
          
      }
    }

    it("Shouldn't store duplicates"){
      withEventStore{
        store => 
          Await.result(store.write("key1", "value1"), 2 seconds) should equal(Offset(1))
          Await.result(store.write("key2", "value2"), 2 seconds) should equal(Offset(2))

          // Write a duplicate - offset should not increase
          Await.result(store.write("key1", "value1"), 2 seconds) should equal(Offset(2))
          // latest version should also not change
          store.latestOffset() should equal (Offset(2))
      }
    }
  }
}
