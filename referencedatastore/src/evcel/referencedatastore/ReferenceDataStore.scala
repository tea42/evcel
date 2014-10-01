package evcel.referencedatastore
import evcel.eventstore.EventStore
import evcel.eventstore.EventStore._
import evcel.eventstore.json.EventStoreJsonProtocol._
import spray.json._
import evcel.referencedata.ReferenceDataTrait
import evcel.referencedata.ReferenceDataIdentifier

case class ReferenceDataStore(kafkaPort : Int)
  extends EventStore[ReferenceDataIdentifier, ReferenceDataTrait](ReferenceDataStore.TOPIC, kafkaPort)
{
  protected def parseMessage(message : String) : (UUID_String, Map[ReferenceDataIdentifier, ReferenceDataTrait]) = {
    val (uuid, list) =
      message.parseJson.convertTo[(UUID_String, List[(ReferenceDataIdentifier, ReferenceDataTrait)])]

    (uuid, list.toMap)
  }

  protected def createMessage(uuid : UUID_String, map : Map[ReferenceDataIdentifier, ReferenceDataTrait]) : String = {
    (uuid, map.toList).toJson.prettyPrint
  }
}

object ReferenceDataStore{
  val TOPIC = "ReferenceData"
}
