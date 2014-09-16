package evcel.marketdatastore

import evcel.curve.environment.MarketDataIdentifier
import evcel.curve.marketdata.MarketData
import evcel.daterange.Day
import evcel.eventstore.json.EventStoreJsonProtocol._
import spray.json._
import evcel.eventstore.EventStore
import evcel.eventstore.EventStore._
import scala.concurrent.Future
import scala.collection.breakOut


case class MarketDataStore(kafkaPort : Int)
  extends EventStore[(Day, MarketDataIdentifier), MarketData](MarketDataStore.TOPIC, kafkaPort)
{
  type SerializedMarketData = String

  protected def parseMessage(message : String) : (UUID_String, Map[(Day, MarketDataIdentifier), MarketData]) = {
    val (uuid, list) =
      message.parseJson.convertTo[(UUID_String, List[((Day, MarketDataIdentifier), MarketData)])]

    (uuid, list.toMap)
  }

  protected def createMessage(uuid : UUID_String, map : Map[(Day, MarketDataIdentifier), MarketData]) : String = {
    (uuid, map.toList).toJson.prettyPrint
  }

  def write(marketDay : Day, key : MarketDataIdentifier, data : MarketData) : Future[Offset] = {
    write(Map((marketDay, key) -> data))
  }
  def read(
    offset : Offset, 
    marketDay : Day, 
    key : MarketDataIdentifier
  ) : Either[EventStore.NotFound[(Day, MarketDataIdentifier)], MarketData] = {
    read(offset, (marketDay, key))
  }
}

object MarketDataStore{
  val TOPIC = "MarketData"
}
