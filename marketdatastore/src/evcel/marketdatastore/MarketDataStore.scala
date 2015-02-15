package evcel.marketdatastore

import evcel.curve.environment.{MarketDataIdentifier, FuturesPricesIdentifier, SpotPricesIdentifier}
import evcel.curve.marketdata.{MarketData, FuturesPriceData, SpotPriceData}
import evcel.daterange.Day
import evcel.eventstore.json.EventStoreJsonProtocol._
import spray.json._
import evcel.eventstore.EventStore
import evcel.eventstore.EventStore._
import scala.concurrent.Future
import scala.collection.breakOut
import scala.util.{Either, Left, Success}


case class MarketDataStore(kafkaPort : Int, removeRedundantPrices : Boolean)
  extends EventStore[(Day, MarketDataIdentifier), MarketData](MarketDataStore.TOPIC, kafkaPort)
{
  import scala.concurrent.ExecutionContext.Implicits.global
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

    var future = write(Map((marketDay, key) -> data))

    if (removeRedundantPrices)
      future = future.andThen{
        case Success(offset) =>
          key match {
            case fp : SpotPricesIdentifier =>
              read(offset, marketDay, key) match {
                case Right(pd : SpotPriceData) => 
                  replaceInMemoryValue(offset, (marketDay, key), pd.removeRedundantPrices())
                case _ =>
              }
            case _ => 
          }
      }
    future
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
