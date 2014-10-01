package evcel.tradestore

import evcel.eventstore.EventStore
import evcel.instrument.trade.Trade
import evcel.eventstore.EventStore._
import evcel.eventstore.json.EventStoreJsonProtocol._
import spray.json._
import scala.concurrent.Future

case class TradeStore(kafkaPort : Int)
  extends EventStore[String, Trade](TradeStore.TOPIC, kafkaPort)
{
  protected def createMessage(uuid : UUID_String, map : Map[String, Trade]) = {
      (uuid, map.toList).toJson.prettyPrint
  }
  protected def parseMessage(message : String) : (UUID_String, Map[String, Trade]) = {
    val (uuid, list) = message.parseJson.convertTo[(UUID_String, List[(String, Trade)])]
    (uuid, list.toMap)
  }
  def write(trade : Trade) : Future[EventStore.Offset] = write(Map(trade.id -> trade))

}

object TradeStore{
  val TOPIC="Trades"
}
