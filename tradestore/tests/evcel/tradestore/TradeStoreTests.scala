package evcel.tradestore

import org.scalatest.FunSpec
import kafka.admin.AdminUtils
import evcel.eventstore.kafka.KafkaTestUtils
import evcel.instrument.Future
import evcel.daterange.DateRangeSugar._
import evcel.instrument.trade.Trade
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.eventstore.EventStore.Offset
import scala.concurrent.Await
import scala.concurrent.duration._
import kafka.server.KafkaServer
import scala.language.postfixOps
import scala.language.reflectiveCalls
import org.scalatest.Matchers

class TradeStoreTests extends FunSpec with Matchers{
  def withTradeStore(fn : TradeStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        KafkaTestUtils.createTopic(server, TradeStore.TOPIC)
        val store = TradeStore(kafkaPort = server.config.port)
        try{
          fn(store)
        } finally {
          store.shutdown()
        }
    }
  }

  def makeTrade(id : String) = 
    Trade(
      id = id,
      tradeDay = 1 / Jun / 2014,
      counterparty = "ACME INC",
      tradeable = Future("WTI", Aug / 2014, Qty("100.0", USD/MT), Qty("10", MT)),
      meta = Map("Portfolio" -> "Book A")
    )

  describe("TradeStore"){
    it("Should store trades"){
      withTradeStore{
        tradeStore => 
          val trade = Trade(
            id = "1234",
            tradeDay = 1 / Jun / 2014,
            counterparty = "ACME INC",
            tradeable = Future("WTI", Aug / 2014, Qty("100.0", USD/MT), Qty("10", MT)),
            meta = Map("Portfolio" -> "Book A")
          )
          Await.result(tradeStore.write(trade), 6 seconds) should equal(Offset(1))
          tradeStore.read(Offset(1), trade.id) should equal (Right(trade))
      }
    }
  }
}
