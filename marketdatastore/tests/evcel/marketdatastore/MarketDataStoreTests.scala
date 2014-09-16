package evcel.marketdatastore

import evcel.curve.environment.{FuturesVolsIdentifier, ZeroRatesIdentifier, FuturesPricesIdentifier}
import org.scalatest.{Matchers, FunSpec}
import evcel.eventstore.kafka.KafkaTestUtils
import kafka.admin.AdminUtils
import evcel.daterange.DateRangeSugar._
import evcel.curve.marketdata.MarketData._
import evcel.curve.marketdata._
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.eventstore.EventStore._
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalatest.prop.TableDrivenPropertyChecks
import evcel.quantity.Percent
import scala.language.reflectiveCalls
import scala.language.postfixOps

class MarketDataStoreTests extends FunSpec with Matchers with TableDrivenPropertyChecks{

  def withMarketDataStore(fn : MarketDataStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        AdminUtils.createTopic(server.zkClient, MarketDataStore.TOPIC, partitions = 1, replicationFactor = 1)
        val store = MarketDataStore(kafkaPort = server.config.port)
        try{
          Thread.sleep(100) // Prevents 'Failed to collate messages by topic' error message from Kafka,
                            // which is just noise, as Kafka invariably retries successfully
          fn(store)
        } finally {
          store.shutdown()
        }
    }
  }

  describe("MarketDataStore"){
    it("Should be able to store and retrieve all kinds of market data"){
      withMarketDataStore{
        store => 
          val marketDay = 10 / Jun / 2014
          Table(
            ("Key", "Data"),
            (FuturesPricesIdentifier("WTI"),
              FuturesPriceData(Jun / 2014 -> Qty("100", USD/MT))),
            (ZeroRatesIdentifier(USD),
              ZeroRateData(Act365, List((10/Jun/2014, Qty("5", PERCENT))))),
            (FuturesVolsIdentifier("WTI"),
              FuturesVolData(
                List(
                  (Jun/2014, Percent("20"), List((0.1, Qty("5", PERCENT)), (0.6, Qty("3", PERCENT))))
            )))
          ).zipWithIndex.foreach{
            case ((key, data), i) => 
              Await.result(store.write(marketDay, key, data), 2 seconds) should equal(Offset(i))
              store.read(Offset(i), marketDay, key) should equal (Right(data))
          }
      }
    }
  }
}
