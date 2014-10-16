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
import evcel.curve.marketdata.MarketDataTest

class MarketDataStoreTests extends FunSpec with MarketDataTest with Matchers with TableDrivenPropertyChecks{

  def withMarketDataStore(fn : MarketDataStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        KafkaTestUtils.createTopic(server, "EVENTS")
        val store = MarketDataStore(kafkaPort = server.config.port)
        try{
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
              futuresPrices(Jun / 2014 -> Qty("100", USD/MT))),
            (ZeroRatesIdentifier(USD),
              ZeroRateData(Act365, List((10/Jun/2014, Qty("5", PERCENT))))),
            (FuturesVolsIdentifier("WTI"),
              futuresVols(
                  (Jun/2014, Percent("20"), List((0.1, Qty("5", PERCENT)), (0.6, Qty("3", PERCENT))))
            ))
          ).zipWithIndex.foreach{
            case ((key, data), i) => 
              Await.result(store.write(marketDay, key, data), 2 seconds) should equal(Offset(i + 1))
              store.read(Offset(i + 1), marketDay, key) should equal (Right(data))
          }
      }
    }
  }
}
