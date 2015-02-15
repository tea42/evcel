package evcel.marketdatastore

import evcel.curve.environment._
import org.scalatest.{Matchers, FunSpec}
import evcel.eventstore.kafka.KafkaTestUtils
import kafka.admin.AdminUtils
import evcel.daterange.DateRangeSugar._
import evcel.curve.marketdata.MarketData._
import evcel.curve.marketdata._
import evcel.quantity.{Qty, Percent, BDQty}
import evcel.quantity.UOM._
import evcel.eventstore.EventStore._
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.language.{reflectiveCalls, postfixOps}
import evcel.daterange.DateRange
import evcel.utils.EitherTestPimps

class MarketDataStoreTests extends FunSpec with 
  MarketDataTest with Matchers 
  with TableDrivenPropertyChecks
  with EitherTestPimps{

  def withMarketDataStore[T](removeRedundantPrices : Boolean = false)(fn : MarketDataStore => T) : T = {
    KafkaTestUtils.withTestKafka{
      server => 
        KafkaTestUtils.createTopic(server, "EVENTS")
        val store = MarketDataStore(kafkaPort = server.config.port, removeRedundantPrices)
        try{
          fn(store)
        } finally {
          store.shutdown()
        }
    }
  }

  describe("MarketDataStore"){
    it("Should be able to store and retrieve all kinds of market data"){
      withMarketDataStore(){
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
              Await.result(store.write(marketDay, key, data), 6 seconds) should equal(Offset(i + 1))
              store.read(Offset(i + 1), marketDay, key) should equal (Right(data))
          }
      }
    }

    it("Should remove redundant spot prices when so configured"){
      val marketDay = 10 / Jun / 2014
      val prices = Map[DateRange, BDQty](
        1 / Jul / 2014 -> Qty("1", USD/MT), 
        10 / Jul / 2014 -> Qty("1", USD/MT), 
        20 / Jul / 2014 -> Qty("6", USD/MT)
      )
      val spotPrices = SpotPriceData(prices)
      val key = SpotPricesIdentifier("ACME")

      def inMemoryPrices(removeRedundantPrices : Boolean) : SpotPriceData = {
        withMarketDataStore(removeRedundantPrices){
          store => 

            val offset = Await.result(store.write(marketDay, key, spotPrices), 1 hour) 
            store.read(offset, marketDay, key).R.asInstanceOf[SpotPriceData]
        }
      }
      inMemoryPrices(removeRedundantPrices = true).periods should equal (Array(1 / Jul / 2014, 20 / Jul / 2014))

      inMemoryPrices(removeRedundantPrices = false).periods should equal (
        Array(1 / Jul / 2014, 10 / Jul / 2014, 20 / Jul / 2014)
      )
    }
  }
  
}
