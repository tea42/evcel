package evcel.referencedatastore

import org.scalatest.{FunSpec, Matchers}
import evcel.eventstore.kafka.KafkaTestUtils
import kafka.admin.AdminUtils
import evcel.referencedata.{CalendarIdentifier, FuturesExpiryRuleIdentifier, FuturesExpiryRule}
import evcel.referencedata.calendar.CalendarData
import evcel.daterange.DateRangeSugar._
import scala.concurrent.duration._
import scala.concurrent.Await
import evcel.eventstore.EventStore.Offset
import evcel.referencedata.market.{FuturesMarketIdentifier, FuturesMarket, VolumeCalcRuleLabel}
import evcel.quantity.UOM._
import scala.language.{reflectiveCalls, postfixOps}
import scala.util.Right

class ReferenceDataStoreTests extends FunSpec with Matchers{
  def withReferenceDataStore(fn : ReferenceDataStore => Unit){
    KafkaTestUtils.withTestKafka{
      server => 
        KafkaTestUtils.createTopic(server, ReferenceDataStore.TOPIC)
        val store = ReferenceDataStore(kafkaPort = server.config.port)
        try{
          fn(store)
        } finally {
          store.shutdown()
        }
    }
  }
  describe("ReferenceDataStore"){
    it("Should be able to store all types of ref data"){
      withReferenceDataStore{
        store => 
          val (calendarKey, calendarData) = (
            CalendarIdentifier("Cal"),
            CalendarData(Set(1 / Jun / 2014))
          )
          Await.result(store.write(calendarKey, calendarData), 6 seconds) should equal (Offset(1))
          store.read(Offset(1), calendarKey) should equal (Right(calendarData))

          val (marketKey, market) = (
            FuturesMarketIdentifier("WTI"),
            FuturesMarket("WTI", "CALENDAR", USD/MT, MT, VolumeCalcRuleLabel.Default)
          )
          Await.result(store.write(marketKey, market), 6 seconds) should equal (Offset(2))
          store.read(Offset(2), marketKey) should equal (Right(market))

          val (expiryKey, expiry) = (
            FuturesExpiryRuleIdentifier("WTI"),
            FuturesExpiryRule("WTI", Map(Jun / 2014 -> 10 / Jul / 2014), Map(Jun / 2014 -> 9 / Jul / 2014))
          )
          Await.result(store.write(expiryKey, expiry), 6 seconds) should equal (Offset(3))
          store.read(Offset(3), expiryKey) should equal (Right(expiry))
          store.read(Offset(3), marketKey) should equal (Right(market))
          store.read(Offset(3), calendarKey) should equal (Right(calendarData))
      }
    }
  }
}
