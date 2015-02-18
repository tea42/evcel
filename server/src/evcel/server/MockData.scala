package evcel.server

import java.util.Random

import evcel.curve.environment._
import evcel.curve.{EnvironmentParams, ValuationContext}
import evcel.daterange.DateRangeSugar._
import evcel.daterange.{Month, Day}
import evcel.instrument.Future
import evcel.instrument.trade.Trade
import evcel.quantity.{Qty}
import evcel.quantity.UOM._
import evcel.quantity.Qty._
import evcel.referencedata.calendar.{Calendar, SimpleCalendar, Calendars}
import evcel.referencedata.{FuturesExpiryRule, FuturesExpiryRules, ReferenceData}
import evcel.utils.{GeneralEvcelFail, EvcelFail}
import scala.language.reflectiveCalls
import evcel.referencedata.market.{Currency, Markets, VolumeCalcRuleLabel, FuturesMarket}
import evcel.report.{TradePivotReportBuilder, PivotReportLayout, PivotReport}
import evcel.valuation.DefaultValuer

import scala.util.{Left, Right, Either}

class MockData {

  val NYMEX_WTI = new FuturesMarket("Nymex WTI", "NYM", USD / BBL, BBL, VolumeCalcRuleLabel.Default)
  val ICE_BRENT = new FuturesMarket("ICE Brent", "ICE", USD / BBL, BBL, VolumeCalcRuleLabel.Default)

  val refData = {
    val expiryRules = new FuturesExpiryRules(
      Map(
        NYMEX_WTI.name -> FuturesExpiryRule("Nymex WTI",
          (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
          (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
        ),
        ICE_BRENT.name -> FuturesExpiryRule("ICE Brent",
          (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
          (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
        )
      )
    )
    val emptyCalendars = new Calendars(Map.empty) {
      override def calendar(name: String) = Right(new SimpleCalendar(Calendar.Holidays(Set.empty)))
    }
    val markets = {
      new Markets(
        Vector(NYMEX_WTI, ICE_BRENT).map { m => m.name -> m}.toMap,
        Map(),
        Map(USD -> Currency(USD, "USD", 2, "USD", false), GBP -> Currency(GBP, "GBP", 2, "GBP", false),
          EUR -> Currency(EUR, "EUR", 2, "EUR", false))
      )
    }
    ReferenceData(expiryRules, emptyCalendars, markets)
  }

  def nullValuationContext(marketDayX:MarketDay, zeroIR:Boolean, environmentParams:EnvironmentParams) = {
    val env = new AtomicEnvironment {
      def marketDay = marketDayX
      def apply(point: AtomicDatumIdentifier): Either[EvcelFail, Qty] = {
        Right(point.nullValue)
      }
    }
    val vc = new ValuationContext(env, refData, environmentParams)
    if (zeroIR) vc.undiscounted else vc
  }

  private def future(id : String, month : Month, mkt : FuturesMarket, cpty : String = "Acme") = {
    Trade(id, 1 / Jan / 2014, cpty, Future(mkt.name, month, 10 (mkt.priceUOM), 50(mkt.priceUOM.denominator)), Map.empty)
  }

  private var listeners = List[(Long)=>Unit]()
  def addListener(listener:(Long)=>Unit): Unit = {
    lock synchronized {
      listeners = listener :: listeners
    }
  }

  val lock = new Object
  private var version = 0L
  private val trades = new java.util.TreeMap[Long,Trade]()

  def latest = lock synchronized { version }
  def tradesAt(version:Long) = {
    lock synchronized {
      import scala.collection.JavaConversions._
      trades.headMap(version, true).values().toList
    }
  }

  private def addTrade() {
    lock synchronized {
      trades.put(version, createNextTrade(version))
      listeners.foreach(_(version))
      version += 1
    }
  }

  def start() {
    new Thread(new Runnable() { def run(): Unit = {
      while (true) {
        addTrade()
        Thread.sleep(3000)
      }
    }}).start()
  }

  val random = new Random(0)

  val months = ((Mar / 2015) to (Sep / 2015)).toArray
  val markets = refData.markets.futuresMarketsList.toArray
  val companies = Array("ABC", "XY10", "PPPPFDSD")
  private def createNextTrade(id:Long) = {
    future("T"+id, pick(months), pick(markets), pick(companies))
  }
  private def pick[T](values:Array[T]) = values(random.nextInt(values.length))
  (0 to 10).foreach(_ => addTrade())
}
