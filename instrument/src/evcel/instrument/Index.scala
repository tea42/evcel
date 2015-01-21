package evcel.instrument

import evcel.referencedata.calendar.Calendar
import evcel.referencedata.ReferenceData
import evcel.curve.ValuationContext
import evcel.daterange.{DateRange, Day, Month}
import evcel.quantity.{UOM, QtyConversions, Qty}
import evcel.curve.environment.MarketDay._
import evcel.curve.environment.AtomicEnvironment
import scala.collection.immutable.Nil
import evcel.referencedata.market._
import scala.util.{Either, Right, Left}
import evcel.utils.{EvcelFail, EitherUtils}
import evcel.utils.EitherUtils._

trait Index {
  def indexName = toString
}

object Index {
  val SPOT = "Spot"
  val FUTURES_FRONT_PERIOD = "Futures Front Period"
  val FUTURES_CONTRACT = "Futures Contract"

  def parse(name : String) : Index = {
    name match {
      case FuturesFrontPeriodIndex.Parse(market, nearby, _, roll) =>
        val nearbyNum = if (nearby.isEmpty) 1 else nearby.toInt
        val rollNum = if (roll.isEmpty) 0 else roll.toInt
        FuturesFrontPeriodIndex(market, nearbyNum, rollNum)
      case FuturesContractIndex.Parse(market, yy, mm) => 
        FuturesContractIndex(market, Month(yy.toInt, mm.toInt))
      case _ => SpotMarketIndex(name)
    }
  }

}


trait FuturesDerivedIndex extends Index {
  def underlyingMarketName: String

}

case class FuturesContractIndex(marketName: String, month: Month) extends FuturesDerivedIndex {

  override def underlyingMarketName = marketName

  override def toString = s"$marketName ($month)"
}

object FuturesContractIndex{
  val Parse = ("""(.*) \(""" + Month.Format + """+\)""").r
}

case class SpotMarketIndex(marketName: String) extends Index {
  override def toString = marketName
}

case class FuturesFrontPeriodIndex(marketName: String, nearby: Int, rollEarlyDays: Int) extends FuturesDerivedIndex {
  require(nearby > 0, "nearby: " + nearby)
  require(rollEarlyDays >= 0, "rollEarlyDays: " + rollEarlyDays)

  override def toString = {
    val nearbyString = " nearby " + nearby
    val rollString = if (rollEarlyDays == 0) "" else " roll " + rollEarlyDays
    marketName + nearbyString + rollString
  }

  override def underlyingMarketName = marketName
}

object FuturesFrontPeriodIndex{
  val Parse = """(.+) nearby ([0-9]+)[ ]?(roll )?([0-9]?)""".r
}

case class IndexSpread(index1: Index, index2: Index) {
  override def toString = s"$index1 vs $index2"

}


object IndexSpread {
  val Parse = """(.+?) vs (.+)""".r

  def parse(name: String): Option[IndexSpread] = {
    name match {
      case Parse(i1, i2) =>
        Some(IndexSpread(Index.parse(i1), Index.parse(i2)))
      case _ => None
    }
  }
}
