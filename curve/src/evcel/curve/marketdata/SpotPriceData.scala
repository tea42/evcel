package evcel.curve.marketdata

import evcel.curve.curves.SpotPrices
import evcel.curve.environment.MarketDay
import evcel.curve.marketdata.MarketData.CantBuildCurve
import evcel.daterange.{DateRange, Day}
import evcel.quantity.{BDQty, UOM, Qty}
import scala.util.Either
import java.util.Arrays
import scala.collection.immutable.VectorBuilder
import java.util.{TreeMap => JavaTreeMap}

case class SpotPriceData (uom : Option[UOM], periods : Array[DateRange], compressedPrices : Array[String])
  extends MarketData with CompressMarketData
{
  def checkDataValidity {
    if (periods.isEmpty){
      require(compressedPrices.isEmpty, "Have prices with no periods")
    } else {
      require(uom.isDefined, "No UOM for SpotPriceData")
      val sortedPeriods = periods.sortWith(_.firstDay < _.firstDay)
      sortedPeriods.zip(sortedPeriods.tail).foreach {
        case (p1, p2) => 
          require(
            p1.lastDay < p2.firstDay,
            s"Have overlapping spot price periods, $p1, $p2"
          )
      }
    }
  }

  checkDataValidity

  def buildCurve(market: String, marketDay: MarketDay): Either[CantBuildCurve, SpotPrices] = {
    val sortedPrices = {
      val prices = compressedPrices.uncompressed.map(Qty(_, uom.get))
      val days = periods.map(_.firstDay)
      val map = new JavaTreeMap[Day, Qty]()
      days.zip(prices).foreach{
        case (d, p) => 
          map.put(d, p)
      }
      map
    }

    Right(SpotPrices(market, marketDay, sortedPrices))
  }

  override def equals(rhs : Any) = Option(rhs) match {
    case Some(SpotPriceData(uom2, periods2, compressedPrices2)) =>
      uom2 == uom && 
      periods.toList == periods2.toList &&
      compressedPrices.toList == compressedPrices2.toList
    case _ => false
  }

  override def hashCode = uom.hashCode + 
    17 * periods.toList.hashCode + 
    17 * 17 * compressedPrices.toList.hashCode
}

object SpotPriceData extends CompressMarketData{
  /*
   * User provided data may contain superfluous prices, a common practise being to provide 
   * all daily prices for each month, with the same price for each day. As reluctant as I am to 
   * throw away information, this can have a significant memory cost. Better to make it explicit 
   * that this is our practise. 
   *
   * Note that as we use left constant interpolation this cannot cause any change to the resulting
   * SpotPrices curve object.
   */
  private def removeRepeatedPrices(prices : Map[DateRange, BDQty]) : Map[DateRange, BDQty] = {
    var map = Map[DateRange, BDQty]()
    var lastPrice : Option[BDQty] = None
    val sortedPeriods = prices.keys.toList.sortWith(_.firstDay < _.firstDay)
    sortedPeriods.foreach{
      period => 
        if (prices.get(period) != lastPrice){
          lastPrice = prices.get(period)
          map += ((period, lastPrice.get))
        }
    }
    map
  }

  def apply(prices : Map[DateRange, BDQty]) : SpotPriceData = {
    if (prices.isEmpty)
      SpotPriceData(None, Array[DateRange](), Array[String]())
    else {
      val pricesSansRepeats = removeRepeatedPrices(prices)
      val periods = pricesSansRepeats.keys.toArray
      val priceValues = periods.toVector.map(pricesSansRepeats(_).bdValue)

      SpotPriceData(
        Some(prices.head._2.uom),
        periods,
        priceValues.compressed
      )
    }
  }
}
