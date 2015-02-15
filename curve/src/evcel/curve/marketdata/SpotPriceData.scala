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

  private def asMap : Map[DateRange, BDQty] = {
    val prices = compressedPrices.uncompressed.map(Qty(_, uom.get))
    periods.zip(prices)(scala.collection.breakOut)
  }

  def buildCurve(market: String, marketDay: MarketDay): Either[CantBuildCurve, SpotPrices] = {
    val sortedPrices = new JavaTreeMap[Day, Qty]()
    asMap.foreach{
      case (period, price) => 
        sortedPrices.put(period.firstDay, price)
    }

    Right(SpotPrices(market, marketDay, sortedPrices))
  }

  override def equals(rhs : Any) = Option(rhs) match {
    case Some(SpotPriceData(uom2, periods2, compressedPrices2)) =>
      uom2 == uom && 
      periods.sameElements(periods2) &&
      compressedPrices.sameElements(compressedPrices2)
    case _ => false
  }

  override def hashCode = uom.hashCode + 
    17 * periods.toList.hashCode + 
    17 * 17 * compressedPrices.toList.hashCode

  /*
   * User provided data may contain superfluous prices, a common practise being to provide 
   * all daily prices for each month, with the same price for each day. As reluctant as I am to 
   * throw away information, this can have a significant memory cost. Better to make it explicit 
   * that this is our practise. 
   *
   * Note that as we use left constant interpolation this cannot cause any change to the resulting
   * SpotPrices curve object.
   */
  def removeRedundantPrices() = {
    val pricesByPeriod = asMap
    var nonRedundantPrices = Map[DateRange, BDQty]()
    var lastPrice : Option[BDQty] = None
    periods.sortWith(_.firstDay < _.firstDay).foreach{
      period => 
        if (pricesByPeriod.get(period) != lastPrice){
          lastPrice = pricesByPeriod.get(period)
          nonRedundantPrices += ((period, lastPrice.get))
        }
    }
    SpotPriceData(nonRedundantPrices)
  }
}

object SpotPriceData extends CompressMarketData{

  def apply(prices : Map[DateRange, BDQty]) : SpotPriceData = {
    if (prices.isEmpty)
      SpotPriceData(None, Array[DateRange](), Array[String]())
    else {
      val periods = prices.keys.toArray
      val priceValues = periods.map(prices(_).bdValue)

      SpotPriceData(
        Some(prices.head._2.uom),
        periods,
        priceValues.compressed
      )
    }
  }
}
