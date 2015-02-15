package evcel.curve.marketdata

import evcel.referencedata.ReferenceData
import evcel.curve.curves.FuturesPrices
import evcel.curve.environment._
import evcel.quantity.{BDQty, Qty, UOM}
import evcel.daterange.{Month, Day}
import scala.collection.SortedMap
import scala.util.{Either, Right}
import scala.collection.mutable.WrappedArray
import java.util.Arrays

case class FuturesPriceData(uom : Option[UOM], months : Array[Month], compressedPrices : Array[String])
  extends MarketData with CompressMarketData
{
  def buildCurve(market : String, marketDay: MarketDay): Either[MarketData.CantBuildCurve, FuturesPrices] = {
    val priceMap : Map[Month, BDQty] = months.zip(compressedPrices.uncompressed).map{
      case (month, price) => 
        month -> Qty(price, uom.get)
    }(scala.collection.breakOut)
    Right(FuturesPrices(market, marketDay, priceMap))
  }
  
  override def equals(rhs : Any) = Option(rhs) match {
    case Some(FuturesPriceData(uom2, months2, compressedPrices2)) =>
      uom2 == uom && 
        months.sameElements(months2) && 
        compressedPrices.sameElements(compressedPrices2)
    case _ => false
  }

  override def hashCode = uom.hashCode + 
    17 * months.toList.hashCode + 
    17 * 17 * compressedPrices.toList.hashCode

}

object FuturesPriceData extends CompressMarketData{
  def apply(prices : Map[Month, BDQty]) : FuturesPriceData = {
    val months = prices.keys.toArray
    val priceArray = months.map(prices(_).bdValue)
    val uom = months.headOption.map(prices(_).uom)
    FuturesPriceData(uom, months, priceArray.compressed)
  }
}
