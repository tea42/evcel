package evcel.curve.marketdata

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal
import scala.collection.immutable.VectorBuilder

/**
  * Storing market data using arays of BigDecimals is more memory efficient then, e.g., 
  * using Maps with Quantity values. Significant further improvements can be achieved 
  * by using interned Strings to represent the BigDecimal, as long as the interning cache 
  * is kept small.
  *
  * This compression algortihm converts an Array[BigDecimal] to an Array[String], splitting
  * each BD's toString representation at the decimal point. The latter step is done allows
  * for a small interning cache. Note though that no interning is done at this point - the 
  * caller needs to take care of that.
  *
  * In tests with large volumes of market data, this compressed representation consumed 25% of 
  * the memory of the simpler Array[BigDecimal] 
  */

trait CompressMarketData{
  import CompressMarketData.SEPARATOR
  val decimalPoint = '.'

  implicit class Compressor(bigDecimals : Seq[BigDecimal]) {
    def compressed : Array[String] = {
      val buffer = new ArrayBuffer[String]()
      bigDecimals.foreach{
        bd => 
          buffer ++= bd.toString.split(decimalPoint)
          buffer += SEPARATOR
      }
      buffer.toArray
    }
  }

  implicit class Uncompressor(array : Array[String]){
    def uncompressed : Seq[BigDecimal] = {
      val builder = new VectorBuilder[BigDecimal]()
      var from = 0
      var to = array.indexOf(SEPARATOR, from)
      while (to != -1){
        builder += BigDecimal(array.slice(from, to).mkString("."))
        from = to + 1
        to = array.indexOf(SEPARATOR, from)
      }
      builder.result
    }
  }
}

object CompressMarketData{
  val SEPARATOR = 35.toChar.toString
}
