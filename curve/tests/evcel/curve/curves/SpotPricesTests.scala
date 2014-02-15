package evcel.curve.curves

import org.scalatest.{Matchers, FreeSpec, EitherValues}
import evcel.daterange.DateRangeSugar._
import evcel.curve.environment.{MarketDayPimps, MarketDay}
import java.util.{TreeMap => JavaTreeMap}
import evcel.daterange.Day
import evcel.quantity.Qty
import evcel.quantity.UOM._
import scala.language.reflectiveCalls
import scala.language.postfixOps

class SpotPricesTests extends FreeSpec with Matchers with MarketDayPimps with EitherValues{

  "SpotPrices" - {
    
    "Should return a left when empty" in {
      spotPrices(1 / Jan / 2014 endOfDay)(2 / Jan / 2014) should be ('left)
    }

    "Should throw an execption when asked for a day before the market day" in {
      intercept[RuntimeException]{
        spotPrices(1 / Jan / 2014 startOfDay)(31 / Dec / 2013)
      }
      intercept[RuntimeException]{
        spotPrices(1 / Jan / 2014 endOfDay)(1 / Jan / 2014)
      }
    }
    "Should return a left when asked for a day before the first price" in {
      spotPrices(
        1 / Jan / 2014 endOfDay, 
        10 / Jan / 2014 -> 12.5
      )(5 / Jan / 2014) should be ('left)
    }

    "Should return a right when asked for a day on the first price" in {
      spotPrices(
        1 / Jan / 2014 endOfDay, 
        5 / Jan / 2014 -> 12.5
      )(5 / Jan / 2014).right.value should be (Qty(12.5, USD/MT))
    }

    "Should do left constant interpolation" in {
      val prices = spotPrices(
        1 / Jan / 2014 endOfDay, 
        10 / Jan / 2014 -> 10.0, 
        20 / Jan / 2014 -> 20.0, 
        30 / Jan / 2014 -> 30.0)

      prices(10 / Jan / 2014).right.value should be (Qty(10.0, USD/MT))
      prices(19 / Jan / 2014).right.value should be (Qty(10.0, USD/MT))

      prices(20 / Jan / 2014).right.value should be (Qty(20.0, USD/MT))
      prices(29 / Jan / 2014).right.value should be (Qty(20.0, USD/MT))

      prices(30 / Jan / 2014).right.value should be (Qty(30.0, USD/MT))
    }

    "Should extrapolate constantly beyond the last point" in {
      
      val prices = spotPrices(
        1 / Jan / 2014 endOfDay, 
        10 / Jan / 2014 -> 10.0
      )
      prices(15 / Apr / 2020).right.value should be (Qty(10.0, USD/MT))
    }
  }

  private def spotPrices(marketDay : MarketDay, prices : (Day, Double) *) : SpotPrices = {
    val map = new JavaTreeMap[Day, Qty]()
    prices.foreach{
      case (d, p) => 
        map.put(d, Qty(p, USD/ MT))
    }
    SpotPrices("XYX", marketDay, map)
  }

}
