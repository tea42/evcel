package evcel.curve.marketdata

import org.scalatest.{Matchers, FreeSpec}
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty
import evcel.quantity.UOM._

class SpotPriceDataTests extends FreeSpec with Matchers{
  "Empty prices should be constructable" in {
    SpotPriceData(Map.empty)
  }

  "Repeated prices should be removed" in {
    val prices = SpotPriceData(
      Map(
        Jan / 14 -> Qty("1", USD/MT),
        Mar / 14 -> Qty("1", USD/MT),
        Apr / 14 -> Qty("2", USD/MT)
      )
    )

    val pricesWithoutRepeats = SpotPriceData(
      Map(
        Jan / 14 -> Qty("1", USD/MT),
        Apr / 14 -> Qty("2", USD/MT)
      )
    )

    prices should equal (pricesWithoutRepeats)

  }
  
}
