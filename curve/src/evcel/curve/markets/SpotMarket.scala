package evcel.curve.markets

import evcel.curve.ReferenceData
import evcel.quantity.{QtyConversions, UOM}

case class SpotMarket(name: String, calendarName: String, priceUOM: UOM, conversions: Option[QtyConversions] = None)

object SpotMarket {
  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => refData.markets.spotMarket(name)
    case _ => None
  }
}
