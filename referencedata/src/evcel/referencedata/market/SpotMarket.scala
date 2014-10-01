package evcel.referencedata.market

import evcel.quantity.{QtyConversions, UOM}
import evcel.referencedata.ReferenceData

case class SpotMarket(name: String, calendarName: String, priceUOM: UOM, conversions: Option[QtyConversions] = None)

object SpotMarket {
  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => refData.markets.spotMarket(name)
    case _ => None
  }
}
