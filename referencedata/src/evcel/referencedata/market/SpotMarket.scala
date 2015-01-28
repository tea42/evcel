package evcel.referencedata.market

import evcel.quantity.{QtyConversions, UOM}
import evcel.referencedata.{Level, ReferenceData}

case class SpotMarket(
  name: String, calendarName: String, 
  priceUOM: UOM, quotedVolumeUOM : UOM, 
  volumeCalcRuleLabel : VolumeCalcRuleLabel, 
  conversions: QtyConversions = QtyConversions(Map.empty),
  level: Level = Level.Close)


object SpotMarket {
  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => refData.markets.spotMarket(name)
    case _ => None
  }
}
