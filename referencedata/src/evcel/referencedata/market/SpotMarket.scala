package evcel.referencedata.market

import evcel.quantity.{QtyConversions, UOM}
import evcel.referencedata.ReferenceData
import evcel.referencedata.calendar.Calendar
import scala.util.Either
import evcel.utils.EvcelFail
import evcel.utils.EitherUtils._

case class SpotMarket(
  name: String, calendarName: String, 
  priceUOM: UOM, quotedVolumeUOM : UOM, 
  volumeCalcRuleLabel : VolumeCalcRuleLabel, 
  conversions: QtyConversions = QtyConversions(Map.empty))


object SpotMarket {
  def unapply(o: AnyRef) = o match {
    case (refData: ReferenceData, name: String) => refData.markets.spotMarket(name)
    case _ => None
  }
}
