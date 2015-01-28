package evcel.referencedata.market

import evcel.referencedata._
import evcel.daterange.{Month, Day}
import evcel.quantity.{QtyConversions, UOM}
import evcel.referencedata.calendar.Calendar
import scala.util.{Either, Left, Right}
import evcel.utils.{EvcelFail, GeneralEvcelFail}
import evcel.utils.EitherUtils._

case class FuturesMarket(
  name: String, 
  calendarName: String, 
  priceUOM: UOM, 
  // Normally the denominator of the priceUOM, however 
  // there are counterexamples
  //   - NBP futures are traded in Thm/Day 
  //   - power futures are traded in MW
  // Markets with a non-standard quotedVolumeUOM 
  // will need a volumeCalcRule
  quotedVolumeUOM : UOM, 
  volumeCalcRuleLabel : VolumeCalcRuleLabel,
  conversions: QtyConversions = QtyConversions(Map.empty),
  level: Level = Level.Close
) 
  extends ReferenceDataTrait

case class FuturesMarketIdentifier(name : String) extends ReferenceDataIdentifier
