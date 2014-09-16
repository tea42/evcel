package evcel.curve.marketdata

import evcel.curve.ReferenceData
import evcel.curve.environment.{MarketDataIdentifier, MarketDay, Curve}
import evcel.daterange.Day
import evcel.quantity.UOM

trait MarketData

object MarketData{
  case class CantBuildCurve(identifier : MarketDataIdentifier, marketDay : MarketDay, reason : String)
  val FUTURES_PRICES = "Futures Prices"
  val ZERO_RATES = "Zero Rates"
  val FUTURES_VOLS = "Futures Vols"
  val SPOT_PRICES = "Spot Prices"
}

