package evcel.curve.marketdata

import evcel.referencedata.ReferenceData
import evcel.curve.environment.{MarketDataIdentifier, MarketDay, Curve}
import evcel.daterange.Day
import evcel.quantity.UOM

trait MarketData

object MarketData{
  case class CantBuildCurve(identifier : MarketDataIdentifier, marketDay : MarketDay, reason : String)
  val FUTURES_PRICES = "Futures Prices"
  val PRICE_FIXINGS = "Price Fixings"
  val ZERO_RATES = "Zero Rates"
  val SPOT_FX = "Spot FX"
  val FUTURES_VOLS = "Futures Vols"
  val SPOT_PRICES = "Spot Prices"
}

