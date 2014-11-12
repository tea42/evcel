package evcel.curve.environment

import evcel.quantity.UOM

sealed trait MarketDataIdentifier



case class ZeroRatesIdentifier(currency : UOM) extends MarketDataIdentifier
case class FuturesPricesIdentifier(market : String) extends MarketDataIdentifier
case class FuturesVolsIdentifier(market : String) extends MarketDataIdentifier
case class SpotPricesIdentifier(market : String) extends MarketDataIdentifier
case class SpotFXIdentifier(from: UOM, to: UOM) extends MarketDataIdentifier

