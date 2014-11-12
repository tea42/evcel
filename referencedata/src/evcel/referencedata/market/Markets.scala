package evcel.referencedata.market

import evcel.quantity.UOM

class Markets(
  futuresMarkets: Map[String, FuturesMarket],
  spotMarkets: Map[String, SpotMarket],
  currencies: Map[UOM, Currency]
  ) {
  def futuresMarket(market: String) = futuresMarkets.get(market)
  def futuresMarketOrThrow(name: String) = futuresMarket(name).getOrElse(sys.error(s"No futures market $name"))

  def spotMarket(market: String) = spotMarkets.get(market)
  def spotMarketOrThrow(market: String) = spotMarket(market).getOrElse(sys.error(s"No spot market $market"))

  def currency(uom: UOM): Option[Currency] = currencies.get(uom)
  def currencyOrThrow(uom: UOM) = currency(uom).getOrElse(sys.error(s"No currency for $uom"))
}
