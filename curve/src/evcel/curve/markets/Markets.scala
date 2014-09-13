package evcel.curve.markets

class Markets(
  futuresMarkets: Map[String, FuturesMarket],
  spotMarkets: Map[String, SpotMarket]
  ) {
  def futuresMarket(market: String) = futuresMarkets.get(market)
  def futuresMarketOrThrow(name: String) = futuresMarket(name).getOrElse(sys.error(s"No futures market $name"))

  def spotMarket(market: String) = spotMarkets.get(market)
  def spotMarketOrThrow(market: String) = spotMarket(market).getOrElse(sys.error(s"No spot market $market"))
}
