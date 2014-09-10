package evcel.curve.markets

class Markets(
  futuresMarkets: Map[String, FuturesMarket],
  spotMarkets: Map[String, SpotMarket]
  ) {
  def futuresMarket(market: String) = futuresMarkets.get(market)
  def futuresMarketOrThrow(name: String) = futuresMarket(name).getOrElse(sys.error("No futures market: "+ name))

  def spotMarket(market: String) = spotMarkets.get(market)
}
