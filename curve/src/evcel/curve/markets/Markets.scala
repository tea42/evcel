package evcel.curve.markets

class Markets(
  futuresMarkets: Map[String, FuturesMarket],
  spotMarkets: Map[String, SpotMarket]
  ) {
  def futuresMarket(market: String) = futuresMarkets.get(market)

  def spotMarket(market: String) = spotMarkets.get(market)
}
