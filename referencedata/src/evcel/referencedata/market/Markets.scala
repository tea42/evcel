package evcel.referencedata.market

import evcel.quantity.UOM
import evcel.utils.{GeneralEvcelFail, EvcelFail}
import scala.util.{Left, Right, Either}

class Markets(
  futuresMarkets: Map[String, FuturesMarket],
  spotMarkets: Map[String, SpotMarket],
  val currencies: Map[UOM, Currency]
  ) {
  def futuresMarket(label: String) : Either[EvcelFail, FuturesMarket] = 
    futuresMarkets.get(label).toRight(GeneralEvcelFail(s"Futures market $label is unknown"))

  def spotMarket(market: String) : Either[EvcelFail, SpotMarket] = 
    spotMarkets.get(market).toRight(GeneralEvcelFail(s"Spot market $market is unknown"))

  def currency(uom: UOM): Either[EvcelFail, Currency] = 
    currencies.get(uom).toRight(GeneralEvcelFail(s"No refernce data for currency $uom"))

  def currencyList = currencies.keySet.toList.sortBy(_.toString)
  def futuresMarketsList = futuresMarkets.values.toList
}
