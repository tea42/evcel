package evcel.curve.marketdata

import evcel.daterange.Month
import evcel.quantity.BDQty
import evcel.daterange.Day

/**
  * Convenience methods for building market data
  */
trait MarketDataTest{
  def futuresPrices(prices : (Month, BDQty)*) = {
    FuturesPriceData(prices.toList)
  }

  def futuresVols(
    firstSmile : (Month, BDQty, List[(Double, BDQty)]), 
    rest       : (Month, BDQty, List[(Double, BDQty)])* ) = 
  {
    FuturesVolData(firstSmile::(rest.toList))
  }

  def futuresVols(firstAtmVol : (Month, BDQty), rest : (Month, BDQty)*) = {
    val data = (firstAtmVol::(rest.toList)).map{
      case (m, v) => (m, v, Nil)
    }
    FuturesVolData(data)
  }
  
  def zeroRates(dayCount : DayCount, rates : (Day, BDQty)*) = {
    ZeroRateData(dayCount, rates.toList)
  }
}
