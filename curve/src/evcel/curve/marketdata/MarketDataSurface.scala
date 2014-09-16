package evcel.curve.marketdata

import evcel.curve.environment._
import evcel.utils.EitherUtils._
import evcel.quantity.UOM

trait MarketDataSurface{
  import MarketDataSurface._
  def marketDay : MarketDay
  protected def data(key : MarketDataIdentifier) : Either[NotFound, MarketData]

  private def typedData[T <: MarketData](curveIdentifier : MarketDataIdentifier) =
    data(curveIdentifier).map(_.asInstanceOf[T])

  def futuresPrices(market : String)  : Either[NotFound, FuturesPriceData] = typedData(FuturesPricesIdentifier(market))
  def zeroRates(currency : UOM)       : Either[NotFound, ZeroRateData] = typedData(ZeroRatesIdentifier(currency))
  def futuresVols(market : String)    : Either[NotFound, FuturesVolData] = typedData(FuturesVolsIdentifier(market))
  def spotPrices(market : String)     : Either[NotFound, SpotPriceData] = typedData(SpotPricesIdentifier(market))
}

object MarketDataSurface{
  case class NotFound(key : MarketDataIdentifier)

  case class TestSurface(marketDay : MarketDay, surface : Map[MarketDataIdentifier, MarketData]) 
    extends MarketDataSurface
  {
    def data(key : MarketDataIdentifier) = surface.get(key) match {
      case Some(data) => Right(data)
      case None => Left(NotFound(key))
    }
  }
}
