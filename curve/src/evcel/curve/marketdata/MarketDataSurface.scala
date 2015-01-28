package evcel.curve.marketdata

import evcel.curve.environment._
import evcel.daterange.Day
import evcel.referencedata.market.{Observable, Index}
import evcel.utils.EitherUtils._
import evcel.quantity.UOM

trait MarketDataSurface {

  import MarketDataSurface._

  protected def data(marketDay: Day, key: MarketDataIdentifier): Either[NotFound, MarketData]

  private def typedData[T <: MarketData](marketDay: Day, curveIdentifier: MarketDataIdentifier) =
    data(marketDay, curveIdentifier).map(_.asInstanceOf[T])

  def futuresPrices(marketDay: Day, market: String): Either[NotFound, FuturesPriceData] =
    typedData(marketDay, FuturesPricesIdentifier(market))

  def zeroRates(marketDay: Day, currency: UOM): Either[NotFound, ZeroRateData] =
    typedData(marketDay, ZeroRatesIdentifier(currency))

  def futuresVols(marketDay: Day, market: String): Either[NotFound, FuturesVolData] =
    typedData(marketDay, FuturesVolsIdentifier(market))

  def spotPrices(marketDay: Day, market: String): Either[NotFound, SpotPriceData] =
    typedData(marketDay, SpotPricesIdentifier(market))

  def fixing(marketDay: Day, observable: Observable): Either[NotFound, PriceFixingData] =
    typedData(marketDay, PriceFixingsIdentifier(observable.label, observable.level))
}

object MarketDataSurface {

  case class NotFound(marketDay: Day, key: MarketDataIdentifier)

  case class TestSurface(surface: Map[Day, Map[MarketDataIdentifier, MarketData]])
    extends MarketDataSurface {
    def data(marketDay: Day, key: MarketDataIdentifier) = {
      val result = for (entries <- surface.get(marketDay); data <- entries.get(key)) yield {
        data
      }
      result.toRight(NotFound(marketDay, key))
    }
  }

}
