package evcel.curve

import evcel.referencedata.calendar.TestCalendars
import evcel.referencedata.TestFuturesExpiryRules
import evcel.curve.environment._
import evcel.curve.marketdata._
import evcel.referencedata.market.TestMarkets
import java.util.concurrent.ConcurrentHashMap
import scala.collection.breakOut
import evcel.quantity.UOM
import evcel.referencedata.ReferenceData

object UnitTestingEnvironment {
  def testRefData = ReferenceData(
    TestFuturesExpiryRules.Test,
    TestCalendars.Empty,
    TestMarkets.Default
  )

  def apply(marketDay_ : MarketDay, data: PartialFunction[AtomicDatumIdentifier, Any]): ValuationContext = {
    new ValuationContext(
      new AtomicEnvironment {
        def marketDay = marketDay_

        def apply(point: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Any]  = {
          if(data.isDefinedAt(point))
            Right(data(point))
          else
            Left(GeneralAtomicEnvironmentFail(s"Missing market data for $point"))
        }
      },
      testRefData,
      EnvironmentParams.Default
    )
  }

  def fromMarketData(
    marketDay_ : MarketDay, 
    data : (AnyRef, MarketData)*
  ): ValuationContext = {

    val curves = {
      var map = Map[MarketDataIdentifier, Either[AnyRef, Curve]]()
      data.foreach{
        case (market : String, fp : FuturesPriceData) =>
          map += (FuturesPricesIdentifier(market) -> fp.buildCurve(market, marketDay_))

        case (market : String, fv : FuturesVolData) =>
          map += (FuturesVolsIdentifier(market) -> fv.buildCurve(market, marketDay_, testRefData))

        case (ccy : UOM, zr : ZeroRateData) =>
          map += (ZeroRatesIdentifier(ccy) -> zr.buildCurve(ccy, marketDay_))

        case ((from: UOM, to: UOM), spot: SpotFXData) =>
          map += (SpotFXIdentifier(from, to) -> spot.buildCurve(from, to))

        case (market : String, sp : SpotPriceData) =>
          map += (SpotPricesIdentifier(market) -> sp.buildCurve(market, marketDay_))

        case other => throw new RuntimeException(s"Unexpected pair $other")

      }
      map
    }

    new ValuationContext(
      new AtomicEnvironment{
        def marketDay = marketDay_
        def apply(point: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Any]  = {
          curves.get(point.curveIdentifier) match {
            case Some(Right(curve)) => curve.apply(point.point)
            case Some(Left(e)) => {
              Left(GeneralAtomicEnvironmentFail(
                s"Couldn't build curve for $point from supplied market data due to $e"))
              }
            case None =>
              Left(GeneralAtomicEnvironmentFail(s"No market data provided for $point"))
          }
        }
      },
      testRefData,
      EnvironmentParams.Default
    )
  }

  def Null(marketDay: MarketDay) = apply(marketDay, {
    case a => a.nullValue(testRefData)
  })
}
