package evcel.curve

import evcel.curve.curves.PriceFixingIdentifier
import evcel.curve.environment._
import evcel.curve.marketdata._
import evcel.daterange.Day
import evcel.quantity.{Qty, UOM}
import evcel.referencedata.calendar.TestCalendars
import evcel.referencedata.market.TestMarkets
import evcel.referencedata.{ReferenceData, TestFuturesExpiryRules}
import evcel.utils.{EitherTestPimps, EvcelFail}

import scala.util.{Either, Left, Right}

object UnitTestingEnvironment extends EitherTestPimps{
  def testRefData = ReferenceData(
    TestFuturesExpiryRules.Test,
    TestCalendars.Empty,
    TestMarkets.Default
  )

  def futuresMarket(name : String) = TestMarkets.Default.futuresMarket(name).R
  def apply(marketDay_ : MarketDay, data: PartialFunction[AtomicDatumIdentifier, Qty]): ValuationContext = {
    new ValuationContext(
      new AtomicEnvironment {
        def marketDay = marketDay_

        def apply(point: AtomicDatumIdentifier): Either[EvcelFail, Qty]  = {
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
      var map = Map[(Day, MarketDataIdentifier), Either[AnyRef, Curve]]()
      data.foreach{
        case (market : String, fp : FuturesPriceData) =>
          map += (marketDay_.day, FuturesPricesIdentifier(market)) -> fp.buildCurve(market, marketDay_)

        case (market : String, fv : FuturesVolData) =>
          map += (marketDay_.day, FuturesVolsIdentifier(market)) -> fv.buildCurve(market, marketDay_, testRefData)

        case (ccy : UOM, zr : ZeroRateData) =>
          map += (marketDay_.day, ZeroRatesIdentifier(ccy)) -> zr.buildCurve(ccy, marketDay_)

        case ((from: UOM, to: UOM), spot: SpotFXData) =>
          map += (marketDay_.day, SpotFXIdentifier(from, to)) -> spot.buildCurve(from, to)

        case (market : String, sp : SpotPriceData) =>
          map += (marketDay_.day, SpotPricesIdentifier(market)) -> sp.buildCurve(market, marketDay_)

        case (pfi@PriceFixingIdentifier(index, obDay), f: PriceFixingData) =>
          val id = PriceFixingsIdentifier(index.label, index.level)
          map += (obDay, id) -> f.buildCurve

        case other => throw new RuntimeException(s"Unexpected pair $other")

      }
      map
    }

    new ValuationContext(
      new AtomicEnvironment{
        def marketDay = marketDay_
        def apply(point: AtomicDatumIdentifier): Either[EvcelFail, Qty]  = {
          val key = point match {
            case PriceFixingIdentifier(index, obDay) => (obDay, point.curveIdentifier)
            case _ => (marketDay_.day, point.curveIdentifier)
          }
          curves.get(key) match {
            case Some(Right(curve)) => curve.apply(point.point)
            case Some(Left(e)) =>
              Left(GeneralAtomicEnvironmentFail(
                s"Couldn't build curve for $point from supplied market data due to $e"))
            case _ =>
              Left(GeneralAtomicEnvironmentFail(s"No market data for $key"))
          }
        }
      },
      testRefData,
      EnvironmentParams.Default
    ) 
  }

  def Null(marketDay: MarketDay) = apply(marketDay, {
    case a => a.nullValue
  })
}
