package evcel.curve.marketdata

import com.opengamma.analytics.math.interpolation.ConstrainedCubicSplineInterpolator
import evcel.curve.ReferenceData
import evcel.curve.curves.FuturesVols
import evcel.curve.environment._
import evcel.quantity.{Qty, BDQty}
import evcel.daterange.Month
import evcel.daterange.Day
import evcel.quantity.UOM._

case class FuturesVolData(data: List[(Month, BDQty, List[(Double, BDQty)])])
  extends MarketData
{
  def buildCurve(market : String, marketDay: MarketDay, refData: ReferenceData) = {
    refData.futuresExpiryRules.expiryRule(market) match {
      case Some(expiryRule) =>

        val atmVols = data.map{
          case (month, atmVol, _) => (month, atmVol.checkedDouble(PERCENT) / 100.0)
        }.toMap
        val spreadsByMonth: Map[Month, Double => Double] = {
          data.map {
            case (month: Month, _ : Qty, volsByDelta: List[(Double, Qty)]) =>
              val spreadFunction = if (volsByDelta.isEmpty)
                { x: Double => 0.0 }
              else {
                val givenXs = volsByDelta.map(_._1)

                val givenYs = volsByDelta.map(_._2.checkedDouble(PERCENT) / 100.0)
                // Extend the data so as to force the interpolator to make a toilet-seat function
                val xs: Array[Double] = (-1.0 :: givenXs ::: List(2.0)).toArray
                val ys: Array[Double] = (givenYs.head :: givenYs ::: List(givenYs.last)).toArray

                { x: Double => new ConstrainedCubicSplineInterpolator().interpolate(xs, ys, x) }
              }
              month -> spreadFunction
          }.toMap
        }
        def deltaSpreads(month: Month, delta: Double) = {
          val spreads = spreadsByMonth.getOrElse(month, throw new RuntimeException(s"No smile data for $data.market/$month"))
          spreads(delta)
        }
        Right(FuturesVols(market, marketDay, expiryRule, atmVols, deltaSpreads _))
      case None =>
        Left(MarketData.CantBuildCurve(FuturesVolsIdentifier(market), marketDay, "No expiry rule found"))
    }

  }
}

