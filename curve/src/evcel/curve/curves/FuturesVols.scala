package evcel.curve.curves

import evcel.curve.environment.Curve
import evcel.daterange.Month
import evcel.curve.environment.MarketDay
import evcel.curve.environment.CurveIdentifier
import evcel.quantity.Percentage
import evcel.quantity.Qty
import evcel.quantity.UOM._
import evcel.curve.environment.AtomicDatumIdentifier
import evcel.maths.BlackScholes
import evcel.maths.Call
import evcel.curve.marketdata.Act365
import evcel.curve.marketdata.FuturesVolData
import com.opengamma.analytics.math.interpolation.ConstrainedCubicSplineInterpolator

/**
 * A non-sticky vol smile. Sticky might best be done by converting to a strike based
 * surface - yet to be writtern
 */
case class FuturesVols(
    market: String,
    marketDay: MarketDay,
    expiryRule: FuturesExpiryRule,
    surface: (Month, Double) => Double) extends Curve {
  def apply(point: Any) = {
    point match {
      case (month: Month, strike: Qty, forwardPrice: Qty) =>
        require(strike.uom == forwardPrice.uom, s"Mismatching strike/forward prices $strike/$forwardPrice")
        val vol = interpolateVol(month, strike.doubleValue, forwardPrice.doubleValue)
        Percentage(vol * 100.0)
    }
  }
  private[curves] def interpolateVol(month: Month, X: Double, F: Double) = {
    // ATM delta isn't exactly 0.5, however the difference isn't likely to be of use
    // to any trader
    val atmVol = surface(month, 0.5)
    val T = Act365.timeBetween(
      marketDay.day, expiryRule.optionExpiryDay(month)
    )
    val deltaOfStrike = BlackScholes(F, X, Call, T, atmVol).analyticDelta
    surface(month, deltaOfStrike)
  }
}

object FuturesVols {
  def apply(
    data: FuturesVolData,
    marketDay: MarketDay,
    expiryRule: FuturesExpiryRule): FuturesVols = {
    require(marketDay.day == data.marketDay, "Market day mismatch")
    val smiles: Map[Month, Double => Double] = {
      data.data.map {
        case (month: Month, volsByDelta: List[(Double, Qty)]) =>
          val givenXs = volsByDelta.map(_._1)

          val givenYs = volsByDelta.map(_._2.checkedDouble(PERCENT) / 100.0)
          // Extend the data so as to force the interpolator to make a toilet-seat function
          val xs: Array[Double] = (-1.0 :: givenXs ::: List(2.0)).toArray
          val ys: Array[Double] = (givenYs.head :: givenYs ::: List(givenYs.last)).toArray

          month -> { x: Double => new ConstrainedCubicSplineInterpolator().interpolate(xs, ys, x) }
      }.toMap
    }
    def surface(month: Month, delta: Double) = {
      val smile = smiles.getOrElse(month, throw new RuntimeException(s"No smile data for $data.market/$month"))
      smile(delta)
    }
    FuturesVols(data.market, marketDay, expiryRule, surface _)
  }
}
case class FuturesVolIdentifier(
  market: String,
  month: Month,
  strike: Qty,
  forwardPrice: Qty)
    extends AtomicDatumIdentifier {
  def curveIdentifier = FuturesVolsIdentifier(market)
  def point = (month, strike, forwardPrice)
}
case class FuturesVolsIdentifier(market: String) extends CurveIdentifier

