package evcel.curve.curves

import com.opengamma.analytics.math.interpolation.ConstrainedCubicSplineInterpolator
import evcel.curve.ReferenceData
import evcel.curve.environment._
import evcel.curve.marketdata.{Act365, FuturesVolData}
import evcel.daterange.Month
import evcel.maths.Call
import evcel.maths.models.BlackScholes
import evcel.quantity.{Percent, Qty}
import evcel.curve.environment.MarketDay._

/**
 * A non-sticky vol smile. Sticky might best be done by converting to a strike based
 * surface - yet to be writtern
 */
case class FuturesVols(
    market: String,
    marketDay: MarketDay,
    expiryRule: FuturesExpiryRule,
    atmVols : Month => Double,
    deltaSpreads: (Month, Double) => Double) extends Curve {
  def apply(point: Any): Either[AtomicEnvironmentFail, Qty] = {
    point match {
      case (month: Month, strike: Qty, forwardPrice: Qty) =>
        require(strike.uom == forwardPrice.uom, s"Mismatching strike/forward prices $strike/$forwardPrice")
        val vol = interpolateVol(month, strike.doubleValue, forwardPrice.doubleValue)
        Right(Percent(vol * 100.0))
    }
  }
  private[curves] def interpolateVol(month: Month, X: Double, F: Double) = {
    val atmVol = atmVols(month)
    val T = Act365.timeBetween(
      marketDay.day, expiryRule.optionExpiryDayOrThrow(month)
    )
    val deltaOfStrike = new BlackScholes(Call, F, X, T, atmVol).analyticDelta
    val spread = deltaSpreads(month, deltaOfStrike)
    atmVol + spread
  }
}

object FuturesVols {
  def apply(
    market : String,
    data: FuturesVolData,
    marketDay: MarketDay,
    expiryRule: FuturesExpiryRule): FuturesVols = {
    val atmVols = data.data.map{
      case (month, atmVol, _) => (month, atmVol.checkedPercent)
    }.toMap
    val spreadsByMonth: Map[Month, Double => Double] = {
      data.data.map {
        case (month: Month, _ : Qty, volsByDelta: List[(Double, Qty)]) =>
          val givenXs = volsByDelta.map(_._1)

          val givenYs = volsByDelta.map(_._2.checkedPercent)
          // Extend the data so as to force the interpolator to make a toilet-seat function
          val xs: Array[Double] = (-1.0 :: givenXs ::: List(2.0)).toArray
          val ys: Array[Double] = (givenYs.head :: givenYs ::: List(givenYs.last)).toArray

          month -> { x: Double => new ConstrainedCubicSplineInterpolator().interpolate(xs, ys, x) }
      }.toMap
    }
    def deltaSpreads(month: Month, delta: Double) = {
      val spreads = 
        spreadsByMonth.getOrElse(month, throw new RuntimeException(s"No smile data for $market/$month"))
      spreads(delta)
    }
    FuturesVols(market, marketDay, expiryRule, atmVols, deltaSpreads)
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
  override def nullValue(refData: ReferenceData) = Percent(10)

  override def forwardStateValue(refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay) = {
    val expiry = refData.futuresExpiryRules.expiryRule(market).map(_.optionExpiryDayOrThrow(month)).getOrElse(
      sys.error(s"Invalid market: $market")
    )
    if (forwardMarketDay >= expiry.endOfDay) {
      sys.error(s"$this has expired on $forwardMarketDay")
    }
    original(this)
  }
}


