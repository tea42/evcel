package evcel.maths.models

import evcel.maths.{Straddle, Put, Call, OptionRight}
import org.apache.commons.math3.distribution.NormalDistribution

/**
 * Code mostly from Option Pricing Formulas (Second edition) - Haug
 */
class BlackScholes(right: OptionRight, S: Double, K: Double, vol: Double, T: Double) {
  val d1 = (math.log(S / K) + (vol * vol / 2) * T) / (vol * Math.sqrt(T))
  val d2 = d1 - vol * math.sqrt(T)
  lazy val N1 = BlackScholes.NormalDist.cumulativeProbability(d1)
  lazy val Nm1 = BlackScholes.NormalDist.cumulativeProbability(-d1)
  lazy val N2 = BlackScholes.NormalDist.cumulativeProbability(d2)
  lazy val Nm2 = BlackScholes.NormalDist.cumulativeProbability(-d2)

  def undiscountedValue = right match {
    case Call => S * N1 - K * N2
    case Put => K * Nm2 - S * Nm1
    case Straddle => S * (N1 - Nm1) - K * (N2 - Nm2)
  }

  // undiscounted
  def analyticDelta = right match {
    case Call => N1
    case Put => N1 - 1.0
    case Straddle => 2 * N1 - 1.0
  }
}

object BlackScholes {
  val NormalDist = new NormalDistribution(0, 1.0)
}
