package evcel.maths.models

import evcel.daterange.Periodicity
import evcel.maths.BarrierType._
import evcel.maths._
import evcel.utils.DoubleUtils._

import scala.math._

/**
 * Code mostly from Option Pricing Formulas (Second edition) - Haug
 *
 * b = 0 option on future
 * b = r option on non-dividend paying stock
 * b = r - q socking paying a dividend yield of q
 */
class MertonReinerRubinsteinBarrier private(periodicity: Periodicity, right: OptionRight, barrier: BarrierType,
                                            S: Double, X: Double, unadjustedH: Double, T: Double, v: Double,
                                            rebate: Double, r: Double, b: Double) {

  private val H = MertonReinerRubinsteinBarrier.adjustH(unadjustedH, S, v, periodicity)

  private def CND(x: Double) = BlackScholes.NormalDist.cumulativeProbability(x)

  private val mu = (b - v.pow(2) / 2) / v.pow(2)
  private val lambda = sqrt(mu.pow(2) + 2 * r / v.pow(2))
  private val X1 = log(S / X) / (v * sqrt(T)) + (1 + mu) * v * sqrt(T)
  private val X2 = log(S / H) / (v * sqrt(T)) + (1 + mu) * v * sqrt(T)
  private val y1 = log(H.pow(2) / (S * X)) / (v * sqrt(T)) + (1 + mu) * v * sqrt(T)
  private val y2 = log(H / S) / (v * sqrt(T)) + (1 + mu) * v * sqrt(T)
  private val z = log(H / S) / (v * sqrt(T)) + lambda * v * sqrt(T)

  private val (eta, phi) = (right, barrier.upOrDown) match {
    case (Call, BarrierDown) => (1, 1)
    case (Call, BarrierUp) => (-1, 1)
    case (Put, BarrierDown) => (1, -1)
    case (Put, BarrierUp) => (-1, -1)
    case o => sys.error("Not supported" + o)
  }

  private def A =
    phi * S * exp((b - r) * T) * CND(phi * X1) - phi * X * exp(-r * T) * CND(phi * X1 - phi * v * sqrt(T))

  private def B =
    phi * S * exp((b - r) * T) * CND(phi * X2) - phi * X * exp(-r * T) * CND(phi * X2 - phi * v * sqrt(T))

  private def C =
    phi * S * exp((b - r) * T) * (H / S).pow(2 * (mu + 1)) * CND(eta * y1) - 
      phi * X * exp(-r * T) * (H / S).pow(2 * mu) * CND(eta * y1 - eta * v * sqrt(T))

  private def D =
    phi * S * exp((b - r) * T) * (H / S).pow(2 * (mu + 1)) * CND(eta * y2) - 
      phi * X * exp(-r * T) * (H / S).pow(2 * mu) * CND(eta * y2 - eta * v * sqrt(T))

  private def E =
    rebate * exp(-r * T) * (CND(eta * X2 - eta * v * sqrt(T)) - 
      (H / S).pow(2 * mu) * CND(eta * y2 - eta * v * sqrt(T)))

  private def F =
    rebate * ((H / S).pow(mu + lambda) * CND(eta * z) + 
      (H / S).pow(mu - lambda) * CND(eta * z - 2 * eta * lambda * v * sqrt(T)))

  def undiscountedValue = value * exp(r * T)

  // use undiscountedValue and discount it instead of calling value.
  // this is to make sure you get the discount to the right day
  def value = {
    barrier.inOrOut match {
      case BarrierOut if barrier.upOrDown.barrierTriggered(S, unadjustedH) => // already knocked  out. just rebate
        rebate
      case BarrierIn if barrier.upOrDown.barrierTriggered(S, unadjustedH) => // already knocked in. same as BS
        BlackScholes(right, S, X, v, T, b).undiscountedValue * exp(-r * T)
      case _ =>
        if (X >= H) {
          (right, barrier) match {
            case (Call, DownAndIn) => C + E
            case (Call, UpAndIn) => A + E
            case (Put, DownAndIn) => B - C + D + E
            case (Put, UpAndIn) => A - B + D + E
            case (Call, DownAndOut) => A - C + F
            case (Call, UpAndOut) => F
            case (Put, DownAndOut) => A - B + C - D + F
            case (Put, UpAndOut) => B - D + F
            case o => sys.error("Not supported" + o)
          }
        } else {
          (right, barrier) match {
            case (Call, DownAndIn) => A - B + D + E
            case (Call, UpAndIn) => B - C + D + E
            case (Put, DownAndIn) => A + E
            case (Put, UpAndIn) => C + E
            case (Call, DownAndOut) => B + F - D
            case (Call, UpAndOut) => A - B + C - D + F
            case (Put, DownAndOut) => F
            case (Put, UpAndOut) => A - C + F
            case o => sys.error("Not supported" + o)
          }
        }
    }
  }
}

object MertonReinerRubinsteinBarrier {
  def apply(periodicity: Periodicity, right: OptionRight, barrier: BarrierType, S: Double, X: Double, H: Double,
            T: Double, v: Double, rebate: Double, r: Double, b: Double): MertonReinerRubinsteinBarrier = {
    new MertonReinerRubinsteinBarrier(periodicity, right, barrier, S, X, H, T, v, rebate, r, b)
  }

  private[models] def adjustH(H: Double, S: Double, v: Double, periodicity: Periodicity) = {
    // Broadie, Glasserman and Kou discrete barrier adjustment (Haug pg 164)
    // 0.5826 ≈ ζ(1/2)/sqrt(2π) -- where ζ is Riemann zeta function
    val adjustedH = if (H >= S)
      H * exp(0.5826 * v * periodicity.years.sqrt)
    else
      H * exp(-0.5826 * v * periodicity.years.sqrt)

    adjustedH
  }
}
