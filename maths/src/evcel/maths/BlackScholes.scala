package evcel.maths

import scala.math._
import org.apache.commons.math3.distribution.NormalDistribution

sealed trait OptionPayoffType {
  def isInTheMoney(X: Double, F: Double): Boolean
}
case object Call extends OptionPayoffType {
  def isInTheMoney(X: Double, F: Double): Boolean = F > X
}
case object Put extends OptionPayoffType {
  def isInTheMoney(X: Double, F: Double): Boolean = F < X
}

case class BlackScholes(F: Double, X: Double, payoffType: OptionPayoffType, T: Double, vol: Double) {
  require(T >= 0, "Negative time not allowed")
  require(vol >= 0, "Negative vol not allowed")

  val isWorthIntrinsic = T == 0.0 || vol == 0.0

  val (_N1, n1, _N2) = if (isWorthIntrinsic) {
    payoffType match {
      case Call if Call.isInTheMoney(X, F) => (1.0, 0.0, 1.0)
      case Put if !Put.isInTheMoney(X, F) => (1.0, 0.0, 1.0)
      case _ => (0.0, 0.0, 0.0)
    }
  } else {
    val d1 = (log(F / X) + vol * vol * T / 2.0) / (vol * sqrt(T))
    val d2 = d1 - vol * sqrt(T)
    val stdNorm = new NormalDistribution()
    (stdNorm.cumulativeProbability(d1), stdNorm.density(d1), stdNorm.cumulativeProbability(d2))
  }

  def undiscountedPrice = {
    payoffType match {
      case Call => F * _N1 - X * _N2
      case Put => -F * (1 - _N1) + X * (1 - _N2)
    }
  }

  def analyticDelta = {
    payoffType match {
      case Call => _N1
      case Put => _N1 - 1.0
    }
  }
}

