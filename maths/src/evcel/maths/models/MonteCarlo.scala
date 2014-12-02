package evcel.maths.models

import breeze.linalg.DenseMatrix
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.random.RandomGenerator


case class MCParams(b: Double, S: Double, T: Double, vol: Double)

trait MCValuation {

  def timeDependent: Boolean

  def stepAndCheckCanTerminate(iteration: Int, timeStep: Int, St: Double): Boolean

  def payoff(iteration: Int, St: Double): Double
}


trait MCValuationSimple extends MCValuation {
  def timeDependent = false

  override def stepAndCheckCanTerminate(iteration: Int, timeStep: Int, St: Double): Boolean = {
    sys.error("Not called")
  }

  def payoff(iteration: Int, St: Double) = payoff(St)

  def payoff(St: Double): Double
}

class MonteCarlo(iterations: Int, _timeSteps: Int, rand: RandomGenerator, params: MCParams) {
  import org.apache.commons.math3.util.FastMath._

  val timeSteps = _timeSteps + 1 // we always simulate time step 0
  val b = params.b
  val S = params.S
  val vol = params.vol
  val dT = params.T / (timeSteps - 1)
  val drift = (b - 0.5 * vol * vol) * dT
  val volSqrtDT = vol * sqrt(dT)

  private val pricePaths = {
    var i = 0
    val prices = DenseMatrix.zeros[Double](iterations, timeSteps)
    while (i < iterations) {
      prices(i, 0) = S
      prices(i + 1, 0) = S
      var j = 1
      while (j < timeSteps) {
        val z = rand.nextGaussian()
        val St = prices(i, j - 1) * exp(drift + z * volSqrtDT)
        prices(i, j) = St

        val StA = prices(i + 1, j - 1) * exp(drift + -z * volSqrtDT)
        prices(i + 1, j) = StA
        j += 1
      }
      i += 2
    }
    prices
  }

  def value(valuation: MCValuation, discount: Double): (Double, Double) = {
    val prices = pricePaths
    var i_path = 0
    var sumPayoff = 0.0
    var sumPayoffSq = 0.0
    val timeDependent = valuation.timeDependent
    while (i_path < iterations) {
      var j_time = 0
      if (timeDependent) {
        var terminate = false
        while (j_time < timeSteps && !terminate) {
          terminate = valuation.stepAndCheckCanTerminate(i_path, j_time, prices(i_path, j_time))
          j_time += 1
        }
      }
      val payoffi = valuation.payoff(i_path, prices(i_path, timeSteps - 1))
      sumPayoff += payoffi
      sumPayoffSq += payoffi * payoffi
      i_path += 1
    }
    val payoff = (sumPayoff / iterations) * discount
    val sd = sqrt((sumPayoffSq - sumPayoff * sumPayoff / iterations) / (iterations - 1)) * discount
    val se = sd / sqrt(iterations)
    (payoff, se)
  }
}
