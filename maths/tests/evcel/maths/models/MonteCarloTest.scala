package evcel.maths.models

import evcel.maths.Call
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.{FunSuite, Matchers}

class MonteCarloTest extends FunSuite with Matchers {

  test("at expiry") {
    val params = MCParams(S = 100, vol = .25, T = 0, b = 0.0)
    val valuation = EuropeanOptionMC.valuation(Call, 100)
    val mc = new MonteCarlo(1000, 1, new MersenneTwister(1234), params)
    val (mcv, se) = mc.value(valuation, discount = 1.0)
    mcv shouldEqual 0.0
    se shouldEqual 0.0
  }
}
