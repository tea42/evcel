package evcel.maths.models

import evcel.maths._
import evcel.maths.utils.DoubleTestUtils
import org.scalatest.{ ShouldMatchers, FunSuite }

class BlackScholesTest extends FunSuite with ShouldMatchers {
  // this is java code for BS converted to scala to test against. taken from
  //  http://www.espenhaug.com/black_scholes.html
  // this is the stock option model so we need to use r = 0.0 and discount the returned value
  private def bs(callPut: Char, S: Double, X: Double, v: Double, T: Double, r: Double): Double = {
    def CND(X: Double) = {
      val a1 = 0.31938153
      val a2 = -0.356563782
      val a3 = 1.781477937
      val a4 = -1.821255978
      val a5 = 1.330274429

      val L = math.abs(X)
      val K = 1.0 / (1.0 + 0.2316419 * L)
      val w = 1.0 - 1.0 / math.sqrt(2.0 * math.Pi) * math.exp(-L * L / 2) * (a1 * K + a2 * K * K + a3 *
        math.pow(K, 3) + a4 * math.pow(K, 4) + a5 * math.pow(K, 5))

      if (X < 0.0)
        1.0 - w
      else
        w
    }

    val d1 = (math.log(S / X) + (r + v * v / 2) * T) / (v * math.sqrt(T))
    val d2 = d1 - v * math.sqrt(T)

    callPut match {
      case 'c' => S * CND(d1) - X * math.exp(-r * T) * CND(d2)
      case 'p' => X * math.exp(-r * T) * CND(-d2) - S * CND(-d1)
      case 's' => bs('c', S, X, v, T, r) + bs('p', S, X, v, T, r)
      case _ => throw new RuntimeException("Not a valid call/put param: " + callPut)
    }
  }

  def discFuturesOptionPrice(right: OptionRight, S: Double, K: Double, vol: Double, T: Double, r: Double) = {
    bs(right.toChar, S, K, vol, T, 0.0) * math.exp(-r * T)
  }

  test("against haug") {
    implicit val equality = new DoubleTestUtils.AlmostEqualsPC(tolPC = 0.001, min = 0.001)
    for (
      cp <- List(Call, Put, Straddle);
      k <- 80.until(120);
      vol <- .01.until(.5, .05);
      t <- .01.until(2.0, .05);
      r <- .0.until(.2, .05)
    ) {
      new BlackScholes(cp, 100, k, vol, t).undiscountedValue * math.exp(-r * t) shouldEqual
        discFuturesOptionPrice(cp, 100, k, vol, t, r)
    }
  }

  test("supersymmetry") {
    for (k <- 80.until(120)) {
      new BlackScholes(Call, 100, k, .2, .5).undiscountedValue shouldEqual
        -1 * new BlackScholes(Put, 100, k, -.2, .5).undiscountedValue
    }
  }

  test("Should match known values") {
    var price = new BlackScholes(S = 100.0, K = 100.0, right = Call, T = 1.0, vol = 0.2).undiscountedValue
    price should equal(7.965567 +- 1e-6)

    price = new BlackScholes(S = 150, K = 100, right = Put, T = 1, vol = 0.2).undiscountedValue
    price should equal(0.19247 +- 1e-5)
  }

  test("Should satisfy put/call parity") {
    val X = 100.0
    val T = 1.3
    val vol = 0.3
    for (f <- 70.0 to 130.0 by 1.0) {
      val callPrice = new BlackScholes(S = f, K = X, right = Call, T = T, vol = vol).undiscountedValue
      val putPrice = new BlackScholes(S = f, K = X, right = Put, T = T, vol = vol).undiscountedValue
      val fwdPrice = f - X

      (callPrice - putPrice) should equal(fwdPrice +- 1e-6)
    }
  }

  test("Should have the correct intrinsic price") {
    info("itm call")
    new BlackScholes(Call, 110, 100, 1.0, 0.0).undiscountedValue should equal(10.0 +- 1e-9)
    info("otm call")
    new BlackScholes(Call, 90, 100, 1.0, 0.0).undiscountedValue should equal(0.0 +- 1e-9)
    info("itm put")
    new BlackScholes(Put, 95, 100, 1.0, 0.0).undiscountedValue should equal(5.0 +- 1e-9)
    info("otm put")
    new BlackScholes(Put, 105, 100, 1.0, 0.0).undiscountedValue should equal(0.0 +- 1e-9)
  }

  test("Should have an analytic delta close to numeric") {
    def bs(F: Double, right: OptionRight) = new BlackScholes(right, F, 100, 1.0, 0.5)
    for (f <- 90.0 to 110.0 by 1.0; right <- List(Call, Put, Straddle)) {
      val numericDelta = NumericalDifferentiation(bs(_, right).undiscountedValue, 1e-6).firstDerivative(f)
      bs(f, right).analyticDelta should equal(numericDelta +- 1e-6)
    }
  }

}
