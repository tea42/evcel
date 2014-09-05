package com.evcel.models

import com.evcel.utils.DoubleTestUtils
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
      new BlackScholes(cp, 100, k, vol, t, r).discountedValue shouldEqual
        discFuturesOptionPrice(cp, 100, k, vol, t, r)
    }
  }

  test("supersymmetry") {
    for (k <- 80.until(120)) {
      new BlackScholes(Call, 100, k, .2, .5, .05).discountedValue shouldEqual
        -1 * new BlackScholes(Put, 100, k, -.2, .5, .05).discountedValue
    }

  }

}
