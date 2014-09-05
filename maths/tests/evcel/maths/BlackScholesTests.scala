package evcel.maths

import org.scalatest.FunSpec
import org.scalatest.Matchers

class BlackScholesTests extends FunSpec with Matchers {
  describe("BlackScholes") {
    it("Should match known values") {
      var price = BlackScholes(F = 100.0, X = 100.0, payoffType = Call, T = 1.0, vol = 0.2).undiscountedPrice
      price should equal(7.965567 +- 1e-6)

      price = BlackScholes(150, 100, Put, 1, 0.2).undiscountedPrice
      price should equal(0.19247 +- 1e-5)
    }

    it("Should satisfy put/call parity") {
      val X = 100.0
      val T = 1.3
      val vol = 0.3
      for (F <- 70.0 to 130.0 by 1.0) {
        val callPrice = BlackScholes(F, X, Call, T, vol).undiscountedPrice
        val putPrice = BlackScholes(F, X, Put, T, vol).undiscountedPrice
        val fwdPrice = F - X

        (callPrice - putPrice) should equal(fwdPrice +- 1e-6)
      }
    }

    it("Should have the correct intrinsic price") {
      info("itm call")
      BlackScholes(110, 100, Call, 1.0, 0.0).undiscountedPrice should equal(10.0 +- 1e-9)
      info("otm call")
      BlackScholes(90, 100, Call, 1.0, 0.0).undiscountedPrice should equal(0.0 +- 1e-9)
      info("itm put")
      BlackScholes(95, 100, Put, 1.0, 0.0).undiscountedPrice should equal(5.0 +- 1e-9)
      info("otm put")
      BlackScholes(105, 100, Put, 1.0, 0.0).undiscountedPrice should equal(0.0 +- 1e-9)
    }

    it("Should have an analytic delta close to numeric") {
      def bs(F: Double) = BlackScholes(F, 100, Call, 1.0, 0.5)
      for (F <- 90.0 to 110.0 by 1.0) {
        val numericDelta = NumericalDifferentiation(bs(_).undiscountedPrice, 1e-6).firstDerivative(F) 
        bs(F).analyticDelta should equal(numericDelta +- 1e-6)
      }
    }
  }

}
