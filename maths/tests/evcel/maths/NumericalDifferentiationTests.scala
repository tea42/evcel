package evcel.maths

import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.math._

class NumericalDifferentiationTests extends FunSpec with Matchers {
  describe("Numerical derivative") {
    it("Should be close to analytic") {
      NumericalDifferentiation(sin _, 1e-6).firstDerivative(0.5) should equal(cos(0.5) +- 1e-6)
      NumericalDifferentiation(log _, 1e-6).firstDerivative(0.5) should equal(2.0 +- 1e-6)

    }
  }
}
