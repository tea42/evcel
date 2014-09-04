package evcel.maths

import scala.collection.immutable.NumericRange
import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.opengamma.analytics.math.interpolation.ConstrainedCubicSplineInterpolator

class ConstrainedCubicSplineTests extends FunSpec with Matchers {

  /* Tests behaviour of http://developers.opengamma.com/ 
     constrained cubic spline implementation. In turn they 
     got it from 

     C.J.C. Kruger, "Constrained Cubic Spline Interpolation for Chemical Engineering Applications," 2002
  */

  def interpolator(xs: Array[Double], ys: Array[Double])(x: Double): Double = {
    new ConstrainedCubicSplineInterpolator().interpolate(xs, ys, x)
  }

  describe("ConstrainedCubicSpline") {

    it("Should go through points") {
      val xs = Array(10.0, 20.0, 30.0, 40.0, 50.0)
      val ys = Array(.5, .4, .44, .55, .65)
      val interp = interpolator(xs, ys) _
      xs.zip(ys).foreach {
        case (x, y) =>
          interp(x) should equal(y +- 1e-6)
      }
    }

    it("Should go through straight line") {
      val xs = Array(1.0, 2.0, 3.0, 4.0)
      val ys = Array(1.0, 2.0, 3.0, 4.0)
      val interp = interpolator(xs, ys) _
      for (x <- 1.0 to 4.0 by 0.05) {
        interp(x) should equal(x +- 1e-6)
      }
    }

    it("Should not overshoot") {
      val xs = Array(1.0, 3.0, 9.0, 10.0)
      val ys = Array(0.0, 5.0, 5.0, 8.0)
      val interp = interpolator(xs, ys) _
      for (x <- 3.0 to 9.0 by 0.5) {
        interp(x) should equal(5.0 +- 1e-6)
      }
    }

    it("Should have derivative zero at boundary of segment of inflection") {
      val xs = Array(1.0, 3.0, 9.0, 10.0)
      val ys = Array(0.0, 5.0, 5.0, 8.0)
      val interp = interpolator(xs, ys) _

      val deriv = NumericalDifferentiation(interp, 1e-6).firstDerivative
      // bounds large compared to dX, as error is O(dX)
      deriv(3.0) should equal(0.0 +- 1e-4)
      deriv(9.0) should equal(0.0 +- 1e-4)
    }

  }
}
