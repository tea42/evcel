package evcel.maths.utils

import org.scalactic.Equality

object DoubleTestUtils {
  class AlmostEqualsPC(tolPC: Double = 1e-9,
      min: Double = 1e-9,
      minNumbersSignsMayDiffer: Boolean = true) extends Equality[Double] {
    def areEqual(a: Double, b: Any): Boolean =
      b match {
        case d: Double if a.abs <= min && d.abs <= min =>
          minNumbersSignsMayDiffer || a * d >= 0
        case d: Double => ((a - d) / a).abs <= tolPC
        case _ => false
      }
  }
}
