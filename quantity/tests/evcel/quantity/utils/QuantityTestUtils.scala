package evcel.quantity.utils

import evcel.quantity.BDQty
import org.scalactic.Equality

object QuantityTestUtils {

  class EssentiallyEqual(tol: BigDecimal = BigDecimal("1e-20")) extends Equality[Any] {
    def areEqual(a: Any, b: Any): Boolean =
      (a, b) match {
        case (c: BDQty, d: BDQty) => (c - d).abs < tol
        case _ => false
      }

  }

}
