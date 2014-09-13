package evcel.quantity.utils

import evcel.quantity.{Qty, BDQty}
import org.scalactic.Equality
import org.scalatest.matchers.{MatchResult, Matcher}

object QuantityTestUtils {
  class EssentiallyEqual(tol: BigDecimal = BigDecimal("1e-20")) extends Equality[Qty] {
    def areEqual(a: Qty, b: Any): Boolean =
      (a, b) match {
        case (c: Qty, d: Qty) => (c - d).abs < tol
        case _ => false
      }

  }

  def matchQty(other: Qty) = QtyMatcher(other)
}

case class QtyMatcher(other: Qty) extends Matcher[Qty] {
  def apply(qty: Qty): MatchResult = {
    val result = new QuantityTestUtils.EssentiallyEqual().areEqual(qty, other)
    MatchResult(result, qty + " did not match " + other, qty + " shouldn't have matched " + other)
  }
}

