package evcel.quantity.utils

import evcel.quantity.{BDQty, Qty, UOM}
import org.scalactic.Equality
import org.scalatest.matchers.{MatchResult, Matcher}
import scalaz.{Equal, Show}
import scala.language.implicitConversions
import org.scalatest.matchers.BeMatcher

object QuantityTestUtils {
  class EssentiallyEqual(tol: BigDecimal = BigDecimal("1e-20")) extends Equality[Qty] {
    def areEqual(a: Qty, b: Any): Boolean =
      (a, b) match {
        case (c: Qty, d: Qty) => (c - d).abs < tol
        case _ => false
      }

  }

  def matchQty(other: Qty) = QtyMatcher(other)

  implicit def withinToleranceMatcher(rhs : Qty) = new {
    def +- (tol : Double) = new BeMatcher[Qty]{
      def apply(lhs : Qty) : MatchResult = {
        require(lhs.uom == rhs.uom, s"UOMs don't match - $lhs vs $rhs")
        val matches = lhs.doubleValue >= rhs.doubleValue - tol && 
          lhs.doubleValue <= rhs.doubleValue + tol
          MatchResult(
            matches,
            s"$lhs was not in range $rhs +- $tol",
            s"$lhs was in range $rhs +- $tol"
          )
      }
    }
  }

  implicit def quantityEqual(implicit tol: BigDecimal = BigDecimal("1e-20")): Equal[Qty] = Equal.equal[Qty]{
    case (lhs, rhs) => new EssentiallyEqual(tol).areEqual(lhs, rhs)
  }

  implicit def quantityShows(implicit tol: BigDecimal = BigDecimal("1e-20")): Show[Qty] = {
    var dp = 0
    while (tol * BigDecimal("10").pow(dp) < 1) {
      dp += 1
    }
    Show.show[Qty](_.toFormattedString(dp))
  }

  implicit val uomEqual: Equal[UOM] = Equal.equalA[UOM]
  implicit val uomShow: Show[UOM] = Show.showA[UOM]

  implicit class eqExtras[F] (val self: F)(implicit val F: Equal[F]) {
    import scalaz.syntax.equal._

    final def assert_=/=[B](other: B)(implicit S: Show[F], ev: B <:< F) =
        if (self.===(other)) sys.error(S.shows(self) + " == " + S.shows(ev(other)))
  }
}

case class QtyMatcher(other: Qty) extends Matcher[Qty] {
  def apply(qty: Qty): MatchResult = {
    val result = new QuantityTestUtils.EssentiallyEqual().areEqual(qty, other)
    MatchResult(result, qty + " did not match " + other, qty + " shouldn't have matched " + other)
  }
}

