package evcel.curve.environment

import scala.util.{Either, Left}
import evcel.utils.EvcelFail
import evcel.quantity.Qty

case class CurveBasedAtomicEnvironment(
  marketDay: MarketDay,
  curves: Map[MarketDataIdentifier, Curve])
    extends AtomicEnvironment {
  def apply(identifier: AtomicDatumIdentifier): Either[EvcelFail, Qty]  = {
    val point = identifier.point
    curves.get(identifier.curveIdentifier) match {
      case Some(curve) => curve.apply(point)
      case None => Left(MissingCurve(identifier.curveIdentifier.toString, point))
    }
  }
}
