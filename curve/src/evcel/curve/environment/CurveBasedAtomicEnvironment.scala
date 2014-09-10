package evcel.curve.environment

import evcel.curve.curves.MissingCurveException
import evcel.curve.curves.MissingCurveDataException

case class CurveBasedAtomicEnvironment(
  marketDay: MarketDay,
  curves: Map[CurveIdentifier, Curve])
    extends AtomicEnvironment {
  def apply(identifier: AtomicDatumIdentifier): Any = {
    val point = identifier.point
    curves.get(identifier.curveIdentifier) match {
      case Some(curve) => curve(point)
      case None => throw new MissingCurveException(identifier.curveIdentifier.toString, point)
    }
  }
}
