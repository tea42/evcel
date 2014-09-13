package evcel.curve.environment

case class CurveBasedAtomicEnvironment(
  marketDay: MarketDay,
  curves: Map[CurveIdentifier, Curve])
    extends AtomicEnvironment {
  def apply(identifier: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Any]  = {
    val point = identifier.point
    curves.get(identifier.curveIdentifier) match {
      case Some(curve) => curve.apply(point)
      case None => Left(MissingCurve(identifier.curveIdentifier.toString, point))
    }
  }
}
