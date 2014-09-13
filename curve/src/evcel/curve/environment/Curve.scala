package evcel.curve.environment

trait Curve {
  def apply(point: Any): Either[AtomicEnvironmentFail, Any]
}
