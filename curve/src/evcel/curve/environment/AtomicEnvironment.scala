package evcel.curve.environment

import evcel.quantity.Qty


trait AtomicEnvironment {
  def marketDay: MarketDay
  def apply(id: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Any]
  def typedApply[T](id: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, T] =
    apply(id).asInstanceOf[Either[AtomicEnvironmentFail, T]]

  def qty(id: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Qty] = typedApply[Qty](id)
  def double(id: AtomicDatumIdentifier): Either[AtomicEnvironmentFail, Double] = typedApply[Double](id)
}

trait AtomicEnvironmentFail {
  def s: String
}

case class GeneralAtomicEnvironmentFail(s: String) extends AtomicEnvironmentFail

case class MissingCurveData(identifier: String, point: Any) extends AtomicEnvironmentFail {
  def s = s"Missing curve data $identifier, $point"
}

case class MissingCurve(identifier: String, point: Any)  extends AtomicEnvironmentFail {
  def s = s"Missing curve $identifier - was asking for $point"
}
