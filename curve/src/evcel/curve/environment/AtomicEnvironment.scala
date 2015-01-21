package evcel.curve.environment

import evcel.quantity.Qty
import scala.util.Either
import evcel.utils.EvcelFail


trait AtomicEnvironment {
  def marketDay: MarketDay
  def apply(id: AtomicDatumIdentifier): Either[EvcelFail, Qty]
}

trait AtomicEnvironmentFail extends EvcelFail

case class GeneralAtomicEnvironmentFail(s: String) extends AtomicEnvironmentFail

case class MissingCurveData(identifier: String, point: Any) extends AtomicEnvironmentFail {
  def s = s"Missing curve data $identifier, $point"
}

case class MissingCurve(identifier: String, point: Any)  extends AtomicEnvironmentFail {
  def s = s"Missing curve $identifier - was asking for $point"
}
