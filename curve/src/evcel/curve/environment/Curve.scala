package evcel.curve.environment

import scala.util.Either
import evcel.utils.EvcelFail
import evcel.quantity.Qty

trait Curve {
  def apply(point: Any): Either[EvcelFail, Qty]
}
