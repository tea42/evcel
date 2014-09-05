package evcel.curve.environment

import evcel.daterange.Day

trait Curve {
  def apply(point: Any): Any
}
