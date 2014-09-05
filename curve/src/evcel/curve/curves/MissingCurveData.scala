package evcel.curve.curves
import evcel.curve.environment.CurveIdentifier

case class MissingCurveData(identifier: String, point: Any) 
  extends RuntimeException(s"Missing curve data $identifier, $point")

case class MissingCurve(identifier: String, point: Any) 
  extends RuntimeException(s"Missing curve $identifier - was asking for $point")
