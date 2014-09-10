package evcel.curve.curves

case class MissingCurveDataException(identifier: String, point: Any)
  extends RuntimeException(s"Missing curve data $identifier, $point")

case class MissingCurveException(identifier: String, point: Any) 
  extends RuntimeException(s"Missing curve $identifier - was asking for $point")
