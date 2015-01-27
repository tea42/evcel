package evcel.utils

object DoubleUtils {
  implicit class RicherDouble(d: Double) {
    def pow(pow: Double) = math.pow(d, pow)
    def sqrt = math.sqrt(d)
  }
}

object ParseDouble {
  def unapply(s: String) = try {
    Some(s.toDouble)
  }
  catch {
    case e: NumberFormatException => None
  }
}

object ParseInt {
  def unapply(s: String) = try {
    Some(s.toInt)
  }
  catch {
    case e: NumberFormatException => None
  }
}