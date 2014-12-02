package evcel.utils

object DoubleUtils {
  implicit class RicherDouble(d: Double) {
    def pow(pow: Double) = math.pow(d, pow)
    def sqrt = math.sqrt(d)
  }
}
