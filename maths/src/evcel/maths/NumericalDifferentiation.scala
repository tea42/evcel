package evcel.maths

case class NumericalDifferentiation(fn: Double => Double, dX: Double) {
  val firstDerivative: Double => Double = {
    x =>
      //(fn(x + dX) - fn(x - dX)) / (2.0 * dX)
      (-fn(x + 2 * dX) + 8 * fn(x + dX) - 8 * fn(x - dX) + fn(x - 2 * dX)) / (12.0 * dX)
  }
}
