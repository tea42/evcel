package evcel.maths

trait Numberlike[T] {
  def add(x: T, y: T): T

  def subtract(x: T, y: T): T

  def multiply(x: T, y: T): T

  def multiply(x: T, y: Double): T

  def divide(x: T, y: T): T

  def divide(x: T, y: Double): T
}

object Numberlike {

  implicit object DoubleNumberlike extends Numberlike[Double] {
    override def add(x: Double, y: Double): Double = x + y

    override def divide(x: Double, y: Double): Double = x / y

    override def multiply(x: Double, y: Double): Double = x * y

    override def subtract(x: Double, y: Double): Double = x - y
  }

}