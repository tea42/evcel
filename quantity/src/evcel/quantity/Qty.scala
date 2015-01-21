package evcel.quantity

import java.text.DecimalFormat
import evcel.maths.Numberlike
import scala.language.implicitConversions
import UOM._
import scala.math.BigDecimal.RoundingMode
import scala.math.{BigDecimal, Ordered}
import scala.util.{Either, Left, Right}
import evcel.utils.EvcelFail
import evcel.utils.EitherUtils._




trait Qty extends Ordered[Qty] {
  def +(other: Qty): Qty
  def -(other: Qty): Qty
  def *(other: Qty): Qty
  def /(other: Qty): Qty

  def doubleValue: Double
  def checkedDouble(uom: UOM): Double = {
    require(this.uom == uom, "UOMs don't match: " + (this.uom, uom))
    doubleValue
  }
  def checkedPercent: Double = {
    require(this.uom == UOM.PERCENT, "Not a percent")
    doubleValue / 100.0
  }
  def bdValue: BigDecimal
  def checkedBDValue(uom: UOM): BigDecimal = {
    require(this.uom == uom, "UOMs don't match: " + (this.uom, uom))
    bdValue
  }

  def uom: UOM
  def negate: Qty
  def unary_- : Qty
  def invert: Qty
  def abs: Qty
  def in(other: UOM, conv: QtyConversions = QtyConversions(Map.empty)): Either[EvcelFail, Qty]
  override def toString = doubleValue + " " + uom

  def toFormattedString(dp: Int) = if (dp < 16) { // DecimalFormat doesn't work after 16 decimal places
    val format = new DecimalFormat("#,###" + (if (dp > 0) "." + "0" * dp else ""))
    format.setRoundingMode(java.math.RoundingMode.HALF_UP)
    format.format(bdValue) + " " + uom
  } else {
    bdValue + " " + uom
  }

  def isFixedPoint: Boolean
  def ensuringFixedPoint: Qty
  def isScalar = uom == UOM.SCALAR
  def isNull = uom == UOM.NULL

  def round(nDP: Int): Qty

  def one: Qty
  def inBaseCcy: Either[EvcelFail, Qty] = in(uom.inBaseCcy)

  def compare(that : Qty) = {
    require(that.uom == uom, s"UOMs don't match: $this, $that")
    doubleValue.compare(that.doubleValue)
  }

  /*
   * Convenience methods that allow us to test for positivity etc, with natural
   * looking code, but without implicit conversions
   */
  private def compareWithZero(zero : Int, comparison : (Double, Int) => Boolean) = {
    require(zero == 0, s"Convenience method for comparing $this with 0, no other number allowed")
    comparison(doubleValue, zero)
  }
  def > (zero : Int) = compareWithZero(zero, _ > _)
  def >= (zero : Int) = compareWithZero(zero, _ >= _)
  def < (zero : Int) = compareWithZero(zero, _ < _)
  def <= (zero : Int) = compareWithZero(zero, _ <= _)
}

class DblQty private[quantity] (private val value: Double, val uom: UOM) extends Qty {
  require(!value.isNaN && !value.isInfinity, "Invalid value: " + value)

  def +(other: Qty) = (this, other) match {
    case (_, Qty.NULL) => this
    case _ => uom.add(other.uom) match {
      case Right(None) => new DblQty(value + other.doubleValue, uom)
      case Right(Some(scale)) => new DblQty(value + (other.doubleValue / scale.doubleValue()), uom)
      case Left(error) => sys.error(error)
    }
  }

  def -(other: Qty) = this.+(other.negate)

  def *(other: Qty) = uom.mult(other.uom) match {
    case (newUOM, mult) => new DblQty(value * other.doubleValue * mult.doubleValue(), newUOM)
  }

  def /(other: Qty) = this.*(other.invert)

  def negate = new DblQty(value * -1, uom)
  override def unary_- = negate

  def invert = new DblQty(1 / value, uom.invert)
  def abs = new DblQty(math.abs(value), uom)

  override def in(other: UOM, conv: QtyConversions) = {
    uom.in(other, conv).map(scale => Qty(this.value * scale, other))
  }

  def doubleValue = value
  override def bdValue = BigDecimal(value)

  override def hashCode() = value.hashCode() ^ uom.hashCode()

  override def equals(obj: Any) = obj match {
    case other: Qty => value == other.doubleValue && uom == other.uom
    case _ => false
  }

  override def isFixedPoint = false
  def ensuringFixedPoint: Qty = throw new RuntimeException("Not fixed point Qty: " + this)

  override def round(nDP: Int) = Qty(value.toString, uom).round(nDP)

  def one: DblQty = new DblQty(1, uom)
}

class BDQty private[quantity] (private val value: BigDecimal, val uom: UOM) extends Qty {
  def +(other: Qty) = (this, other) match {
    case (Qty.NULL, _) => other
    case (_, Qty.NULL) => this
    case (_, _: DblQty) => dblQty.+(other)
    case _ => uom.add(other.uom) match {
      case Right(None) => new BDQty(value + other.bdValue, uom)
      case Right(Some(scale)) => new BDQty(value + (other.bdValue / scale), uom)
      case Left(error) => sys.error(error)
    }
  }

  def -(other: Qty) = this.+(other.negate)

  def *(other: Qty) = other match {
    case _: DblQty => dblQty.*(other)
    case _ => uom.mult(other.uom) match {
      case (newUOM, Qty.bdOne) => new BDQty(value * other.bdValue, newUOM)
      case (newUOM, mult) => new BDQty(value * other.bdValue * mult, newUOM)
    }
  }

  def mult(other: BDQty) = this.*(other).asInstanceOf[BDQty]
  def plus(other: BDQty) = this.+(other).asInstanceOf[BDQty]

  def /(other: Qty) = other match {
    case _: DblQty => dblQty./(other)
    case _ => uom.div(other.uom) match {
      case (newUOM, Qty.bdOne) => new BDQty(value / other.bdValue, newUOM)
      case (newUOM, mult) => new BDQty(value / other.bdValue * mult, newUOM)
    }
  }

  def negate = new BDQty(value * BigDecimal(-1), uom)
  override def unary_- = negate
  def invert = new BDQty(Qty.bdOne / value, uom.invert)
  def abs = new BDQty(value.abs, uom)

  override def in(other: UOM, conv: QtyConversions) = {
    uom.in(other, conv).map(scale => Qty(this.value * scale, other))
  }

  def doubleValue = value.toDouble
  override def bdValue = value

  override def hashCode() = value.hashCode() ^ uom.hashCode()

  override def equals(obj: Any) = obj match {
    case other: Qty => value.equals(other.bdValue) && uom == other.uom
    case _ => false
  }

  def dblQty = new DblQty(value.toDouble, uom)

  override def isFixedPoint = true
  def ensuringFixedPoint: Qty = this

  def round(nDP: Int) = new BDQty(value.setScale(nDP, RoundingMode.HALF_UP), uom)

  def one: BDQty = new BDQty(Qty.bdOne, uom)
}

object BDQty{
  
  def sum(qtys: Iterable[BDQty]) = {
    qtys.foldLeft[BDQty](Qty.NULL)(_ plus _)
  }
}

object Qty {
  val bdOne = BigDecimal(1.0)

  val NULL : BDQty = Qty(BigDecimal(0), UOM.NULL)

  def apply(value: Int, uom: UOM): BDQty = new BDQty(value, uom)

  def apply(value: Double, uom: UOM): DblQty = new DblQty(value, uom)

  def apply(value: String, uom: UOM): BDQty = apply(BigDecimal(value), uom)

  def apply(value: BigDecimal, uom: UOM): BDQty = new BDQty(value, uom)

  def average(qtys: Iterable[Qty]) = qtys.size match {
    case 0 => sys.error("Can't get the average zero quantities.")
    case 1 => qtys.head
    case _ => sum(qtys) / Qty(BigDecimal(qtys.size), SCALAR)
  }

  def sum(qtys: Iterable[Qty]) = {
    qtys.foldLeft[Qty](NULL)(_+_)
  }

  // we don't have a double to scalar to avoid people accidentally using a double like .1
  // and losing precision.
  // if you want to divide by a scalar double then write it out long form.
  implicit def intToScalarQty(value: Int) : BDQty = Qty(BigDecimal(value), SCALAR)

  implicit def bigDecimalToScalarQty(value: BigDecimal) : BDQty = Qty(value, SCALAR)

  implicit class RichIntQty(value: Int) {
    def apply(uom: UOM) : BDQty = Qty(value, uom)

    def toQty : BDQty = Qty(value, UOM.SCALAR)
  }
  implicit class RichDblQty(value: Double) {
    def apply(uom: UOM) : DblQty = Qty(value, uom)

    def toQty : DblQty = Qty(value, UOM.SCALAR)
  }

  implicit object QtyNumberlike extends Numberlike[Qty]{
    override def add(x: Qty, y: Qty): Qty = x + y

    override def divide(x: Qty, y: Qty): Qty = x / y

    override def multiply(x: Qty, y: Qty): Qty = x * y

    override def subtract(x: Qty, y: Qty): Qty = x - y

    override def multiply(x: Qty, y: Double): Qty = x * Qty(y, UOM.SCALAR)

    override def divide(x: Qty, y: Double): Qty = x / Qty(y, UOM.SCALAR)
  }
}
