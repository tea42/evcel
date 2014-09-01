package com.evcel.core.qty

import com.evcel.core.qty.UOM._
import scala.language.implicitConversions

trait Qty {
  def +(other: Qty): Qty
  def -(other: Qty): Qty
  def *(other: Qty): Qty
  def /(other: Qty): Qty

  def doubleValue: Double
  def checkedDouble(uom: UOM): Double = {
    require(this.uom == uom, "UOMs don't match: " + (this.uom, uom))
    doubleValue
  }
  def bdValue: BigDecimal
  def checkedBDValue(uom: UOM): BigDecimal = {
    require(this.uom == uom, "UOMs don't match: " + (this.uom, uom))
    bdValue
  }

  def uom: UOM
  def negate: Qty
  def invert: Qty
  def in(other: UOM, conv: Option[QtyConversions] = None): Option[Qty]
  override def toString() = doubleValue + " " + uom

  def isFixedPoint: Boolean
  def ensuringFixedPoint: Qty
}

class DblQty private[qty] (private val value: Double, val uom: UOM) extends Qty {
  def +(other: Qty) = uom.add(other.uom).map(
    scale => new DblQty(value + (other.doubleValue / scale.doubleValue()), uom)
  ).getOrElse(throw new RuntimeException("Can't add uoms: " + (uom, other.uom)))

  def -(other: Qty) = this.+(other.negate)

  def *(other: Qty) = uom.mult(other.uom) match {
    case (newUOM, mult) => new DblQty(value * other.doubleValue * mult.doubleValue(), newUOM)
  }

  def /(other: Qty) = this.*(other.invert)

  def negate = new DblQty(value * -1, uom)
  def invert = new DblQty(1 / value, uom.invert)

  override def in(other: UOM, conv: Option[QtyConversions]) = {
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
}

class BDQty private[qty] (private val value: BigDecimal, val uom: UOM) extends Qty {
  def +(other: Qty) = other match {
    case _: DblQty => dblQty.+(other)
    case _ => uom.add(other.uom).map(
      scale => new BDQty(value + (other.bdValue / scale), uom)
    ).getOrElse(throw new RuntimeException("Can't add uoms: " + (uom, other.uom)))
  }

  def -(other: Qty) = this.+(other.negate)

  def *(other: Qty) = other match {
    case _: DblQty => dblQty.*(other)
    case _ => uom.mult(other.uom) match {
      case (newUOM, mult) => new BDQty(value * other.bdValue * mult, newUOM)
    }
  }

  def /(other: Qty) = this.*(other.invert)

  def negate = new BDQty(value * BigDecimal(-1), uom)
  def invert = new BDQty(BigDecimal("1") / value, uom.invert)

  override def in(other: UOM, conv: Option[QtyConversions]) = {
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
}

object Qty {
  def apply(value: Double, uom: UOM): Qty = new DblQty(value, uom)

  def apply(value: String, uom: UOM): Qty = apply(BigDecimal(value), uom)

  def apply(value: BigDecimal, uom: UOM): Qty = new BDQty(value, uom)

  // we don't have a double to scalar to avoid people accidentally using a double like .1
  // and losing precision.
  // if you want to divide by a scalar double then write it out long form.
  implicit def intToScalarQty(value: Int) = Qty(BigDecimal(value), SCALAR)

  implicit def bigDecimalToScalarQty(value: BigDecimal) = Qty(value, SCALAR)

  implicit class RichDblQty(value: Double) {
    def apply(uom: UOM) = Qty(value, uom)
  }
}

