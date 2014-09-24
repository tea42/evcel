package evcel.quantity

import org.apache.commons.math3.util.ArithmeticUtils
import scala.annotation.tailrec
import scalaz.Scalaz._

case class UOM(dimension: UOMRatio, secondary: UOMRatio) {
  /**
   * @return Some(magnitude) or None if not a valid addition.
   *         magnitude is the value to divide the second component by.
   *         e.g. if you had USD.add(USC) you would be returned 100.0
   */
  def add(other: UOM): Option[BigDecimal] = other match {
    case UOM(`dimension`, `secondary`) => Some(1.0)
    case UOM(`dimension`, _) => Some(magnitude / other.magnitude)
    case _ => None
  }

  def subtract(other: UOM) = add(other)

  def *(other: UOM): UOM = mult(other)._1

  def /(other: UOM): UOM = mult(other.invert)._1

  def mult(other: UOM): (UOM, BigDecimal) = {
    val newUOM = UOM(dimension * other.dimension, secondary * other.secondary)
    val dimGCD = newUOM.dimension.gcd
    // if the dimension has a gcd > 1 then we have some redundancy in the UOM, something like
    // USDBBL/CENT. We need to reduce this to BBL
    if (dimGCD > 1) {
      // reduce the dimension component, e.g. MASSVOLUME/VOLUME => MASS
      val uomReducedDimensions = newUOM.copy(dimension = newUOM.dimension.reduce)

      // now reduce the secondary component, e.g. MTBBL/BBL => MT
      val redundantPrimes = newUOM.dimension.redundantPrimes
      val secondaryPrimes = redundantPrimes.flatMap(p => UOM.byDimension(p).map(u => u.secondary.num.toInt))
      val numDivisible = secondaryPrimes.filter(p => newUOM.secondary.num % p == 0)
      val denDivisible = secondaryPrimes.filter(p => newUOM.secondary.den % p == 0)

      val uomReducedSecondary = numDivisible.zip(denDivisible).foldLeft(uomReducedDimensions) {
        case (uom, (p1, p2)) =>
          val newRatio = UOMRatio(uom.secondary.num / p1, uom.secondary.den / p2)
          uom.copy(secondary = newRatio)
      }

      (uomReducedSecondary.intern, newUOM.magnitude / uomReducedSecondary.magnitude)
    } else if (this.isPercent && other.isScalar) {
      (this, 1.0)
    } else if (this.isPercent && other.isPercent) {
      (this, other.magnitude)
    } else if (this.isPercent) {
      (other, this.magnitude)
    } else if (other.isPercent) {
      (this, other.magnitude)
    } else {
      (newUOM, 1.0)
    }
  }

  def div(other: UOM) = mult(other.invert)

  def invert = UOM(dimension.invert, secondary.invert)

  def magnitude: BigDecimal = {
    asPrimeMap.foldLeft(BigDecimal(1.0)) {
      case (bd, (prime, power)) => bd * UOM.conversionToDimensionBase(prime).pow(power)
    }
  }

  def asPrimeMap: Map[Int, Int] = {
    val num = secondary.factorNum.groupBy(identity).mapValues(a => a.size)
    val den = secondary.factorDen.groupBy(identity).mapValues(_.size * -1)
    num.mappend(den)
  }

  def in(other: UOM, conversions: Option[QtyConversions] = None): Option[BigDecimal] = if (this == other) {
    Some(BigDecimal(1.0))
  } else {
    def toUOMAndPower(single: Map[Int, Int]) = {
      require(single.size == 1, "Can't convert from/to: " + (this, other, single))
      val (prime, power) = single.head
      (UOM.primeToUOM(prime), power)
    }
    val (oldUOM, power) = toUOMAndPower(asPrimeMap -- other.asPrimeMap.keySet)
    val (newUOM, powerN) = toUOMAndPower(other.asPrimeMap -- asPrimeMap.keySet)
    require(power == powerN, "Powers should match: " + (power, powerN))

    oldUOM.div(newUOM) match {
      case (UOM.SCALAR, scale) => Some(scale.pow(power))
      case _ => // custom conversion
        conversions.flatMap(_.rate(oldUOM, newUOM)).map(r => r.pow(power))
    }
  }

  private lazy val string = {
    def formatPower(reduced: UOMRatio, primes: List[Int], negate: Boolean) = reduced match {
      case UOMRatio(i, 1) if i < 2 => UOM.symbols(i.toInt).head :: Nil // special case for null and scalar
      case _ =>
        primes.groupBy(identity).map {
          case (prime, all) =>
            val size = all.size
            if (size == 1) {
              UOM.symbolFor(prime) + (if (negate) "^-1" else "")
            } else {
              UOM.symbolFor(prime) + (if (negate) "^-" else "^") + size
            }
        }
    }

    // we only care about the secondary type for string representation
    val reduced = secondary.reduce
    val num = reduced.factorNum
    val den = reduced.factorDen
    val numString = formatPower(reduced, num, negate = false).mkString("")
    if (den.nonEmpty && numString.isEmpty)
      formatPower(reduced, den, negate = true).mkString("")
    else if (den.nonEmpty)
      numString + "/" + formatPower(reduced, den, negate = false).mkString("")
    else
      numString
  }

  private def intern = UOM.uoms.getOrElse(this, this)

  override def toString = string

  lazy val isScalar = this == UOM.SCALAR
  lazy val isPercent = asPrimeMap.keySet == UOM.PERCENT.asPrimeMap.keySet

  lazy val numerator = secondary.factorNum.groupBy(identity).map{
    case (prime, instances) => UOM.primeToUOM(prime).pow(instances.size)
  }.foldLeft(UOM.SCALAR)(_*_)
  lazy val denominator = invert.numerator

  def pow(n: Int) = {
    @tailrec
    def rec(uom: UOM, rem: Int): UOM = if (rem == 0)
      UOM.SCALAR
    else if (rem == 1)
      uom
    else
      rec(uom * uom, rem - 1)
    if(n < 0)
      rec(this, n.abs).invert
    else
      rec(this, n)
  }
}

object UOM {
  private var symbols = Map[Int, List[String]]()
  private var byDimension = Map[Int, List[UOM]]()
  private var primeToUOM = Map[Int, UOM]()
  private var uoms = Map[UOM, UOM]()
  private val primesIterator = Primes.primes()
  private var conversionToDimensionBase = Map[Int, BigDecimal]()

  val NULL = UOM(0, 0, "NULL")

  val SCALAR = UOM(1, 1, "")
  val PERCENT = UOM(UnitDimension.SCALAR, 0.01, "%")

  // ccys
  val USD = UOM(UnitDimension.USD, 1.0, "USD")
  val US_CENT = UOM(UnitDimension.USD, 0.01, "¢", "USC")
  val GBP = UOM(UnitDimension.GBP, 1.0, "GBP")
  val EUR = UOM(UnitDimension.EUR, 1.0, "EUR")

  // oil
  val BBL = UOM(UnitDimension.OilVolume, 1.0, "BBL")
  val GAL = UOM(UnitDimension.OilVolume, 1 / BigDecimal(42.0), "GAL")

  // mass
  val G = UOM(UnitDimension.Mass, 1.0, "G")
  val MT = UOM(UnitDimension.Mass, 1e6, "MT")

  private def symbolFor(prime: Int) = symbols(prime).head

  lazy val primesUsed = (symbols ++ byDimension).keys.toList.filter(_ >= 2).sortWith(_ > _)

  private def apply(dimension: UnitDimension, magnitude: BigDecimal, strs: String*) = UOM.synchronized {
    val prime = primesIterator.next()
    val uom = new UOM(UOMRatio(dimension.prime, 1), UOMRatio(prime, 1))
    symbols += prime -> strs.toList
    byDimension += dimension.prime -> (uom :: byDimension.getOrElse(dimension.prime, Nil))
    conversionToDimensionBase += prime -> magnitude
    primeToUOM += prime -> uom
    uoms += (uom -> uom)
    uom
  }

  private def apply(dimension: Int, secondary: Int, str: String) = UOM.synchronized {
    val uom = new UOM(UOMRatio(dimension, 1), UOMRatio(secondary, 1))
    symbols += secondary -> (str :: Nil)
    byDimension += dimension -> (uom :: byDimension.getOrElse(dimension, Nil))
    primeToUOM += secondary -> uom
    uom
  }
}

/*
 * Based on prime factorisation
 * If we want to represent BBL^2USD, and BBL => 3, USD => 5, we have 3 * 3 * 5 => 45
 * 45 can only be factored back into 3,3,5 (using primes) so we can recover the unit.
 * For BBl^2/USD we set num => 3*3, den => 5
*/
case class UOMRatio(num: Long, den: Long) {

  def redundantPrimes: List[Int] = if (num == 0) {
    Nil
  } else {
    var gcd = ArithmeticUtils.gcd(num, den)
    var reduced = this
    var primes = List[Int]()
    while (gcd > 1) {
      reduced = copy(num / gcd, den / gcd)
      primes :::= Primes.factor(gcd, UOM.primesUsed)
      gcd = ArithmeticUtils.gcd(reduced.num, reduced.den)
    }
    primes
  }

  // reduces something like USDBBL/USD to BBL
  def reduce: UOMRatio = redundantPrimes.foldLeft(this)((u, p) => u.copy(u.num / p, u.den / p))

  def gcd = ArithmeticUtils.gcd(num, den)

  def *(other: UOMRatio) = UOMRatio(num * other.num, den * other.den)

  def /(other: UOMRatio) = this * other.invert

  def invert = UOMRatio(den, num)

  def factorNum = if (num <= 1) Nil else Primes.factor(num, UOM.primesUsed)

  def factorDen = if (den <= 1) Nil else Primes.factor(den, UOM.primesUsed)
}

