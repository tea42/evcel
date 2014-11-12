package evcel.quantity

import evcel.utils.{Enumerate, Cache}
import org.apache.commons.math3.util.ArithmeticUtils
import pimpathon.GenTraversableLikeCapturer
import scala.annotation.tailrec
import scalaz.Scalaz._
import pimpathon.genTraversableLike._
import pimpathon.multiMap._

case class UOM(dimension: UOMRatio, secondary: UOMRatio) {
  /**
   * @return Some(magnitude) or None if not a valid addition.
   *         magnitude is the value to divide the second component by.
   *         e.g. if you had USD.add(USC) you would be returned 100.0
   */
  def add(other: UOM): Either[String, Option[BigDecimal]] = other match {
    case UOM(`dimension`, `secondary`) => Right(None)
    case UOM(`dimension`, _) => Right(Some(magnitude / other.magnitude))
    case _ => Left(s"Can't add $this and $other")
  }

  def subtract(other: UOM) = add(other)

  def *(other: UOM): UOM = mult(other)._1

  def /(other: UOM): UOM = mult(other.invert)._1

  def mult(other: UOM): (UOM, BigDecimal) = UOM.multCache.memoize((this, other)) {
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
      (this, Qty.bdOne)
    } else if (this.isPercent && other.isPercent) {
      (this, other.magnitude)
    } else if (this.isPercent) {
      (other, this.magnitude)
    } else if (other.isPercent) {
      (this, other.magnitude)
    } else {
      (newUOM, Qty.bdOne)
    }
  }

  def div(other: UOM) = mult(other.invert)

  def invert = UOM(dimension.invert, secondary.invert)

  def magnitude: BigDecimal = {
    asPrimeMap.foldLeft(Qty.bdOne) {
      case (bd, (prime, power)) => bd * UOM.conversionToDimensionBase(prime).pow(power)
    }
  }

  def asPrimeMap: Map[Int, Int] = {
    val num = secondary.factorNum.groupBy(identity).mapValues(a => a.size)
    val den = secondary.factorDen.groupBy(identity).mapValues(_.size * -1)
    num.mappend(den)
  }

  def in(other: UOM, conversions: Option[QtyConversions] = None): Option[BigDecimal] = if (this == other) {
    Some(Qty.bdOne)
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
    if (den.nonEmpty && numString.isEmpty) {
      formatPower(reduced, den, negate = true).mkString("")
    }else if (den.nonEmpty) {
      val denString = formatPower(reduced, den, negate = false).mkString("")
      if (isCcyPair)
        denString + numString
      else
        numString + "/" + denString
    } else {
      numString
    }
  }

  private def intern = {
    UOM.instances.getOrElse(this, this)
  }

  override def toString = string

  def isCcy = UOM.baseCurrenciesMap.contains(this)
  def isCcyPair = numerator.isCcy && denominator.isCcy

  lazy val isScalar = this == UOM.SCALAR
  lazy val isPercent = asPrimeMap.keySet == UOM.PERCENT.asPrimeMap.keySet

  lazy val numerator = secondary.factorNum.groupBy(identity).map{
    case (prime, instances) => UOM.primeToUOM(prime).pow(instances.size)
  }.foldLeft(UOM.SCALAR)(_*_)
  lazy val denominator = invert.numerator

  def inBaseCcy = {
    require(isCcy, "Not a currency: " + this)
    UOM.baseCurrenciesMap(this)
  }

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

  def isPerTimeUnit = dimension.den == UnitDimension.Time.prime

  def allNames = {
    if(this == UOM.NULL) {
      UOM.NULL.toString :: Nil
    } else if(this == UOM.SCALAR) {
      UOM.SCALAR.toString :: Nil
    } else {
      val primes = asPrimeMap
      require(primes.size == 1 && primes.values.head == 1, "Not valid on compound UOMs: " + this)
      UOM.symbols(primes.keys.head)
    }
  }
}

object UOM extends Enumerate[UOM](classOf[UOM], _.allNames){
  private val primesIterator = Primes.primes()
  private val multCache = Cache.createStaticCache("UOMCache.mult")

  case class UOMData(dimension: Int, secondary: Int, magnitude: BigDecimal, strs: List[String], uom: UOM)
  private var uomData: List[UOMData] = Nil

  val NULL = UOM(0, 0, "NULL")

  val SCALAR = UOM(1, 1, "")
  val PERCENT = UOM(UnitDimension.SCALAR, 0.01, "%")

  // ccys
  val USD = UOM(UnitDimension.USD, 1.0, "USD")
  val US_CENT = UOM(UnitDimension.USD, 0.01, "¢", "USC", "US_CENT")
  val GBP = UOM(UnitDimension.GBP, 1.0, "GBP")
  val PENCE = UOM(UnitDimension.GBP, 0.01, "p", "PENCE")
  val EUR = UOM(UnitDimension.EUR, 1.0, "EUR")
  val CAD = UOM(UnitDimension.CAD, 1.0, "CAD")
  val NZD = UOM(UnitDimension.NZD, 1.0, "NZD")
  val AUD = UOM(UnitDimension.AUD, 1.0, "AUD")
  val CHF = UOM(UnitDimension.CHF, 1.0, "CHF")
  val JPY = UOM(UnitDimension.JPY, 1.0, "JPY")

  val TRY = UOM(UnitDimension.TRY, 1.0, "TRY")
  val MXN = UOM(UnitDimension.MXN, 1.0, "MXN")

  // oil
  val BBL = UOM(UnitDimension.OilVolume, 1.0, "BBL")
  val GAL = UOM(UnitDimension.OilVolume, 1 / BigDecimal(42.0), "GAL")

  // gas
  val THM = UOM(UnitDimension.GasVolume, 1.0, "THM")
  val MMBTU = UOM(UnitDimension.GasVolume, 10.0, "MMBTU")

  // mass
  val G = UOM(UnitDimension.Mass, 1.0, "G")
  val MT = UOM(UnitDimension.Mass, 1e6, "MT")

  // time
  val DAY = UOM(UnitDimension.Time, 24 * 60 * 60, "Day")
  val SECOND = UOM(UnitDimension.Time, 1.0, "sec")

  private def symbolFor(prime: Int) = symbols(prime).head

  private def apply(dimension: UnitDimension, magnitude: BigDecimal, strs: String*) = UOM.synchronized {
    val prime = primesIterator.next()
    val uom = new UOM(UOMRatio(dimension.prime, 1), UOMRatio(prime, 1))
    val data = UOMData(dimension.prime, prime, magnitude, strs.toList, uom)
    uomData ::= data
    uom
  }

  private def apply(dimension: Int, secondary: Int, str: String) = UOM.synchronized {
    val uom = new UOM(UOMRatio(dimension, 1), UOMRatio(secondary, 1))
    val data = UOMData(dimension, secondary, 1.0, str :: Nil, uom)
    uomData ::= data
    uom
  }

  private lazy val instances: Map[UOM, UOM] = uomData.map(d => d.uom -> d.uom).toMap // used for interning.
  private lazy val byDimension: Map[Int, List[UOM]] = uomData.map(_.uom).asMultiMap.withKeys(_.dimension.num.toInt)
  private lazy val symbols = uomData.map(d => d.secondary -> d.strs).toMap
  private lazy val primeToUOM = uomData.map(d => d.secondary -> d.uom).toMap
  private lazy val conversionToDimensionBase = uomData.map(d => d.secondary -> d.magnitude).toMap

  private lazy val baseCurrenciesMap: Map[UOM, UOM] = { // USD -> US_CENT, USD -> USD, etc.
    val (ccyBase, ccyDenominations) = instances.keys.flatMap{
      uom =>
        if(UnitDimension.currencyPrimes.contains(uom.dimension.num.toInt))
           Some(uom)
        else
          None
    }.partition(_.magnitude == 1)

    ccyDenominations.map{
      uom => uom -> ccyBase.find(_.dimension == uom.dimension).getOrElse(sys.error("No base unit for " + uom))
    } ++ ccyBase.map(ccy => ccy -> ccy)
  }.toMap

  lazy val primesUsed: List[Int] =
    (instances.keys.toList.flatMap(
      uom => List(uom.dimension.num.toInt, uom.secondary.num.toInt)) ++ UnitDimension.primesUsed)
      .filter(_ > 1).distinct.sortWith(_ > _)
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

