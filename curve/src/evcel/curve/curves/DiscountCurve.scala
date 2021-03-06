package evcel.curve.curves

import evcel.referencedata.ReferenceData
import evcel.quantity._
import evcel.daterange.Day
import evcel.quantity.UOM._
import evcel.curve.marketdata._
import evcel.curve.environment._
import scala.math._
import evcel.utils.EitherUtils._
import scala.util.{Either, Left, Right}
import scala.collection.immutable.Nil

abstract class DiscountCurve extends Curve {
  private[curves] def discountRate(day: Day): DblQty
  def currency: UOM
  def marketDay: Day
  def apply(point: Any): Either[AtomicEnvironmentFail, DblQty] with Product with Serializable = {
    point match {
      case day: Day =>
        require(day >= marketDay, s"Asked for discount rate for $day - market day is $marketDay")
        Right(discountRate(day))
      case _ => Left(GeneralAtomicEnvironmentFail(s"Unexpected point passed to discount curve $currency"))
    }
  }
}

case class ForwardRateBasedDiscountCurve(
  marketDay: Day,
  currency: UOM,
  dayCount: DayCount,
  forwardRates: List[DiscountCurve.ForwardRateData])
    extends DiscountCurve {

  private[curves] def discountRate(day: Day): DblQty = {
    forwardRates.find(_.toDay >= day) match {
      case Some(DiscountCurve.ForwardRateData(fromDay, toDay, fromDiscount, toDiscount, forwardRate)) =>
        Qty(fromDiscount * exp(-forwardRate * dayCount.timeBetween(fromDay, day)), UOM.SCALAR)
      case None =>
        val DiscountCurve.ForwardRateData(_, lastDay, _, lastDiscount, lastForwardRate) = forwardRates.last
        Qty(lastDiscount * exp(-lastForwardRate * dayCount.timeBetween(lastDay, day)), UOM.SCALAR)
    }
  }
}

case class UndiscountedDiscountCurve(marketDay: Day, currency: UOM) extends DiscountCurve {
  private[curves] def discountRate(day: Day) = Qty(1.0, UOM.SCALAR)
}

case class DiscountRateIdentifier(currency: UOM, day: Day) extends AtomicDatumIdentifier {
  def curveIdentifier = ZeroRatesIdentifier(currency)
  def point = day
  override def nullValue = Qty(1.0, UOM.SCALAR)

  override def forwardStateValue(refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay) = {
    for(d1 <- original(this); d2 <- original(copy(day = forwardMarketDay.day))) yield d1 / d2
  }
}

object DiscountCurve {
  case class ForwardRateData(fromDay: Day, toDay: Day, fromDiscount: Double, toDiscount: Double, forwardRate: Double)

  def apply(
    currency: UOM,
    marketDay: Day, 
    dayCount: DayCount, 
    rates: List[(Day, Qty)]): DiscountCurve = 
  {
    rates.foreach {
      case (day, _) =>
        require(
          day > marketDay, 
          s"Attempting to construct a discount curve with a day, $day, before the market day $marketDay")
    }
    rates match {
      case Nil =>
        UndiscountedDiscountCurve(marketDay, currency)
      case _ =>
        var fromDiscount = 1.0
        val sortedRates = ((marketDay, Percent(0)) :: rates).sortWith(_._1 < _._1)

        val forwardRates = sortedRates.zip(sortedRates.tail).map {
          case ((fromDay, _), (toDay, toRate)) =>
            val tFrom = dayCount.timeBetween(marketDay, fromDay)
            val tTo = dayCount.timeBetween(marketDay, toDay)
            val toDiscount = exp(-toRate.checkedDouble(PERCENT) / 100.0 * tTo)
            val fwdRate = log(fromDiscount / toDiscount) / (tTo - tFrom)
            val rateData = ForwardRateData(fromDay, toDay, fromDiscount, toDiscount, fwdRate)
            fromDiscount = toDiscount
            rateData
        }
        ForwardRateBasedDiscountCurve(marketDay, currency, dayCount, forwardRates)
    }
  }
}

