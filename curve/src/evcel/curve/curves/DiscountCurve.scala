package evcel.curve.curves

import evcel.curve.ReferenceData
import evcel.quantity.Percent
import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.quantity.UOM._
import evcel.curve.marketdata.DayCount
import evcel.curve.environment.Curve
import evcel.curve.environment.CurveIdentifier
import scala.math._
import evcel.curve.marketdata.ZeroRateData
import evcel.curve.environment.AtomicDatumIdentifier
import evcel.quantity.Qty

abstract class DiscountCurve extends Curve {
  private[curves] def discountRate(day: Day): Double
  def currency: UOM
  def marketDay: Day
  def apply(point: Any) = {
    point match {
      case day: Day =>
        require(day >= marketDay, s"Asked for discount rate for $day - market day is $marketDay")
        discountRate(day)
      case _ => throw new RuntimeException(s"Unexpected point passed to discount curve $currency")
    }
  }
}

case class ForwardRateBasedDiscountCurve(
  marketDay: Day,
  currency: UOM,
  dayCount: DayCount,
  forwardRates: List[DiscountCurve.ForwardRateData])
    extends DiscountCurve {

  private[curves] def discountRate(day: Day): Double = {
    forwardRates.find(_.toDay >= day) match {
      case Some(DiscountCurve.ForwardRateData(fromDay, toDay, fromDiscount, toDiscount, forwardRate)) =>
        fromDiscount * exp(-forwardRate * dayCount.timeBetween(fromDay, day))
      case None =>
        val DiscountCurve.ForwardRateData(_, lastDay, _, lastDiscount, lastForwardRate) = forwardRates.last
        lastDiscount * exp(-lastForwardRate * dayCount.timeBetween(lastDay, day))
    }
  }
}

case class UndiscountedDiscountCurve(marketDay: Day, currency: UOM) extends DiscountCurve {
  private[curves] def discountRate(day: Day) = 1.0
}

case class DiscountCurveIdentifier(currency: UOM) extends CurveIdentifier
case class DiscountRateIdentifier(currency: UOM, day: Day) extends AtomicDatumIdentifier {
  def curveIdentifier = DiscountCurveIdentifier(currency)
  def point = day
  override def nullValue(refData: ReferenceData) = 1.0
}

object DiscountCurve {
  case class ForwardRateData(fromDay: Day, toDay: Day, fromDiscount: Double, toDiscount: Double, forwardRate: Double)

  def apply(zeroRateData: ZeroRateData): DiscountCurve = {
    import zeroRateData._
    apply(currency, marketDay, dayCount, rates)
  }

  private[curves] def apply(
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

