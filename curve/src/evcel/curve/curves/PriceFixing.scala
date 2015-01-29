package evcel.curve.curves

import evcel.curve.{EnvironmentParams, ValuationContext, RichIndex}
import evcel.curve.environment._
import evcel.daterange.Day
import evcel.quantity.Qty
import evcel.referencedata.ReferenceData
import evcel.referencedata.market.{Observable, FuturesFrontPeriodIndex, Index}
import evcel.utils.EitherUtils._

case class PriceFixing(fixing: Qty) extends Curve {
  def apply(point: Any): Either[AtomicEnvironmentFail, Qty] = {
    point match {
      case Unit =>
        Right(fixing)
      case _ => Left(GeneralAtomicEnvironmentFail(s"Unexpected point $point"))
    }
  }
}

case class PriceFixingIdentifier(index: Observable, observationDay: Day)
  extends AtomicDatumIdentifier {

  val curveIdentifier = PriceFixingsIdentifier(index.label, index.level)
  val point = Unit // Point isn't needed

  override def nullValue = {
    Qty("123", index.priceUOM)
  }

  override def forwardStateValue(
    refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay
    ) = {
    RichIndex(refData, index).flatMap{
      ri => {
        if (ri.hasFixed(observationDay, original.marketDay)) {
          original(this)
        } else if (ri.hasFixed(observationDay, forwardMarketDay)) {
          // moved ahead of original environment, so we have no fixings for this observationDay
          // but we can use forward prices (as we are still before forwardMarketDay).
          val vc = ValuationContext(original, refData, EnvironmentParams.Default)
          ri.price(vc, observationDay)
        } else {
          sys.error(s"Looking for a fixing for a day that hasn't fixed yet: $observationDay/$forwardMarketDay")
        }
      }
    }
  }
}
