package evcel.curve.curves

import evcel.curve.environment._
import evcel.daterange.Day
import evcel.quantity.Qty
import evcel.referencedata.ReferenceData
import evcel.referencedata.market.{Observable, FuturesFrontPeriodIndex, Index}

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
    // TODO DC: need to discuss how to do this
    original(this)
  }
}


