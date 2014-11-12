package evcel.curve.curves

import evcel.curve.{EnvironmentParams, ValuationContext}
import evcel.curve.environment._
import evcel.quantity._
import evcel.referencedata.ReferenceData
import evcel.referencedata.market.FXPair

case class SpotFX(from: UOM, to: UOM, rate: BDQty) extends Curve {
  private val pair = FXPair(to, from)
  def apply(point: Any): Either[AtomicEnvironmentFail, BDQty] = point match {
    case `pair` => Right(rate)
    case o => sys.error("Invalid request: " + o)
  }
}

case class BaseFXRateKey(baseCCY: UOM, otherCCY: UOM) extends AtomicDatumIdentifier {
  override def curveIdentifier: MarketDataIdentifier = SpotFXIdentifier(baseCCY, otherCCY)

  override def forwardStateValue(refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay) = {
    val pair = FXPair(otherCCY, baseCCY)
    val newSpotDate = refData.fxMarket(pair).spotDate(refData, forwardMarketDay.day)
    Right(ValuationContext(original, refData, EnvironmentParams.Default).forwardFX(pair, newSpotDate))
  }

  override def nullValue(refData: ReferenceData): Any = Qty(1, baseCCY / otherCCY)

  override def point: Any = FXPair(otherCCY, baseCCY)
}

