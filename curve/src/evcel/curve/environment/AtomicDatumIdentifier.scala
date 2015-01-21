package evcel.curve.environment

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.daterange.DateRange
import evcel.quantity.Qty
import scala.util.Either
import evcel.utils.EvcelFail

trait AtomicDatumIdentifier {
  def curveIdentifier: MarketDataIdentifier
  def point: Any
  def nullValue: Qty
  def forwardStateValue(refData: ReferenceData,
                        original: AtomicEnvironment,
                        forwardMarketDay: MarketDay): Either[EvcelFail, Qty]
}

trait PriceIdentifier extends AtomicDatumIdentifier {
  def point: DateRange
  def dP(vc: ValuationContext): Qty
}
