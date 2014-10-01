package evcel.curve.environment

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.daterange.DateRange
import evcel.quantity.Qty

trait AtomicDatumIdentifier {
  def curveIdentifier: MarketDataIdentifier
  def point: Any
  def nullValue(refData: ReferenceData): Any
  def forwardStateValue(refData: ReferenceData,
                        original: AtomicEnvironment,
                        forwardMarketDay: MarketDay): Either[AtomicEnvironmentFail, Any]
}

trait PriceIdentifier extends AtomicDatumIdentifier {
  def market: String
  def point: DateRange
  def dP(vc: ValuationContext): Qty
}
