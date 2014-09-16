package evcel.curve.environment

import evcel.curve.ReferenceData
import evcel.daterange.DateRange

trait AtomicDatumIdentifier {
  def curveIdentifier: MarketDataIdentifier
  def point: Any
  def nullValue(refData: ReferenceData): Any
}

trait PriceIdentifier extends AtomicDatumIdentifier {
  def market: String
  def point: DateRange
}
