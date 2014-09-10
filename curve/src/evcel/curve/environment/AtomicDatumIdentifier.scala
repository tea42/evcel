package evcel.curve.environment

import evcel.curve.ReferenceData

trait AtomicDatumIdentifier {
  def curveIdentifier: CurveIdentifier
  def point: Any
  def nullValue(refData: ReferenceData): Any
}
