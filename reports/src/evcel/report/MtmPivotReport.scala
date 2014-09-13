package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument
import evcel.instrument.valuation.InstrumentValuationContext
import evcel.quantity.Qty

class MtmPivotReport(vc: ValuationContext, ivc: InstrumentValuationContext) extends PivotReport {
  override def rows(instr: Instrument) = {
    new MtmRow(instr, vc, ivc) :: Nil
  }
}

class MtmRow(instr: Instrument, vc: ValuationContext, ivc: InstrumentValuationContext) extends PivotRow {
  override def market = "?"

  override def period = None

  override def value(field: PivotField): Qty = field match {
    case MtmPivotReportType.MtmField => ivc.valuer.value(vc, instr)
    case _ => throw new RuntimeException("Invalid field : " + field)
  }
}

object MtmPivotReportType extends PivotReportType {
  override def create(vc: ValuationContext, ivc: InstrumentValuationContext) = new MtmPivotReport(vc, ivc)

  override def fields = List(MtmField)

  val MtmField = new PivotField {
    override def name = "Mtm"
  }
}
