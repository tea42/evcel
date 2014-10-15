package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument
import evcel.quantity.Qty
import evcel.instrument.valuation.Valuer

class MtmPivotReport(vc: ValuationContext, valuer : Valuer) extends PivotReport {
  override def rows(instr: Instrument) = {
    new MtmRow(instr, vc, valuer) :: Nil
  }
}

class MtmRow(instr: Instrument, vc: ValuationContext, valuer : Valuer) extends PivotRow {
  override def market = "?"

  override def period = None

  override def value(field: PivotField): Qty = field match {
    case MtmPivotReportType.MtmField => valuer.value(vc, instr)
    case _ => throw new RuntimeException("Invalid field : " + field)
  }
}

object MtmPivotReportType extends PivotReportType {
  override def create(vc: ValuationContext, valuer : Valuer) = new MtmPivotReport(vc, valuer)

  override def fields = List(MtmField)

  val MtmField = new PivotField {
    override def name = "Mtm"
  }
}
