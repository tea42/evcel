package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument
import evcel.instrument.valuation.Valuer
import evcel.quantity.UOM

class MtmPivotReport(vc: ValuationContext) extends PivotReport {
  override def rows(instr: Instrument) = {
    new MtmRow(instr, vc) :: Nil
  }
}

class MtmRow(instr: Instrument, vc: ValuationContext) extends PivotRow {
  override def market = "?"

  override def period = None

  override def value(field: PivotField) = field match {
    case MtmPivotReportType.MtmField => Valuer.value(vc, UOM.USD, instr)
    case _ => throw new RuntimeException("Invalid field : " + field)
  }
}

object MtmPivotReportType extends PivotReportType {
  override def create(vc: ValuationContext) = new MtmPivotReport(vc)

  override def fields = List(MtmField)

  val MtmField = new PivotField {
    override def name = "Mtm"
  }
}
