package evcel.report

import evcel.curve.Environment
import evcel.instrument.Instrument
import evcel.instrument.valuation.Valuer
import evcel.quantity.UOM

class MtmPivotReport(env: Environment) extends PivotReport {
  override def rows(instr: Instrument) = {
    new MtmRow(instr, env) :: Nil
  }
}

class MtmRow(instr: Instrument, env: Environment) extends PivotRow {
  override def market = "?"

  override def period = None

  override def value(field: PivotField) = field match {
    case MtmPivotReportType.MtmField => Valuer.value(env, UOM.USD, instr)
    case _ => throw new RuntimeException("Invalid field : " + field)
  }
}

object MtmPivotReportType extends PivotReportType {
  override def create(env: Environment) = new MtmPivotReport(env)

  override def fields = List(MtmField)

  val MtmField = new PivotField {
    override def name = "Mtm"
  }
}
