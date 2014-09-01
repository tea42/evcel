package com.evcel.report

import com.evcel.Environment.Environment
import com.evcel.core.qty.UOM
import com.evcel.instrument.Instrument

class MtmPivotReport(env: Environment) extends PivotReport {
  override def rows(instr: Instrument) = {
    new MtmRow(instr, env) :: Nil
  }
}

class MtmRow(instr: Instrument, env: Environment) extends PivotRow {
  override def market = "?"

  override def period = None

  override def value(field: PivotField) = field match {
    case MtmPivotReportType.MtmField => instr.valuer.value(env, UOM.USD)
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
