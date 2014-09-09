package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument

trait PivotRow {
  def market: String

  def period: Option[String] // should be a type so we can sort sensibly

  def value(field: PivotField): Any
}

trait PivotReport {
  def rows(instr: Instrument): Iterable[PivotRow]
}

trait PivotReportType {
  def create(vc: ValuationContext): PivotReport

  def fields: List[PivotField]
}

trait PivotField {
  def name: String
}
