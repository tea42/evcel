package com.evcel.report

import com.evcel.Environment.Environment
import com.evcel.instrument.Instrument

trait PivotRow {
  def market: String

  def period: Option[String] // should be a type so we can sort sensibly

  def value(field: PivotField): Any
}

trait PivotReport {
  def rows(instr: Instrument): Iterable[PivotRow]
}

trait PivotReportType {
  def create(env: Environment): PivotReport

  def fields: List[PivotField]
}

trait PivotField {
  def name: String
}
