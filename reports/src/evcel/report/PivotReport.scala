package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.trade.Trade
import evcel.valuation.Valuer
import evcel.pivot._
import scalaz.syntax.std.boolean._
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.collection.breakOut
import scala.runtime.ScalaRunTime

case class PivotReport(
  rowTable : PivotTable,
  columnValueTrees : Seq[ColumnValuesTree],
  measureValues : MeasureValues
)

object PivotReport{

  def build(
    trades : Seq[Trade], 
    vc : Option[ValuationContext],
    valuer : Option[Valuer],
    layout : PivotReportLayout
  ) : PivotReport = {
          
    val tablesByColumnFieldTree : Seq[PivotTable] = layout.columnFieldTrees.map{
      columnFieldTree => 
        ValuationTableBuilder(
          layout.rowFields ++ columnFieldTree.fields,
          layout.filters,
          vc,
          valuer
        ).build(trades)
    }

    val rowTable = {
      val fields = layout.rowFields
      val rows : Seq[PivotRow] = PivotRow.sort(
        tablesByColumnFieldTree.flatMap(_.buildRows(fields)).distinct
      )
      PivotTable(fields, rows)
    }

    val columnTrees : Seq[(ColumnFieldsTree, Seq[ColumnValuesTree])] = 
      layout.columnFieldTrees.zip(tablesByColumnFieldTree).map{
        case (columnFieldTree, columnTable) => 
          (columnFieldTree, ColumnValuesTree.build(columnTable, columnFieldTree))
      }
    val measures : MeasureValues = MeasureValues.build(
      rowTable,
      columnTrees, 
      tablesByColumnFieldTree
    )
    PivotReport(rowTable, columnTrees.map(_._2).flatten, measures)
  }
}
