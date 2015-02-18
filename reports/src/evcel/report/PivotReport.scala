package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Future
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

trait PivotReportBuilder {
  def fields:Seq[PivotField]
  def createTable(fields:Seq[PivotField], filters:Map[PivotField, PivotValue => Boolean]):PivotTable

  def build(layout : PivotReportLayout):PivotReport = {
    val tablesByColumnFieldTree : Seq[PivotTable] = layout.columnFieldTrees.map {
      columnFieldTree => createTable(layout.rowFields ++ columnFieldTree.fields, layout.filters)
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
  def fieldFor(name:String) = fields.find(_.name == name).get
}

class TradePivotReportBuilder(trades : Seq[Trade],vc : Option[ValuationContext],valuer : Option[Valuer])
    extends PivotReportBuilder {

  def createTable(fields:Seq[PivotField], filters:Map[PivotField, PivotValue => Boolean]) = {
    ValuationTableBuilder(fields, filters, vc, valuer).build(trades)
  }

  def fields: Seq[PivotField] = {
    PivotTrade.STANDARD_FIELDS :::
      PivotInstrument.instrumentTypeFields(Future).toList :::
      PivotValuer.FIELDS.toList
  }
}
