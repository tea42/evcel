package evcel.web

import evcel.pivot.{PivotField, PivotRow}
import evcel.report._
import evcel.webshared.{WebField, Cell}

import scala.collection.breakOut

/**
 *
 */

object PivotData {
  def pivotData(pivot: PivotReportBuilder, filterFields:Seq[PivotField], layout: PivotReportLayout) = {
    val usedFields = (layout.fields ++ filterFields).map(_.name)
    val spares = pivot.fields.filterNot(f => usedFields.contains(f.name)).map(f => WebField(f.name, f.isMeasure))
    PivotData(spares, filterFields.map(_.name), layout, pivot.build(layout))
  }
  def createLayout(pivot: PivotReportBuilder, filters:List[String], rows:List[String], columns:List[String], measures:List[String]) = {
    def fields(names:List[String]) = names.map(pivot.fieldFor)
    val filterFields = fields(filters)
    val rowFields = fields(rows)
    val columnFields = fields(columns)
    val measureFields = fields(measures)
    (filterFields, PivotReportLayout(rowFields, ColumnFieldsTree.simple(columnFields, measureFields), Map()))
  }

}

case class PivotData(fieldArea:Seq[WebField], filters:Seq[String], layout:PivotReportLayout, report:PivotReport) {


  lazy val rowFields = report.rowTable.fields.map(_.name)
  lazy val columnArea = cellsFor(report.columnValueTrees)
  lazy val measuresWidth = columnArea.headOption.map(_.map(_.width).sum).getOrElse(1)
  def rowsWidth = rows.length
  def totalWidth = rowsWidth + measuresWidth

  def spares = fieldArea
  def rows = layout.rowFields.map(_.name)
  def columns = columnsFor(layout.columnFieldTrees, Nil)
  def table = {
    val noRows = rowFields.isEmpty
    val header = columnArea match {
      case Nil => List( (if (noRows) Seq(Cell("x")) else rowFields.map(name=>Cell(name))) ++  Seq(Cell("bc")))
      case top :: rest =>
        val columnHeight = math.max(1, columnArea.map(_.headOption.map(_.height).getOrElse(0)).sum)
        ((if (noRows) Seq(Cell("br", true, 1, columnHeight)) else rowFields.map(name=>Cell(name, true, 1, columnHeight))) ++ top) :: rest
    }
    val rowAreaAndData = {
      val cols = report.measureValues.cols
      val measures = report.measureValues.measures
      val rows:Iterable[PivotRow] = if (noRows) List(PivotRow(Nil)(Nil)) else report.measureValues.rows
      rows.map{
        row =>
          val rowCells = if (noRows) blankRow else row.pivotValues.map { value => Cell(value.toString) }(breakOut)
          val measureCells = cols.map { col => Cell(measures((row, col)).toString(), isHeading=false) }(breakOut)
          rowCells ++ measureCells
      }(breakOut)
    }
    header ++ rowAreaAndData
  }

  val blankRow = Seq(Cell("Q"))

  private def columnsFor(columnFieldTrees:Seq[ColumnFieldsTree], soFar:List[String]):List[String] = {
    columnFieldTrees match {
      case Seq(ColumnFieldsTree(field, false, children)) => columnsFor(children, field.name :: soFar)
      case _ => soFar.reverse
    }
  }
  def measures = measuresFor(layout.columnFieldTrees)
  private def measuresFor(columnFieldTrees:Seq[ColumnFieldsTree]):Seq[String] = {
    columnFieldTrees match {
      case Seq(ColumnFieldsTree(field, false, children)) => measuresFor(children)
      case x if x.forall(_.isMeasureNode) => x.map(_.node.name)
      case _ => Nil
    }
  }

  def cellsFor(columnValueTrees : Seq[ColumnValuesTree]):List[List[Cell]] = {
    if (columnValueTrees.isEmpty) {
      Nil
    } else {
      val grids = columnValueTrees.map(cellsFor)
      val height = grids.map(_.length).max
      (0 until height).toList.map { i =>
        grids.map(_(i)).flatten.toList
      }
    }
  }
  def cellsFor(columnValueTrees : ColumnValuesTree):List[List[Cell]] = {
    val name = columnValueTrees.label
    val childGrid = cellsFor(columnValueTrees.children)
    childGrid.headOption match {
      case None => List( List(Cell(name)) )
      case Some(topRow) =>
        val width = topRow.map(_.width).sum
        List( Cell(name, true, width, 1)) :: childGrid
    }
  }
}