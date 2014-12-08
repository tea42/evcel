package evcel.report

import evcel.pivot._
import scala.collection.breakOut
import scala.collection.mutable
import scala.collection.immutable.VectorBuilder

/**
  * The measure area of a pivot report, indexed by row values
  * and the leaf values of the column area
  */
case class MeasureValues(
  rows : Iterable[PivotRow],
  cols : Iterable[ColumnValuesLeaf],
  measures : Map[(PivotRow, ColumnValuesLeaf), PivotValue] 
){
  def valuesByRow : Seq[Seq[PivotValue]] = {
    rows.map{
      row => 
        cols.map{
          col => 
            measures((row, col))
        }(breakOut)
    }(breakOut)
  }
}

object MeasureValues{
  def build(
    rowTable: PivotTable,
    columnTrees : Seq[(ColumnFieldsTree, Seq[ColumnValuesTree])],
    tables : Seq[PivotTable]
  ) : MeasureValues = {
    val map = mutable.Map[(PivotRow, ColumnValuesLeaf), PivotValue]()
    val pivotRows = rowTable.pivotRows
    val colsBuilder = new VectorBuilder[ColumnValuesLeaf]()

    columnTrees.zip(tables).foreach{
      case ((columnFieldsTree, columnValuesTrees), table) => 
        val byRows = PivotTable.groupByFieldValues(table, rowTable.fields)
        val columnValueLeaves = ColumnValuesTree.leaves(columnValuesTrees)
        val columnFields = columnFieldsTree.nonMeasureFields
        colsBuilder ++= columnValueLeaves
        rowTable.pivotRows.foreach{
          row => 
            byRows.get(row.pivotValues) match {
              case None => 
                columnValueLeaves.foreach{
                  columnValues => 
                    map += ((row, columnValues) -> NullPivotValue)
                }
              case Some(subTable) => 
                val byCols = PivotTable.groupByFieldValues(subTable, columnFields)
                columnValueLeaves.foreach{
                  col =>
                    val measure = byCols.get(col.path).map{
                      subTable => 
                        col.maybeMeasureField.map(
                          _.mergedValue(subTable)
                        ).getOrElse(NullPivotValue)
                    }.getOrElse(NullPivotValue)
                    map += ((row, col) -> measure)
                }
            }
        }
    }
    MeasureValues(pivotRows, colsBuilder.result, map.toMap)
  }
}

