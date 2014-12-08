package evcel.report

import evcel.pivot.PivotField
import evcel.pivot.PivotValue
import scala.runtime.ScalaRunTime
import evcel.pivot.PivotRow
import evcel.pivot.PivotTable
import evcel.pivot.PivotTable
import scalaz.syntax.std.boolean._

/**
  * Contains the pivot values corresponding to a ColumnFieldsTree
  */
trait ColumnValuesTree{
  def field : PivotField
  def value : Option[PivotValue] // None for measure cells
  def label : String = value.map(_.toString).getOrElse(field.name)
  def children : Seq[ColumnValuesTree]
  def leaves : Seq[ColumnValuesLeaf]
  def isLeaf = children.isEmpty
  def depth : Int = 1 + (if (isLeaf) 0 else children.map(_.depth).max)
}

/**
  * Using a special class for the leaf nodes as only these
  * require knowledge of the entire path of values from 
  * the root, and any measure field contained in that path. 
  * These are used when calculating the measure values in a column
  * of the pivot report
  */
case class ColumnValuesLeaf(
  field : PivotField,
  value : Option[PivotValue],
  maybeMeasureField : Option[PivotField],
  path : Seq[PivotValue]
) 
  extends ColumnValuesTree 
{
  val children : Seq[ColumnValuesTree] = Vector()
  def leaves = Vector(this)
}
case class ColumnValuesNonLeaf(
  field : PivotField,
  value : Option[PivotValue],
  children : Seq[ColumnValuesTree]
)
  extends ColumnValuesTree
{
  def leaves = children.flatMap(_.leaves)
  override lazy val hashCode = ScalaRunTime._hashCode(this)
}


object ColumnValuesTree{

  def leaves(cells : Seq[ColumnValuesTree]) : Seq[ColumnValuesLeaf] = 
    cells.foldLeft(Seq[ColumnValuesLeaf]()){
      case (leaves, cell) => leaves ++ cell.leaves 
    }

  def build(
    table : PivotTable, 
    fieldTree : ColumnFieldsTree, 
    maybeMeasureField : Option[PivotField] = None,
    path : Seq[PivotValue] = Vector()
  ) : Seq[ColumnValuesTree] = {

    import fieldTree.{node => field, isMeasureNode => isMeasure, isLeaf}

    val valuesAndSubTables : Seq[(Option[PivotValue], PivotTable)] = if (fieldTree.isMeasureNode)
      Vector((None, table))
    else 
      PivotTable.groupByFieldValue(table, fieldTree.node).map{
        case (pivotValue, subTable) => (Some(pivotValue), subTable)
      }

    valuesAndSubTables.map{
      case (maybePivotValue, subTable) => 
        val childPath = maybePivotValue match {
          case Some(value) => path :+ value
          case None => path
        }
        if (isLeaf)
          ColumnValuesLeaf(
            field,
            maybePivotValue,
            maybeMeasureField orElse isMeasure.option(field),
            childPath
          )
        else
          ColumnValuesNonLeaf(
            field,
            maybePivotValue,
            fieldTree.children.flatMap{
              childTree => 
                build(
                  subTable, 
                  childTree, 
                  maybeMeasureField orElse isMeasure.option(field), 
                  childPath
                )
            }
          )
    }
  }

}

