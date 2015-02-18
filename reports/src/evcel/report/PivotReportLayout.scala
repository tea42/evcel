package evcel.report

import evcel.pivot.PivotField
import evcel.pivot.PivotValue
  
case class PivotReportLayout(
  rowFields : Seq[PivotField],
  columnFieldTrees : Seq[ColumnFieldsTree],
  filters : Map[PivotField, PivotValue => Boolean]
){
  def fields = (rowFields ++ columnFieldTrees.flatMap(_.fields)).distinct
}

case class ColumnFieldsTree(
  node : PivotField, 
  isMeasureNode : Boolean, 
  children : Seq[ColumnFieldsTree]
){

  val fields : Seq[PivotField] = 
    (children.flatMap(_.fields) :+ node).distinct

  lazy val nonMeasureFields : Seq[PivotField] = {
    val childFields = children.flatMap(_.nonMeasureFields)
    (if (isMeasureNode) childFields else childFields :+ node).distinct
  }


  def isLeaf = children.isEmpty

}

object ColumnFieldsTree {
  def simple(columns:List[PivotField], measures:List[PivotField]) = {
    var trees = measures.map(f => ColumnFieldsTree(f, true, Nil))
    columns.reverse.foreach { c => trees = ColumnFieldsTree(c, false, trees) :: Nil}
    trees
  }
}

