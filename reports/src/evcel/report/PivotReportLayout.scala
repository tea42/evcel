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



