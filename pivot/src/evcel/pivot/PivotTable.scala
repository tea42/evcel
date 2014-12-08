package evcel.pivot

import scala.collection.AbstractIterable
import scala.collection.mutable
import scala.collection.breakOut
import evcel.utils.StringUtils

trait PivotTable{
  def fields : Seq[PivotField]
  def pivotRows : Iterable[PivotRow]
  def buildRows(fields : Seq[PivotField]) : Iterable[PivotRow] = {
    val bldr = PivotRow(fields)
    pivotRows.map{row => bldr(row.subRow(fields))}
  }

  def pivotValues(field : PivotField) = pivotRows.map(_.pivotValue(field))
  def isEmpty = pivotRows.isEmpty
  override def toString = {
    val fieldsAsStrings = fields.map(_.name)
    val rowsAsStrings : Seq[Seq[String]] = pivotRows.map{
      pr => 
        fields.map(pr.pivotValue(_).toString)
    }.toVector
    val allRows : Seq[Seq[String]] = fieldsAsStrings +: rowsAsStrings
    val widths = allRows.foldLeft(Vector.fill(fields.size)(0)){
      case (ws, v2) => 
        ws.zip(v2.map(_.size)).map{case (w1, w2) => w1 max w2}
    }
    allRows.map{
      row => 
        row.zip(widths).map{
          case (s, w) => StringUtils.leftJustify(s, w)
        }.mkString("  ")
    }.mkString("\n")
  }
}

object PivotTable{

  def apply(fields_ : Seq[PivotField], pivotRows_ : Iterable[PivotRow]) = new PivotTable(){
    def fields = fields_
    def pivotRows = pivotRows_
  }

  def apply(fields_ : Seq[PivotField], pivotRows_ : Seq[Seq[PivotValue]]) = new PivotTable(){
    def fields = fields_
    def pivotRows = {
      val bldr = PivotRow(fields_)
      pivotRows_.map(bldr)
    }
  }

  case object Null extends PivotTable{
    val fields = Vector()
    val pivotRows = Vector()
  }

  def groupByFieldValues(table : PivotTable, fields : Seq[PivotField]) : Map[Seq[PivotValue], PivotTable] = {
    table.pivotRows.groupBy(_.subRow(fields)).map{
      case (subRowValues, subGroupRows) => 
        (subRowValues, PivotTable(fields, subGroupRows))
    }
  }

  
  /**
    * Splits the table by its value for the given field, returning the
    * results sorted by the field values natural order
    */
  def groupByFieldValue(table : PivotTable, field : PivotField) : Seq[(PivotValue, PivotTable)] = {
    val byField : Map[PivotValue, PivotTable] = groupByFieldValues(table, Vector(field)).map{
      case (Vector(fieldValue), subTable) => 
        (fieldValue, subTable)
    }(breakOut)
    val sortedValues = field.sort(byField.keys.toVector)
    sortedValues.map{
      value => 
        value -> byField(value)
    }
  }

  /**
    * Concatenates several tables into one by stacking 
    * columnwise.
    * Each of the tables therefore must have the same fields
    */
  def append(tables : Seq[PivotTable]) : PivotTable = {
    if (tables.isEmpty)
      Null
    else {
      val fields_ = tables.head.fields
      require(tables.forall(_.fields == fields_))
      PivotTable(
        fields_,
        new scala.collection.Iterable[PivotRow](){
          def iterator : Iterator[PivotRow] = 
            tables.map(_.pivotRows.iterator).reduceLeftOption(_++_).getOrElse(Iterator.empty)
        }
      )
    }
  }

  /**
    * Creates a new table by adding the same pivot row to each of its rows
    */
  def addSingleRow(pivotRow : PivotRow, table : PivotTable) : PivotTable = {
    val addedFields = pivotRow.fields.toSet
    require(
      addedFields.intersect(table.fields.toSet).isEmpty, 
      "Would imply multiple values per row for the same field"
    )
    if (table.isEmpty)
      pivotRow.asPivotTable
    else {
      new PivotTable(){
        lazy val fields_ = pivotRow.fields ++ table.fields
        def fields = fields_
        def pivotRows = table.pivotRows.map{
          tableRow => 
            new PivotRow(){
              def fields = fields_
              def pivotValue(field : PivotField) = if (addedFields.contains(field))
                pivotRow.pivotValue(field)
              else 
                tableRow.pivotValue(field)
            }
        }
      }
    }
  }

}
