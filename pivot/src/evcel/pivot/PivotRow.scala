package evcel.pivot
import evcel.utils.StringUtils

trait PivotRow {
  def fields : Seq[PivotField]
  def pivotValue(field : PivotField) : PivotValue
  def pivotValues = fields.map(pivotValue)

  def satisfies(predicates : Map[PivotField, PivotValue => Boolean]) = predicates.forall{
    case (field, predicate) => predicate(pivotValue(field))
  }
  def subRow(fields : Seq[PivotField]) : Seq[PivotValue] = fields.map(pivotValue)
  override def toString = {
    val fieldStrings = fields.map(_.name)
    val valueStrings = fields.map(pivotValue(_).toString)
    val widths = fieldStrings.zip(valueStrings).map{case (f, v) => f.size max v.size}
    val line1 = fieldStrings.zip(widths).map{case (f, w) => StringUtils.leftJustify(f, w)}.mkString("  ")
    val line2 = valueStrings.zip(widths).map{case (f, w) => StringUtils.leftJustify(f, w)}.mkString("  ")
    line1 + "\n" + line2
  }

  def asPivotTable : PivotTable = PivotTable(fields, Vector(this))

  override def equals(rhs_ : Any) = Option(rhs_) match {
    case Some(rhs : PivotRow) =>
      fields == rhs.fields && pivotValues == rhs.pivotValues
    case _ => false
  }
  override lazy val hashCode = fields.hashCode * 41 + pivotValues.hashCode

}

object PivotRow{
  /**
    * Normally PivotRows will live inside a PivotTable. This builder
    * avoids duplication of `fields`
    */
  def apply(fields_ : Seq[PivotField]) : Seq[PivotValue] => PivotRow = {
    val indexes = fields_.zipWithIndex.toMap
    values : Seq[PivotValue] => new PivotRow(){
      def fields = fields_
      def pivotValue(field : PivotField) = {
        val i_field = indexes.getOrElse(field, sys.error(s"Unrecognized field $field"))
        values(i_field)
      }
    }
  }
  def apply(field : PivotField, value : PivotValue) = new PivotRow(){
    def fields = Vector(field)
    def pivotValue(field_ : PivotField) = {
      require (field_ == field, s"Unexpected field $field_")
      value
    }
  }


  def sort(rows : Seq[PivotRow]) = {
    if (rows.isEmpty)
      rows
    else {
      val fields = rows.head.fields
      
      val ordering = new Ordering[PivotRow]{
        def compare(row1 : PivotRow, row2 : PivotRow) = {
          fields.view.map{
            f => 
              f.ordering.compare(row1.pivotValue(f), row2.pivotValue(f)) 
          }.find(_ != 0).getOrElse(0)
        }
      }
      rows.sorted(ordering)
    }
  }
}
