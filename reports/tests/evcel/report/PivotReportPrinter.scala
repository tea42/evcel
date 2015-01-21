package evcel.report

import scala.collection.breakOut
import evcel.pivot.PivotValue
import evcel.utils.StringUtils
import scala.collection.immutable.VectorBuilder
import evcel.pivot.PivotRow
import evcel.pivot.PivotField
import scala.collection.mutable

case class PivotReportPrinter(report : PivotReport){
  private val COL_SEP = " | "
  private val AREA_SEP = " || "
  import report.{rowTable, columnValueTrees, measureValues}

  lazy val minimumMeasureWidths : Map[ColumnValuesLeaf, Int] = {
    import measureValues.{rows, cols, measures}
    cols.map{
      col => 
        col -> 
          rows.map{
            row : PivotRow => 
              measures((row, col)).toString.size
          }.max
    }(breakOut)
  }

  private def printWidths(
    trees : Seq[ColumnValuesTree],
    measureMinimumPrintWidths : Map[ColumnValuesLeaf, Int],
    fieldSeparatorWidth : Int
  ) : Map[ColumnValuesTree, Int] = {

    val minimumPrintWidths : Map[ColumnValuesTree, Int] = {
      val accumulator = mutable.Map[ColumnValuesTree, Int]()
      def collectMinimumPrintWidths(
        tree : ColumnValuesTree
      ){
        import tree.{isLeaf, children, label}
        children.foreach(collectMinimumPrintWidths)

        val treeNodeMinWidth = if (isLeaf)
          label.size max measureMinimumPrintWidths(tree.asInstanceOf[ColumnValuesLeaf])
        else {
          val sumOfChildPrintWidths = children.map(accumulator).sum +
            (children.size - 1) * fieldSeparatorWidth
          label.size max sumOfChildPrintWidths 
        }
        accumulator += (tree -> treeNodeMinWidth)
      }
      trees.foreach(collectMinimumPrintWidths)
      accumulator.toMap
    }

    val actualWidthsAccumulator = mutable.Map[ColumnValuesTree, Int]()

    def collectActualPrintWidths(
      tree : ColumnValuesTree, nodePrintWidth : Int
    ){
      import tree.{isLeaf, children}
      actualWidthsAccumulator += tree -> nodePrintWidth

      if (! isLeaf){

        val sumOfMinimumChildWidths = children.map(minimumPrintWidths).sum + 
          (children.size - 1) * fieldSeparatorWidth
        val childPadding = nodePrintWidth - sumOfMinimumChildWidths
        val padPerChild = childPadding / children.size
        val N_childrenWithOneExtraSpace = childPadding - padPerChild * children.size
        children.zipWithIndex.foreach{
          case (child, i) => 
            val childWidth = 
              minimumPrintWidths(child) + 
              padPerChild + 
              (if (i < N_childrenWithOneExtraSpace) 1 else 0)
            collectActualPrintWidths(child, childWidth)
        }
      }
    }

    trees.foreach{tree => collectActualPrintWidths(tree, minimumPrintWidths(tree))}
    actualWidthsAccumulator.toMap
  }
  private lazy val columnWidths : Map[ColumnValuesTree, Int] = printWidths(
    columnValueTrees,
    minimumMeasureWidths,
    fieldSeparatorWidth = COL_SEP.size
  )

  private lazy val measureArea : Seq[String] = {
    measureValues.valuesByRow.map{
      row : Seq[PivotValue] => 
        row.zip(measureValues.cols).map{
          case (value, col) => 
            StringUtils.rightJustify(value.toString, columnWidths(col))
        }.mkString(COL_SEP)
    }
  }

  private lazy val columnArea : Seq[String] = {
    if (columnValueTrees.isEmpty)
      Vector()
    else {
      val builders = Vector.fill(columnValueTrees.map(_.depth).max)(new VectorBuilder[String]())
      def build(cell : ColumnValuesTree, bldrs : Seq[VectorBuilder[String]]){
        val justifiedLabel = StringUtils.centreFill(cell.label, columnWidths(cell)) 
        bldrs.head += justifiedLabel
        cell.children.foreach(build(_, builders.tail))
        if (cell.isLeaf){
          val blank = " " * justifiedLabel.size
          bldrs.tail.foreach(_ += blank)
        }
      }
      columnValueTrees.foreach(build(_, builders))
      builders.map(_.result.mkString(COL_SEP))
    }
  }


  lazy val rowWidths : Map[PivotField, Int] = rowTable.fields.map{
    field => 
      val width = field.name.size max rowTable.pivotValues(field).map(_.toString.size).max
      field -> width
  }(breakOut)
  private lazy val rowAreaHeadings : String = rowTable.fields.map{
    field => 
      StringUtils.leftJustify(field.name, rowWidths(field))
  }.mkString(COL_SEP)

  private lazy val rowArea : Seq[String] = {
    var lastRow : Option[PivotRow] = None
    rowTable.pivotRows.map{
      row => 
        val (repeat, nonRepeat) = rowTable.fields.span{
          field => 
            lastRow.map(_.pivotValue(field)) == Some(row.pivotValue(field))
        }

        lastRow = Some(row)
        val seq = repeat.map(" " * rowWidths(_)) ++ 
          nonRepeat.map{
            field => 
              StringUtils.rightJustify(row.pivotValue(field).toString, rowWidths(field))
          }
        seq.mkString(COL_SEP)
    }(breakOut)
  }

  lazy val topArea : Seq[String] = {
    if (columnArea.isEmpty)
      Vector(rowAreaHeadings)
    else {
      columnArea.dropRight(1).map(" " * rowAreaHeadings.size + AREA_SEP + _) ++
        Vector(rowAreaHeadings + AREA_SEP + columnArea.last)
    }
  }
  lazy val bottomArea : Seq[String] = rowArea.zip(measureArea).map{
    case (rowText, measureText) => 
      List(rowText, measureText).mkString(AREA_SEP)
  }

  lazy val separator = 
    Vector("-" * rowAreaHeadings.size + "-||-" + measureArea.headOption.map("-" * _.size).getOrElse(""))

  def formatted : String = (topArea ++ separator ++ bottomArea).mkString("\n")
  def formattedAsCode : String = 
    "val expected = \n" + 
    formatted.split("\n").mkString("  \"\"\"|", "$\n     |", "$\"\"\".stripMargin.replace(\"$\", \"\")")

}
