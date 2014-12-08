package evcel.pivot

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import scala.language.reflectiveCalls
import evcel.quantity.Qty._
import evcel.quantity.UOM._

class PivotRowTests extends FunSpec with Matchers{

  describe("PivotRow builder"){
    it("Should work when empty "){
      val row = PivotRow(Vector())(Vector())
      row.fields should be ('empty)
      intercept[RuntimeException]{
        row.pivotValue(StringPivotField("foo")) 
      }
    }

    it("Should work when non empty"){
      val field1 = StringPivotField("1")
      val field2 = DayField("Day")
      val value1 = field1.pivotValue("Foo")
      val value2 = field2.pivotValue(10 / May / 2014)
      val row = PivotRow(Vector(field1, field2))(
        Vector(field1.pivotValue("Foo"), field2.pivotValue(10 / May / 2014))
      ) 
      row.fields.toSet shouldEqual Set(field1, field2)
      row.pivotValue(field1) shouldEqual value1
      row.pivotValue(field2) shouldEqual value2
    }
  }

  describe("PivotRow sorting"){
    it("Should work on an empty sequence"){
      PivotRow.sort(Vector()) should be ('empty)
    }
    it("Should work on a singleton sequence"){
      val field = StringPivotField("Foo value")
      val value = field.pivotValue("Foo")
      
      val row = PivotRow(field, value)
      PivotRow.sort(Vector(row)) shouldEqual (Vector(row))
    }

    it("Should work on a single column rows"){
      val field = StringPivotField("Foo")
      val value1 = field.pivotValue("1")
      val value2 = field.pivotValue("2")
      
      val row1 = PivotRow(field, value1)
      val row2 = PivotRow(field, value2)
      PivotRow.sort(Vector(row1)) shouldEqual (Vector(row1))
      PivotRow.sort(Vector(row1, row2)) shouldEqual (Vector(row1, row2))
      PivotRow.sort(Vector(row2, row1)) shouldEqual (Vector(row1, row2))
    }

    it("Should work on multi column rows"){
      val fields = Vector(
        StringPivotField("Fred"), DayField("Day"), SummingQtyField("Value")
      )
      val unsortedRows = PivotTableTests.testTable(
        fields,
        "a", 12 / May / 2014, 10(USD),
        "a", 12 / May / 2014, 9(USD),
        "a", 23 / May / 2014, 4(USD),
        "b", 13 / May / 2014, 10(USD),
        "b", 13 / May / 2014, 41(USD),
        "a", 13 / May / 2014, 10(USD)
      ).pivotRows.toVector
      val expectedSortedRows = PivotTableTests.testTable(
        fields,
        "a", 12 / May / 2014, 9(USD),
        "a", 12 / May / 2014, 10(USD),
        "a", 13 / May / 2014, 10(USD),
        "a", 23 / May / 2014, 4(USD),
        "b", 13 / May / 2014, 10(USD),
        "b", 13 / May / 2014, 41(USD)
      ).pivotRows.toVector
      val actualSortedRows = PivotRow.sort(unsortedRows)
      actualSortedRows shouldEqual expectedSortedRows
    }
  }
}
object PivotRowTests{
  def testRow(fields : Seq[PivotField], values : Any*) : PivotRow = {
    require(values.size == fields.size)
    val pivotValues = fields.zip(values).map{
      case (f, v) => f.pivotValue(v.asInstanceOf[f.GoodType])
    }
    PivotRow(fields)(pivotValues)
  }
}

