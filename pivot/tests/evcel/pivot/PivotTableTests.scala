package evcel.pivot

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult
import evcel.daterange.DateRangeSugar._
import scala.language.reflectiveCalls

class PivotTableTests extends FunSpec with Matchers{
  import PivotTableTests.{testTable, sameAs}

  describe("PivotTable.addSingleRow"){
    it("Adding a row to as empty table should make a table with one row"){
      val field = StringPivotField("counterparty")
      val row = PivotRow(field, field.pivotValue("ACME"))
      val table = PivotTable.addSingleRow(row, PivotTable.Null)
      table should be (sameAs(row.asPivotTable))
    }

    it("Adding to a non empty table should work too"){
      val tableFields = Vector(StringPivotField("Market"), DayField("Day"))
      val table = testTable(
        tableFields,
        "wti", 10 / Jun / 2014,
        "brent", 15 / May / 2010
      )
      val rowFields = Vector(StringPivotField("Cpty"), PriceQtyField("Price"))
      val row = PivotRowTests.testRow(rowFields, "ACME", 10(USD/MT))
      val actual = PivotTable.addSingleRow(row, table)
      val expected = testTable(
        rowFields ++ tableFields,
        "ACME", 10(USD/MT), "wti", 10 / Jun / 2014,
        "ACME", 10(USD/MT), "brent", 15 / May / 2010
      )
      actual should be (sameAs(expected))

    }
  }

  describe("PivotTable.append"){

    describe("Appending an empty vector should return a null table"){
      PivotTable.append(Vector()) should be (sameAs(PivotTable.Null))
    }

    describe("Appending a single table should return an identical table"){
      it("For a null table"){
        PivotTable.append(Vector(PivotTable.Null)) should be (sameAs(PivotTable.Null))
      }
      it("For a non empty table"){
        val fields = Vector(StringPivotField("foo"), StringPivotField("bar"))
        val table = testTable(
          fields,
          "fred", "mike",
          "jack", "jill"
        )
        PivotTable.append(Vector(table)) should be (sameAs(table))
      }
    }

    it("Appending more than one table should concatenate them"){
        val fields = Vector(StringPivotField("market"), DayField("day"))
        val table1 = testTable(
          fields,
          "wti", 10 / Jun / 2014,
          "brent", 11 / Jan / 2013
        )
        val table2 = testTable(
          fields,
          "gasoil", 20 / Dec / 2014,
          "brent", 21 / Sep / 2013
        )
        val expexted = testTable(
          fields,
          "wti", 10 / Jun / 2014,
          "brent", 11 / Jan / 2013,
          "gasoil", 20 / Dec / 2014,
          "brent", 21 / Sep / 2013
        )

        PivotTable.append(Vector(table1, table2)) should be (sameAs(expexted))

      
    }
  }

}

object PivotTableTests{
  def sameAs(rhs : PivotTable) = new BeMatcher[PivotTable]{
    def apply(lhs : PivotTable) : MatchResult = {
      val matches = lhs.fields == rhs.fields &&
                    lhs.pivotRows.toVector == rhs.pivotRows.toVector
      MatchResult(
        matches,
        s"Tables don't match",
        s"Tables match"
      )
    }
  }

  def testTable(
    fields : Seq[PivotField],
    values : Any*
  ) : PivotTable = {
    val rowBuilder = PivotRow(fields)
    val pivotRows = values.grouped(fields.size).toVector.map{
      row =>
        val values = fields.zip(row).map{
          case (f, a) => f.pivotValue(a.asInstanceOf[f.GoodType])
        }
        rowBuilder(values)
    }
    PivotTable(fields, pivotRows)
  }
}
