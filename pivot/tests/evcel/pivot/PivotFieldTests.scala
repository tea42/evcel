package evcel.pivot

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import scala.util.Random
import evcel.quantity.Qty
import evcel.quantity.UOM

class PivotFieldTests extends FunSpec with Matchers{
  private def exceptionValue(msg : String) = ExceptionPivotValue(new RuntimeException(msg))
  private val stringField = StringPivotField("fred")

  describe("General sorting"){

    it("Should leave an empty collection alone"){
      stringField.sort(Vector()) should be ('empty)
    }

    it("Should leave singleton collections alone"){
      Vector(
        NullPivotValue,
        ExceptionPivotValue(new RuntimeException("bang!")),
        stringField.pivotValue("mike")
      ).foreach {
        v => 
          stringField.sort(Vector(v)) shouldEqual (Vector(v))
      }
    }

    it("Should put errors first, null values last and good values inbetween"){
      val goodValue = stringField.pivotValue("mike")
      val mergedGoodValues = stringField.merge(
        Vector(
          stringField.pivotValue("a"),
          stringField.pivotValue("b")
        )
      )

      val ex1 = exceptionValue("1")
      val ex2 = exceptionValue("2")
      val values = Vector(
        NullPivotValue, 
        goodValue,
        ex2, 
        mergedGoodValues,
        NullPivotValue,
        ex1
      )
        
      val actual = stringField.sort(values)
      val expected = Vector(
        ex1, 
        ex2,
        goodValue,
        mergedGoodValues,
        NullPivotValue,
        NullPivotValue
      )
      actual shouldEqual (expected)
        
    }

    it("Should merge good values by content size, lexicographically when sizes match"){
      def pivotValue(strs : String*) = stringField.merge(strs.toVector.map(stringField.pivotValue(_)))
      val v1 = pivotValue("a")
      val v2_a = pivotValue("b", "c")
      val v2_b = pivotValue("b", "d")
      val v3 = pivotValue("a", "b", "c")
      stringField.sort(Vector(v3, v2_a, v2_b, v1)) shouldEqual Vector(v1, v2_a, v2_b, v3)
    }

  }

  describe("General merging"){
    it("Should return NullPivotValue for an empty collection"){
      stringField.merge(Vector()) shouldEqual (NullPivotValue)
    }

    it("Should return the head of any singleton collection"){

      Vector(
        NullPivotValue,
        ExceptionPivotValue(new RuntimeException("bang!")),
        stringField.pivotValue("mike")
      ).foreach {
        v => 
          stringField.merge(Vector(v)) shouldEqual (v)
      }
    }

    it("Should return the first exception value found - if any"){
      val ex1 = exceptionValue("1")
      val ex2 = exceptionValue("2")
      val mergedValue = stringField.merge(
        Vector(
          NullPivotValue,
          ex2, 
          ex1,
          stringField.pivotValue("mike")
        )
      )
      mergedValue shouldEqual (ex2)
    }
  }

  describe("DistinctMerge fields"){
    it("should merge with distinct union"){
      val Vector(a, b, c) = Vector("a", "b", "c").map(stringField.pivotValue(_))
      val mergedValue = stringField.merge(Vector(a, c, b, NullPivotValue))
      mergedValue.content should contain allOf("a", "c", "b")
      mergedValue.content.size should be (3)
    }
  }

  describe("StringPivotField"){
    val Vector(a, b, c) = Vector("a", "b", "c").map(stringField.pivotValue(_))
    it("should sort alphabetically"){
      stringField.sort(Vector(c, a, b)) shouldEqual (Vector(a, b, c))
    }
  }

  describe("SummingQtyField"){
    val field = SummingQtyField("volume")
    it("Should sort by uom name"){
      val Vector(usd, gbp, eur, mt) = Vector(1(USD), 2(GBP), -5(EUR), 8(MT)).map(field.pivotValue(_))

      val sortedValues = field.sort(Vector(usd, gbp, eur, mt)) shouldEqual (Vector(eur, gbp, mt, usd))
    }
    it("Should merge by netting UOMs"){
      val qs = Vector(
        1(USD), 9(USD),
        2(GBP), 8(GBP), 
        -5(EUR), 
        8(MT), -4(MT)
      ).map(field.pivotValue(_))
      val mergedValues = field.merge((new Random(12345)).shuffle(qs))
      mergedValues.content should contain allOf(10(USD), 10(GBP), -5(EUR), 4(MT))
      mergedValues.content.size should be (4)
    }
  }

  describe("OptionInt field"){
    it("Should sort putting Somes before nones"){
      val field = OptionIntField("fred")
      val Vector(some1, some2, some3, some4) = Vector(1, 2, 3, 4).map{i => field.pivotValue(Some(i))}
      val none = field.pivotValue(None)
      val sorted = field.sort(Vector(
        none, 
        some3, 
        none,
        none,
        some4,
        some2,
        none,
        some1
      ))
      val expected = Vector(some1, some2, some3, some4, none, none, none, none)
      sorted shouldEqual(expected)
    }
  }

  describe("IdQtyField"){
    val field = IdSummingQtyField("fred")
    it("Should sort by quantity"){
      val vec@Vector(usd1, gbp1, eur3, eur1) = Vector(1(USD), 1(GBP), 3(EUR), 1(EUR)).map{q => field.pivotValue(q)}
      field.sort(vec) shouldEqual(Vector(eur1, eur3, gbp1, usd1))
    }
    it("Formatting should net across ids"){
      def pivotValue(id : Long, q : Qty) = field.pivotValue((id, q))
      val mergedAcrossIDs = field.merge(Vector(
        pivotValue(1, 1(USD)), 
        pivotValue(2, 10(USD))
      ))

      mergedAcrossIDs.toString shouldEqual(11(USD).toString)
    }
  }
}
