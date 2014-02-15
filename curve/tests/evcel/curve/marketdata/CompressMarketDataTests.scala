package evcel.curve.marketdata

import org.scalatest.{Matchers, FreeSpec}

class CompressMarketDataTests extends FreeSpec with Matchers with CompressMarketData{

  import CompressMarketData.SEPARATOR

  "An empty Seq should compress to an empty Array" in {
    Vector[BigDecimal]().compressed should equal (Array[String]())
  }

  "A simple big decimal should split at the point" in {
    Vector(BigDecimal("1.23")).compressed should equal (Array("1", "23", SEPARATOR))
  }

  "BigDecimals without decimals in their representation should not spilt" in {
    Vector(BigDecimal("123")).compressed should equal (Array("123", SEPARATOR))
    Vector(BigDecimal("1E-9")).compressed should equal (Array("1E-9", SEPARATOR))
  }

  "Same sequence should be returned after round trip" in {
    def roundTrip(numbers : String*){
      val seq = numbers.map(BigDecimal(_)).toVector
      seq.compressed.uncompressed should equal (seq)
    }

    roundTrip("1.23", "0.5", "12")
    roundTrip("1E12", "0.000000000001")

    roundTrip(
      "12.34",
      ".34",
      "1",
      "10.",
      "0.239842893472894728789",
      "290948290482.2093809284902842",
      "1e10",
      "1.234E90",
      "1.23e-9"
    )
  }

}
