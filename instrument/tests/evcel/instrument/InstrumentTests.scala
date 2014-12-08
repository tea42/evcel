package evcel.instrument

import org.scalatest.FunSpec
import org.scalatest.Matchers

class InstrumentTests extends FunSpec with Matchers{
  describe("Instruments"){
    it("Should have non empty samples for each type"){
      InstrumentType.types.foreach{
        instType => 
          require(instType.samples.nonEmpty, s"Missing samples for instrument $instType")
      }
    }
  }
}
