package evcel.valuation

import evcel.quantity.Qty
import evcel.quantity.UOM._

class PositionTest extends ValuationTest with ValuationTestPimps{

  test("Test position of single swap"){
    val vc = createVC()
    val richIndex = RichIndex(vc.refData, createSwap().index, createSwap().level).R
    val observationDays = richIndex.observationDays(createSwap().averagingPeriod)
    val swap = createSwap(volume = Qty((observationDays.size * 13).toString, BBL))
    val positions = swap.positions(vc)
    val expectedHedgeInstrument = swap.copy(strike = Qty(0, USD/BBL), volume = Qty(1, BBL))
    positions.keySet shouldEqual Set(expectedHedgeInstrument)
    positions.head._2 shouldEqual Right(observationDays.size * 13)
  }
}
