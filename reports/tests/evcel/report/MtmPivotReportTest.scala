package evcel.report

import evcel.curve.environment.{TimeOfDay, MarketDay}
import evcel.curve.{UnitTestingEnvironment, Environment}
import evcel.daterange.{Day, Month}
import evcel.instrument.{EuropeanOption, FuturesOption}
import evcel.maths.Call
import evcel.quantity.Qty
import evcel.quantity.UOM._
import org.scalatest.{ShouldMatchers, FunSuite}


class MtmPivotReportTest extends FunSuite with ShouldMatchers {

  test("not much yet") {
    val option = new FuturesOption(
      "market", Month(2010, 1), Qty("100", USD / BBL), Qty("1000", BBL), Call, EuropeanOption,
      isCashSettled = false, bizDaysAfterExpiryToSettlement = 1, customExpiry = None
    )
    val pr = new MtmPivotReport(UnitTestingEnvironment(MarketDay(Day(2009, 12, 1), TimeOfDay.end), {
      case _ => sys.error("not needed yet")
    }))
    val rows = pr.rows(option)
    rows.size shouldBe 1
    rows.head.value(MtmPivotReportType.MtmField) shouldEqual Qty(1000.0, USD)
  }
}
