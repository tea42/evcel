package com.evcel.report

import com.evcel.Environment.Environment
import com.evcel.core.qty.Qty
import com.evcel.core.qty.UOM._
import com.evcel.date.Month
import com.evcel.instrument.{ EuropeanOption, FuturesOption }
import com.evcel.models.Call
import org.scalatest._

class MtmPivotReportTest extends FunSuite with ShouldMatchers {

  test("not much yet") {
    val option = new FuturesOption(
      "market", Month(2010, 1), Qty("100", USD / BBL), Qty("1000", BBL), Call, EuropeanOption, false, 1
    )
    val pr = new MtmPivotReport(new Environment)
    val rows = pr.rows(option)
    rows.size shouldBe 1
    rows.head.value(MtmPivotReportType.MtmField) shouldEqual Qty(1000.0, USD)
  }
}
