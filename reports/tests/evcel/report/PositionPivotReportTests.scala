package evcel.report

import org.scalatest.FunSpec
import org.scalatest.Matchers
import evcel.daterange.DateRangeSugar._
import evcel.curve.environment.MarketDay._
import evcel.instrument.CommoditySwap
import evcel.quantity.Qty
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.instrument.valuation.DefaultValuer
import evcel.curve.UnitTestingEnvironment
import evcel.curve.marketdata.FuturesPriceData
import scala.language.reflectiveCalls
import evcel.quantity.utils.QuantityTestUtils._
import evcel.daterange.Day
import evcel.instrument.valuation.Index

class PositionPivotReportTests extends FunSpec with Matchers{
  val valuer = new DefaultValuer()
  val market = "Nymex WTI"
  val index = "Nymex WTI nearby 1"
  val marketDay = (1 / Jun / 2014).endOfDay
  val F = Qty("100", USD / BBL)
  val K = Qty("101", USD / BBL)
  val swap = new CommoditySwap(
    index, Oct / 2014, K, Qty("123", BBL)
  )
  val valuationContext = UnitTestingEnvironment.fromMarketData(
    marketDay,
    market -> FuturesPriceData(Nov / 2014 -> F, Dec / 2014 -> F)
  )
  describe("PositionPivotReport"){
    it("Should by default return a swap as its own position"){
      val report = new PositionPivotReport(valuationContext, valuer)
      report.rows(swap) match {
        case List(PositionRow(mkt, period, position)) => 
          mkt should be (index)
          period should be (Some(swap.averagingPeriod.toString))
          position should be (swap.volume +- 1e-9)
        case other => 
          throw new RuntimeException("Unexpected position")
      }
    }

    it("Should return equal volumes when run daily"){
      val vc = valuationContext.copy(params = valuationContext.params.copy(tenor = Some(Day)))
      val report = new PositionPivotReport(vc, valuer)
      val rows = report.rows(swap)
      rows.forall(_.market == index) should be (true)
      val index_ = Index.parse(index)(vc.refData).get
      index_.observationDays(vc, swap.averagingPeriod)
      rows.map(_.period.get).toSet should 
        equal (index_.observationDays(vc, swap.averagingPeriod).map(_.toString).toSet)
      val positions : List[Qty] = rows.toList.map{
        case PositionRow(_, _, position) => position
      }
      Qty.sum(positions) should be (swap.volume +- 1e-9)
      positions.foreach{
        pos => 
          pos should be (positions.head +- 1e-9)
      }
    }

    // This will fail until fixings are added
    ignore("Should return an empty list for an expired instrument"){
      val expiredSwap = new CommoditySwap(
        index, Oct / 2010, K, Qty("123", BBL)
      )
      val report = new PositionPivotReport(valuationContext, valuer)
      report.rows(expiredSwap) should be ('empty)
    }
  }
}
