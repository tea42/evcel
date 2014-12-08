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
import evcel.curve.marketdata.MarketDataTest
import evcel.instrument.trade.Trade
import evcel.instrument.valuation.Valuer._
import evcel.quantity.utils.QuantityTestUtils._
import evcel.pivot.PivotField
import evcel.daterange.DateRangePeriodLabel
import evcel.pivot.PivotTable
import evcel.curve.ValuationContext
import evcel.pivot.PivotRow
import evcel.instrument.Tradeable
import evcel.daterange.Month
import evcel.quantity.Percent
import evcel.instrument.FuturesOption
import evcel.referencedata.TestFuturesExpiryRules
import evcel.curve.environment.MarketDay
import evcel.curve.marketdata.Act365
import evcel.maths.models.BlackScholes
import evcel.maths.Call
import evcel.maths.EuropeanOption
import evcel.curve.environment.TimeOfDay
import evcel.daterange.PeriodLabel
import evcel.instrument.Future
import evcel.daterange.DateRange
import evcel.quantity.utils.QuantityTestUtils
import evcel.report.PivotValuer._

class ValuationTableBuilderTests extends FunSpec with MarketDataTest with Matchers{
  val valuer = new DefaultValuer()
  val market = "Nymex WTI"
  val index = "Nymex WTI nearby 1"
  val marketDay = (1 / Jun / 2014).endOfDay
  val F = Qty("100", USD / BBL)
  val K = Qty("101", USD / BBL)
  val liveSwap = new CommoditySwap(
    index, Oct / 2014, K, Qty("123", BBL)
  )
  val expiredSwap = new CommoditySwap(
    index, Oct / 2010, K, Qty("123", BBL)
  )
  def makeTrade(tradeable : Tradeable) = Trade(
    "1234",
    1 / Jan / 2014,
    "ACME",
    tradeable
  )
  val valuationContext = UnitTestingEnvironment.fromMarketData(
    marketDay,
    market -> futuresPrices(Nov / 2014 -> F, Dec / 2014 -> F)
  )

  private def buildTable(
    tradeable : Tradeable, 
    fields : Seq[PivotField] = Vector(RiskMarketField, RiskPeriodField, PositionField),
    vc : ValuationContext = valuationContext) : PivotTable = {
    ValuationTableBuilder(
      fields,
      filters = Map.empty,
      Some(vc),
      Some(valuer)
    ).build(Vector(makeTrade(tradeable)))
  }

  private def singleValue[T](row : PivotRow, field : PivotField) = {
    val values = row.pivotValue(field).content
    if (values.size != 1)
      values.foreach(println)
    require(values.size == 1)
    values.head.asInstanceOf[T]
  }

  describe("Positions"){
    it("Should by default return a swap as its own position"){
      val table = buildTable(liveSwap)
      table.pivotRows.size should be (1)
      val row = table.pivotRows.head

      singleValue[String](row, RiskMarketField) should be (index)
      singleValue[PeriodLabel](row, RiskPeriodField) should be (DateRangePeriodLabel(liveSwap.averagingPeriod))
      singleValue[Qty](row, PositionField) should be (liveSwap.volume +-1e-9)
    }

    it("Should return equal volumes when run daily"){
      val vc = valuationContext.copy(params = valuationContext.params.copy(tenor = Some(Day)))
      val table = buildTable(liveSwap, vc = vc)
      val rows = table.pivotRows
      rows.forall(singleValue[String](_, RiskMarketField) === index)
      val index_ = Index.parse(index)(vc.refData).get
      index_.observationDays(vc, liveSwap.averagingPeriod)
      rows.map(singleValue(_, RiskPeriodField).asInstanceOf[DateRangePeriodLabel].dr).toSet should 
        equal (index_.observationDays(vc, liveSwap.averagingPeriod).toSet)
      val positions : Iterable[Qty] = rows.map{
        singleValue(_, PositionField).asInstanceOf[Qty]
      }
      Qty.sum(positions) should be (liveSwap.volume +- 1e-9)
      positions.foreach{
        pos => 
          pos should be (positions.head +- 1e-9)
      }
    }

    // This will fail until fixings are added
    ignore("Should return an empty list for an expired instrument"){
      val table = buildTable(expiredSwap)
      table.pivotRows should be ('empty)
    }
  }

  describe("Mtm"){
    it("mtm report on european option matches black scholes") {
      val month = Month(2014, 12)
      val market = "Nymex WTI"
      val F = Qty("100", USD / BBL)
      val K = Qty("101", USD / BBL)
      val r = Percent("5")
      val vol = Percent("20")
      val option = new FuturesOption(
        market, month, K, Qty("1", BBL), Call, EuropeanOption
      )
      val expiryDay = TestFuturesExpiryRules.Test.expiryRule(market).map(_.optionExpiryDayOrThrow(month)).get
      val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
      val T = Act365.timeBetween(marketDay.day, expiryDay)
      val vc = UnitTestingEnvironment.fromMarketData(
        marketDay, 
        market -> futuresPrices(month -> F),
        USD -> zeroRates(Act365, (marketDay.day + 100) -> r),
        market -> futuresVols(month -> vol)
      )
      val table = buildTable(
        option, 
        fields = Vector(MTMField),
        vc
      )

      val bsValue = new BlackScholes(Call, F.doubleValue, K.doubleValue, vol.checkedPercent, T).undiscountedValue * 
        math.exp(-r.checkedPercent * T) 
      val rows = table.pivotRows
      rows.size shouldBe 1
      singleValue[(Long, Qty)](rows.head, MTMField)._2 shouldEqual Qty(bsValue, USD)
    }
    it("mtm report on future") {
      val month = Month(2014, 12)
      val market = "Nymex WTI"
      val K = Qty("91", USD / BBL)
      val future = new Future(
        market, month, K, Qty("1", BBL)
      )
      val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
      val vc = UnitTestingEnvironment.Null(marketDay)
      val F = vc.futuresPrice(market, month)
      val table = buildTable(
        future,
        fields = Vector(MTMField),
        vc
      )

      val rows = table.pivotRows
      rows.size shouldBe 1
      singleValue[(Long, Qty)](rows.head, MTMField)._2 shouldEqual (F - K) * Qty("1", BBL)
    }
  
    it("mtm report on swap") {
      val oct = Month(2014, 10)
      val nov = oct.next
      val dec = nov.next
      val market = "Nymex WTI"
      val index = "Nymex WTI nearby 1"
      val K = Qty("91", USD / BBL)
      val Fnov = Qty("93", USD / BBL)
      val Fdec = Qty("98", USD / BBL)
      val swap = new CommoditySwap(
        index, oct, K, Qty("1", BBL)
      )
      val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
      val vc = UnitTestingEnvironment.fromMarketData(
        marketDay, 
        market -> futuresPrices(nov -> Fnov, dec -> Fdec)
      )
      val table = buildTable(
        swap,
        fields = Vector(MTMField),
        vc
      )
      val rows = table.pivotRows
      rows.size shouldBe 1
  
      def observedDaysToMonth(vc: ValuationContext, delivery: DateRange): Map[Day, Month] = {
        val fm = vc.futuresMarketOrThrow(market)
        val calendar = vc.futuresCalendarOrThrow(market)
        val observationDays = oct.days.filter(calendar.isBusinessDay).toIterable
        observationDays.map(d => d -> fm.frontMonth(vc.refData, d)).toMap
      }
  
      val weights = {
        val observed = observedDaysToMonth(vc, oct).groupBy(_._2).mapValues(_.size)
        observed.mapValues(i => BigDecimal(i) / BigDecimal(observed.values.sum))
      }
  
      val expected: Qty = ((Fnov * weights(nov) + Fdec * weights(dec)) - K) * Qty("1", BBL)
      val actual: Qty = singleValue[(Long, Qty)](rows.head, MTMField)._2 
  
      implicit val equality = new QuantityTestUtils.EssentiallyEqual()
      actual shouldEqual expected
    }
  }
}

