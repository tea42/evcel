package evcel.report

import evcel.referencedata.market.IndexLabel
import org.scalatest.{FunSpec, Matchers}
import evcel.daterange.DateRangeSugar._
import evcel.instrument._
import evcel.quantity.{Qty, Percent}
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.valuation._
import evcel.curve._
import evcel.curve.marketdata.{FuturesPriceData, MarketDataTest, Act365}
import scala.language.reflectiveCalls
import evcel.quantity.utils.QuantityTestUtils._
import evcel.daterange._
import evcel.instrument.trade.Trade
import evcel.valuation.Valuer._
import evcel.pivot.{PivotField, PivotTable, PivotRow}
import evcel.referencedata.{Level, TestFuturesExpiryRules}
import evcel.curve.environment.{MarketDay, TimeOfDay, MarketDayPimps}
import evcel.maths.models.BlackScholes
import evcel.maths.{Call, EuropeanOption}
import evcel.quantity.utils.QuantityTestUtils
import evcel.report.PivotValuer._
import scala.math.BigDecimal
import evcel.utils.EitherTestPimps

class ValuationTableBuilderTests extends FunSpec with MarketDataTest with Matchers with 
  EitherTestPimps with MarketDayPimps {
  val valuer = new DefaultValuer()
  val market = "Nymex WTI"
  val F = Qty("100", USD / BBL)
  val K = Qty("101", USD / BBL)
  val marketDay = (1 / Jun / 2014).endOfDay
  val valuationContext = UnitTestingEnvironment.fromMarketData(
    marketDay,
    market -> futuresPrices(Nov / 2014 -> F, Dec / 2014 -> F)
  )
  val index = IndexLabel.parse("Nymex WTI nearby 1")
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

      singleValue[String](row, RiskMarketField) should be (index.indexName)
      singleValue[PeriodLabel](row, RiskPeriodField) should be (DateRangePeriodLabel(liveSwap.averagingPeriod))
      singleValue[Qty](row, PositionField) should be (liveSwap.volume +-1e-9)
    }

    it("Should return equal volumes when run daily"){
      val vc = valuationContext.copy(params = valuationContext.params.copy(tenor = Some(Day)))
      val table = buildTable(liveSwap, vc = vc)
      val rows = table.pivotRows
      rows.forall(singleValue[String](_, RiskMarketField) === index)
      val richIndex = RichIndex(vc.refData, index, Level.Close).R
      rows.map(singleValue(_, RiskPeriodField).asInstanceOf[DateRangePeriodLabel].dr).toSet should 
        equal (richIndex.observationDays(liveSwap.averagingPeriod).toSet)
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
      val expiryDay = TestFuturesExpiryRules.Test.expiryRule(market).right.get.optionExpiryDay(month).right.get
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
      val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
      val vc = UnitTestingEnvironment.Null(marketDay)
      val month = Month(2014, 12)
      val market = vc.futuresMarket("Nymex WTI").R
      val K = Qty("91", USD / BBL)
      val future = new Future(
        market.name, month, K, Qty("1", BBL)
      )
      val F = vc.futuresPrice(market, month).R
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
      val marketDay = MarketDay(Day(2014, 6, 1), TimeOfDay.end)
      val Fnov = Qty("93", USD / BBL)
      val Fdec = Qty("98", USD / BBL)
      val vc = UnitTestingEnvironment.fromMarketData(
        marketDay, 
        market -> futuresPrices(nov -> Fnov, dec -> Fdec)
      )
      val index = IndexLabel.parse("Nymex WTI nearby 1")
      val K = Qty("91", USD / BBL)
      val swap = new CommoditySwap(
        index, oct, K, Qty("1", BBL)
      )
      val table = buildTable(
        swap,
        fields = Vector(MTMField),
        vc
      )
      val rows = table.pivotRows
      rows.size shouldBe 1
  
      def observedDaysToMonth(vc: ValuationContext, delivery: DateRange): Map[Day, Month] = {
        val richMarket = RichFuturesMarket(vc.refData, market).R
        val observationDays = oct.days.filter(richMarket.calendar.isBusinessDay).toIterable
        observationDays.map(d => d -> richMarket.frontMonth(d).R).toMap
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

