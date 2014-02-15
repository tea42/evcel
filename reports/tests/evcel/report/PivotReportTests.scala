package evcel.report

import org.scalatest.{FunSpec, Matchers}
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._
import evcel.instrument.trade.Trade
import evcel.report.PivotTrade._
import evcel.instrument.{Future, Instrument}
import scala.language.reflectiveCalls
import evcel.curve.marketdata.MarketDataTest
import evcel.curve.environment.{MarketDay, MarketDayPimps}
import evcel.curve.UnitTestingEnvironment
import evcel.valuation.{Valuer, DefaultValuer}
import evcel.pivot.{PivotTable, PivotField}
import evcel.referencedata.market.TestMarkets._
import evcel.referencedata.market.FuturesMarket
import evcel.daterange.Month
import evcel.report.PivotValuer._

class PivotReportTests extends FunSpec with Matchers with MarketDataTest with MarketDayPimps {
  import PivotReportTests._
  val mkt = "Nymex WTI"
  val marketDay = (1 / Feb / 2014).endOfDay
  val vc = UnitTestingEnvironment.fromMarketData(
    marketDay, 
    "Nymex WTI" -> futuresPrices(Sep / 2014 -> 12 (USD/BBL), Oct / 2014 -> 24 (USD/BBL)),
    "ICE Brent" -> futuresPrices(Sep / 2014 -> 22 (USD/BBL), Oct / 2014 -> 23 (USD/BBL))
  )

  def future(
    id : String, 
    mkt : FuturesMarket = NYMEX_WTI, 
    cpty : String = "Acme",
    month : Month = Sep / 2014) = {
    Trade(
      "1234",
      1 / Jan / 2014,
      cpty,
      Future(mkt.name, month, 10 (mkt.priceUOM), 50(mkt.priceUOM.denominator)),
      Map.empty
    )
  }

  private def buildReport(trades : Seq[Trade], layout : PivotReportLayout) : PivotReport = {
    PivotReport.build(
      trades,
      Some(vc), Some(new DefaultValuer()),
      layout
    )
  }


  describe("Pivot"){
    it("Simplest risk report"){
      val layout = buildReportLayout(
        rows = 
           "RiskMarket",
        cols = 
        """|RiskPeriod
           |M:Position"""
      )
      val trade = future("1234")
      val actual = PivotReportPrinter(buildReport(Vector(trade), layout)).formatted
      val expected = 
        """|           ||  2014-09$
           |RiskMarket || Position$
           |-----------||---------$
           | Nymex WTI || 50.0 bbl$""".stripMargin.replace("$", "")
 
      assert(actual === expected)
    }

    it("Trade and risk report"){
      val layout = buildReportLayout(
        rows = "Counterparty|RiskMarket",
        cols = """|RiskPeriod
                  |M:Position"""
      )
      val trade = future("1234")
      val report = buildReport(Vector(trade), layout)
      val actual = PivotReportPrinter(report).formatted
      val expected =
        """|                          ||  2014-09$
           |Counterparty | RiskMarket || Position$
           |--------------------------||---------$
           |        Acme |  Nymex WTI || 50.0 bbl$""".stripMargin.replace("$", "")
      assert(actual === expected)
    }

    it("Trade and instrument and mtm report"){
      val layout = buildReportLayout(
        rows = "Counterparty|RiskPeriod",
        cols = """|RiskMarket
                  |M:MTM"""
      )
      val trade = future("1234")
      val report = buildReport(Vector(trade), layout)
      val actual = PivotReportPrinter(report).formatted

      val expected =
      """|                          || Nymex WTI
         |Counterparty | RiskPeriod ||    MTM   
         |--------------------------||----------
         |        Acme |    2014-09 || 100.0 USD""".stripMargin
      assert(actual === expected)
    }

    it("Positon and MTM"){
      val layout = buildReportLayout(
        rows = "Counterparty|RiskMarket",
        cols = """|     RiskPeriod    
                  |  M:MTM  | M:Position """
      )
      val trade = future("1234")
      val report = buildReport(Vector(trade), layout)

      val actual = PivotReportPrinter(report).formatted
      val expected =
        """|                          ||        2014-09      $
           |Counterparty | RiskMarket ||    MTM    | Position$
           |--------------------------||---------------------$
           |        Acme |  Nymex WTI || 100.0 USD | 50.0 bbl$""".stripMargin.replace("$", "")
        
      assert(actual === expected)
    }

    it("Position and MTM on multiple trades"){
      val layout = buildReportLayout(

        rows = "Counterparty|RiskMarket",

        cols = """|     RiskPeriod    
                  |  M:MTM  | M:Position """
      )
      val trade1 = future("1234")
      val trade2 = future("1235", mkt = ICE_BRENT)
      val trade3 = future("1236", cpty = "OIL_R_US", month = Oct / 2014)
      val trade4 = future("1237", cpty = "OIL_R_US", month = Oct / 2014)
      val report = buildReport(Vector(trade1, trade2, trade3, trade4), layout)
      val actual = PivotReportPrinter(report).formatted
      val expected =
        """|                          ||        2014-09       |         2014-10       $
           |Counterparty | RiskMarket ||    MTM    | Position |     MTM    |  Position$
           |--------------------------||----------------------------------------------$
           |        Acme |  ICE Brent || 600.0 USD | 50.0 bbl |            |          $
           |             |  Nymex WTI || 100.0 USD | 50.0 bbl |            |          $
           |    OIL_R_US |  Nymex WTI ||           |          | 1400.0 USD | 100.0 bbl$""".stripMargin.replace("$", "")

      assert(actual === expected)
    }

  }
}

object PivotReportTests{
  private val fieldsByName : Map[String, PivotField] = Vector(
    RiskMarketField, 
    RiskPeriodField, 
    PositionField, 
    MTMField,
    CPTY_FIELD
  ).map{
    f => 
      f.name -> f
  }.toMap
  def fieldForName(name : String) = fieldsByName.getOrElse(name, throw new RuntimeException("No field for " + name))
    
  def buildColumnTrees(lines : Seq[String]) : Seq[ColumnFieldsTree] = {
    if (lines.isEmpty)
      Vector()
    else {
      val index = lines.head.indexOf("|")
      if (index > 0){
        val left = lines.map(_.take(index))
        val right = lines.map(_.drop(index + 1))
        buildColumnTrees(left) ++ buildColumnTrees(right)
      } else {
        val topText = lines.head.trim
        val children = buildColumnTrees(lines.tail)

        topText.split(":").toList match {
          case List("M", name) => Vector(ColumnFieldsTree(fieldForName(name), isMeasureNode = true, children))
          case List(name)      => Vector(ColumnFieldsTree(fieldForName(name), isMeasureNode = false, children))
        }
      }
      
    }
  }

  def buildReportLayout(
    rows : String,
    cols : String
  ) : PivotReportLayout = {
    val rowFields = rows.split('|').map{text => fieldForName(text.trim)}.toVector
    val columnFieldTrees : Seq[ColumnFieldsTree] = buildColumnTrees(cols.stripMargin.split("\n"))
    PivotReportLayout(rowFields, columnFieldTrees, filters = Map.empty)
  }

}
