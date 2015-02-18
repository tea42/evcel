package evcel.web

import java.net.URLEncoder

import evcel.pivot._
import evcel.quantity.UOM
import evcel.referencedata.FuturesExpiryRule
import evcel.referencedata.market.Currency
import evcel.report.{ColumnFieldsTree, PivotReportLayout, PivotReportBuilder}
import evcel.server.EvcelServer

/**
 * Creates pivot representations of reference data
 */
class ReferenceDataViewer(server:EvcelServer) {

  case class InitialLayout(rows:Seq[PivotField], columns:Seq[PivotField], measures:Seq[PivotField])

  trait ReferenceDataPivot extends PivotReportBuilder {
    def name:String
    def initialLayout:InitialLayout
  }
  val referenceDataPivots:List[ReferenceDataPivot] = List(
    new FuturesExpiryRulePivot(server.referenceData.futuresExpiryRules.rules),
    new CurrenciesPivot(server.referenceData.markets.currencies)
    //Markets
    //Spot markets
    //Calendars
  )

  def pivotFor(name:String) = referenceDataPivots.find(_.name == name).get

  class CurrenciesPivot(currencies: Map[UOM, Currency]) extends ReferenceDataPivot {
    def name = "Currencies"
    val codeField = StringPivotField("Code")
    val nameField = StringPivotField("Name")
    val daysToSpotField = StringPivotField("Days To Spot")
    val calendarField = StringPivotField("Calendar")
    val spotDayUsesBothCalendarsField = StringPivotField("Spot Day Rule")

    override def initialLayout = InitialLayout(Seq(codeField), Seq(),
      Seq(nameField, daysToSpotField, calendarField, spotDayUsesBothCalendarsField))

    override def fields: Seq[PivotField] =
      List(codeField, nameField, daysToSpotField, calendarField, spotDayUsesBothCalendarsField)

    override def createTable(fields: Seq[PivotField], filters: Map[PivotField, (PivotValue) => Boolean]): PivotTable = {
      PivotTable(CurrenciesPivot.this.fields, currencies.values.toSeq.map { currency =>
        Seq(
          codeField.pivotValue(currency.uom.toString),
          nameField.pivotValue(currency.name),
          daysToSpotField.pivotValue(currency.daysToSpot.toString),
          calendarField.pivotValue(currency.calendarName),
          spotDayUsesBothCalendarsField.pivotValue({
            currency.spotDayUsesBothCalendars match { case true => "Both"; case false => "Single" }
          }))
      })
    }
  }

  class FuturesExpiryRulePivot(rules: Map[String, FuturesExpiryRule]) extends ReferenceDataPivot {
    def name = "ExpiryRules"
    val marketField = StringPivotField("Market")
    val monthField = DateRangeField("Month")
    val expiryDayField = DayField("Expiry Day")

    override def initialLayout = InitialLayout(Seq(marketField), Seq(monthField), Seq(expiryDayField))

    override def fields: Seq[PivotField] = List(marketField, monthField, expiryDayField)

    override def createTable(fields: Seq[PivotField], filters: Map[PivotField, (PivotValue) => Boolean]): PivotTable = {
      PivotTable(FuturesExpiryRulePivot.this.fields, rules.toSeq.flatMap { case (market, rule) => {
        val marketValue = marketField.pivotValue(market)
        rule.futureExpiries.map { case (month, day) =>
          Seq(marketValue, monthField.pivotValue(month), expiryDayField.pivotValue(day))
        }
      } })
    }
  }


}
