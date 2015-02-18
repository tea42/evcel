package evcel.web

import evcel.curve.EnvironmentParams
import evcel.curve.environment.{TimeOfDay, MarketDay}
import evcel.daterange.{Month, Day}
import evcel.quantity.UOM
import evcel.report.{PivotReportBuilder, TradePivotReportBuilder}
import evcel.server.{RiskReportParameters, EvcelServer}
import evcel.webshared._

import scala.concurrent.Future

/**
 * Created by thomas on 17/02/15.
 */
class PageProcessor(evcelServer:EvcelServer) {

  private val refData = new ReferenceDataViewer(evcelServer)
  def addListener( l:(Long)=>Unit ) {
    evcelServer.mockData.addListener(l)
  }

  private val referenceDataListResponse = ReferenceDataListPageResponse(refData.referenceDataPivots.map {
    p => (p.name, ReferenceDataPivot(p.name, WebPivotLayout(
      Nil,
      p.initialLayout.rows.toList.map(_.name),
      p.initialLayout.columns.toList.map(_.name),
      p.initialLayout.measures.toList.map(_.name)))) }
  )

  def pageResponseFor(pageRequest: PageRequest):Future[PageResponse] = {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    Future { generatePageResponseFor(pageRequest) }
  }

  private def generatePageResponseFor(pageRequest: PageRequest):PageResponse = {
    pageRequest match {
      case HomePageRequest(_) => HomePageResponse(List(
        "Reference Data" -> ListReferenceDataPivots(1),
        "Risk Report" -> InitialReportRequest))
      case r:ReportPageRequest => riskReport(r)
      case ListReferenceDataPivots(_) => referenceDataListResponse
      case ReferenceDataPivot(name, layout) => ReferenceDataPageResponse(run(refData.pivotFor(name), layout))
    }
  }

  private val InitialReportRequest = ReportPageRequest(
    WebRiskReportParameters(WebDay("2015-01-01"), true, false, false, false, WebCCY("USD"), WebCCY("GBP"), WebTenor("Month")),
    WebPivotLayout(Nil, List("RiskPeriod"), List("RiskMarket"), List("Position", "MTM"))
  )



  private def riskReport(r:ReportPageRequest) = {
    val pivot = evcelServer.riskPivot(RiskReportParameters(
      MarketDay(
        Day.fromISO(r.parameters.marketDay.day).get,
        TimeOfDay(
          r.parameters.pricesCanMove,
          r.parameters.fixingsShouldExist)),
      r.parameters.zeroIR,
      EnvironmentParams(
        r.parameters.showEqFutures,
        UOM.fromText(r.parameters.baseCCY.name),
        UOM.fromText(r.parameters.valuationCcy.name),
        (r.parameters.tenor.name match {
          case "Default" => None
          case "Day" => Some(Day)
          case "Month" => Some(Month)
        })
      )
    ))
    ReportPageResponse(run(pivot, r.layout))
  }

  private def run(builder:PivotReportBuilder, webLayout:WebPivotLayout) = {
    val layout = PivotData.createLayout(builder, Nil, webLayout.rows, webLayout.columns, webLayout.measures)
    val pivotData = PivotData.pivotData(builder, Nil, layout._2)
    WebPivotResponse(pivotData.spares.toList, pivotData.rowsWidth, pivotData.measuresWidth, pivotData.table)
  }


}
