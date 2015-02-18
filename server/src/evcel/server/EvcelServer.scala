package evcel.server

import evcel.curve.EnvironmentParams
import evcel.curve.environment.MarketDay
import evcel.pivot.PivotField
import evcel.quantity.UOM
import evcel.report.{PivotReportLayout, PivotReportBuilder, TradePivotReportBuilder}
import evcel.valuation.DefaultValuer


case class RiskReportParameters(
  marketDay:MarketDay,
  zeroIR:Boolean,
  environmentParams:EnvironmentParams
) {
  def withValuationCCY(ccy:UOM) = copy(environmentParams=environmentParams.copy(valuationCcy = ccy))
}

case class EnvironmentParameters(marketDay:MarketDay, baseCCY:UOM, zeroIR:Boolean)

case class RiskReportVersion(trades:Long, marketData:Long, referenceData:Long)

class EvcelServer {

  val mockData = new MockData
  mockData.start()

  def referenceData = mockData.refData

  def valuationContext(params:EnvironmentParameters) = {
    mockData.nullValuationContext(params.marketDay, params.zeroIR, EnvironmentParams(baseCCY = params.baseCCY))
  }

  def riskPivot(reportParameters:RiskReportParameters) = {
    val vc = mockData.nullValuationContext(
      reportParameters.marketDay, reportParameters.zeroIR, reportParameters.environmentParams)
    new TradePivotReportBuilder(mockData.tradesAt(mockData.latest), Some(vc), Some(new DefaultValuer()))
  }

}
