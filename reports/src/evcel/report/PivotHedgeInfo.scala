package evcel.report

import evcel.instrument.valuation.HedgeInfo
import evcel.instrument.valuation.Valuer
import evcel.instrument.valuation.Valuer._
import evcel.pivot.{PivotRow, PivotField}
import evcel.report.PivotValuer._

case class PivotHedgeInfo(hedgeInfo : HedgeInfo) extends PivotRow{
  def fields = PivotValuer.POSITION_FIELDS
  def pivotValue(field : PivotField) = field match {
    case RiskMarketField => RiskMarketField.pivotValue(hedgeInfo.riskMarket)
    case RiskPeriodField => RiskPeriodField.pivotValue(hedgeInfo.riskPeriod)
    case PositionField   => PositionField.pivotValue(hedgeInfo.volume)
    case _               => ???
  }
}
