package evcel.report

import evcel.valuation.Valuer
import evcel.valuation.Valuer._
import evcel.pivot.{PivotRow, PivotField, ExceptionPivotValue}
import evcel.report.PivotValuer._
import scala.util.Either
import evcel.utils.EvcelFail
import evcel.instrument.HedgeInstrument
import evcel.quantity.{Qty, UOM}

case class PivotHedgeInfo(hedge : HedgeInstrument, position : Either[EvcelFail, Double]) extends PivotRow{
  def fields = PivotValuer.POSITION_FIELDS
  def pivotValue(field : PivotField) = field match {
    case RiskMarketField => RiskMarketField.pivotValue(hedge.riskMarketLabel)
    case RiskPeriodField => RiskPeriodField.pivotValue(hedge.riskPeriod)
    case PositionField   => 
      position.fold(
        fail => ExceptionPivotValue(fail), 
        position => PositionField.pivotValue(hedge.volume * Qty(position, UOM.SCALAR))
      )
    case _               => ???
  }
}
