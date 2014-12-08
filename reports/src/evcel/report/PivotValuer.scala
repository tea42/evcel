package evcel.report

import evcel.pivot._

object PivotValuer{
  val MTMField = new IdSummingQtyField("MTM")
  val PositionField = new SummingQtyField("Position")
  val RiskMarketField = new StringPivotField("RiskMarket")
  val RiskPeriodField = new PeriodField("RiskPeriod") 
  val POSITION_FIELDS = Vector(PositionField, RiskMarketField, RiskPeriodField)
  val FIELDS: Vector[PivotField] = POSITION_FIELDS :+ MTMField
}
