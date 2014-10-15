package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument
import evcel.instrument.valuation.Valuer._
import evcel.quantity.Qty
import evcel.instrument.valuation.HedgeInfo
import evcel.instrument.valuation.Valuer

class PositionPivotReport(vc: ValuationContext, valuer : Valuer) extends PivotReport {
  implicit val valuer_ = valuer

  override def rows(instr: Instrument) = {
    val positions = instr.positions(vc)
    positions.map{
      case h : HedgeInfo => PositionRow(h.market, Some(h.period.toString), h.volume)
    }
  }
}

object PositionPivotReport{
  val PositionField = new PivotField{
    override def name = "Position"
  }
}
case class PositionRow(market : String, period : Option[String], position : Qty ) extends PivotRow {

  override def value(field: PivotField): Qty = field match {
    case PositionPivotReport.PositionField => position
    case _ => throw new RuntimeException("Invalid field : " + field)
  }
}
