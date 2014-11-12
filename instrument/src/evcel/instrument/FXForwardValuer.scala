package evcel.instrument

import evcel.curve.ValuationContext
import evcel.daterange.Day
import evcel.instrument.valuation.{Valuer, Valuation}
import evcel.quantity.{BDQty, Qty}
import evcel.quantity.Qty._
import evcel.referencedata.market.FXPair
import evcel.instrument.valuation.Valuer._

case class CashValuer(vc: ValuationContext, cash: Cash) extends Valuation{
  override def value: Qty = {
    val discount = cash.delivery.map(d => vc.discountRate(cash.volume.uom, d)).getOrElse(1.0).toQty
    cash.volume * discount
  }
}

case class FXForwardValuer(vc: ValuationContext, fxForward: FXForward)(implicit val valuer: Valuer) extends Valuation{
  override def value: Qty = {
    val strike = if(fxForward.strike.uom.numerator == fxForward.volume.uom)
      fxForward.strike.invert
    else
      fxForward.strike

    val paying = new Cash(-fxForward.volume mult strike, Some(fxForward.delivery))
    val receiving = new Cash(fxForward.volume, Some(fxForward.delivery))

    paying.mtm(vc) + receiving.mtm(vc)
  }
}
