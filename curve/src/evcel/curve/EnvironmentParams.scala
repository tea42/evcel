package evcel.curve

import evcel.daterange.TenorType
import evcel.quantity.UOM

case class EnvironmentParams(
  showEqFutures: Boolean = false,
  baseCCY: UOM = UOM.USD, // base for fx rates. domestic ccy in FXPair
  valuationCcy: UOM = UOM.USD,
  tenor: Option[TenorType] = None
) {

  def withShowEqFutures(b: Boolean = true) = copy(showEqFutures = b)
  def withTenor(t: Option[TenorType]) = copy(tenor = t)
  def withValuationCcy(ccy: UOM) = copy(valuationCcy = ccy)
}

object EnvironmentParams {
  val Default = new EnvironmentParams()
}
