package evcel.curve

import evcel.daterange.TenorType
import evcel.quantity.UOM

case class EnvironmentParams(
  showEqFutures: Boolean = false,
  valuationCcy: UOM = UOM.USD,
  tenor: Option[TenorType] = None
  ) {

  def withShowEqFutures(b: Boolean) = copy(showEqFutures = b)
  def withTenor(t: Option[TenorType]) = copy(tenor = t)
}

object EnvironmentParams {
  val Default = new EnvironmentParams()
}
