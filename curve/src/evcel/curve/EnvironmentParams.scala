package evcel.curve

import evcel.daterange.TenorType
import evcel.quantity.UOM

case class EnvironmentParams(
  showEqFutures: Boolean = false,
  valuationCcy: UOM = UOM.USD,
  tenor: Option[TenorType] = None,
  positionAsPower: Boolean = true // Power or Energy. Where Power = Energy / Time
  ) {

  def withShowEqFutures(b: Boolean = true) = copy(showEqFutures = b)
  def withTenor(t: Option[TenorType]) = copy(tenor = t)
  def withPositionAsPower = copy(positionAsPower = true)
  def withPositionAsEnergy = copy(positionAsPower = false)
}

object EnvironmentParams {
  val Default = new EnvironmentParams()
}
