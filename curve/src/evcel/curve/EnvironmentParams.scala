package evcel.curve

import evcel.quantity.UOM

case class EnvironmentParams(
  showEqFutures: Boolean = false,
  valuationCcy: UOM = UOM.USD
  )

object EnvironmentParams {
  val Default = new EnvironmentParams()
}
