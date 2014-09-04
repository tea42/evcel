package evcel.quantity

object Percentage {
  def apply(valueInPercent: Double) = Qty(valueInPercent, UOM.PERCENT)
  def apply(valueInPercent: String) = Qty(valueInPercent, UOM.PERCENT)
}
