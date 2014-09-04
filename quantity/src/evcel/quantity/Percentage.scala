package evcel.quantity

object Percentage {
  def apply(valueInPercent: Double) = Qty(valueInPercent, UOM.PERCENT)
}
