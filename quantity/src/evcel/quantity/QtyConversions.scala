package evcel.quantity

class QtyConversions(rates: Map[(UOM, UOM), BigDecimal]) {
  def rate(from: UOM, to: UOM): Option[BigDecimal] =
    rates.get((from, to)).orElse(rates.get((to, from)).map(1 / _))
}
