package evcel.curve.curves

import evcel.curve.ReferenceData
import evcel.curve.environment._
import evcel.daterange.{Day, DateRange, Month}
import evcel.quantity.Qty

case class SpotPrices(market: String, marketDay: MarketDay, prices: Map[DateRange, Qty]) extends Curve {
  def isMonthly = prices.keys.forall(_.isInstanceOf[Month])
  def isDaily = prices.keys.forall(_.isInstanceOf[Day])
  require(isDaily || isMonthly, s"Invalid mixed data: ${prices.keys}")

  def apply(point: Any): Either[AtomicEnvironmentFail, Qty] = {
    point match {
      case d: Day if isMonthly => price(d.containingMonth)
      case d: Day => price(d)
      case _ => Left(GeneralAtomicEnvironmentFail(s"Unexpected point $point"))
    }
  }

  def price(dr: DateRange) = {
    prices.get(dr).toRight(left = MissingCurveData(s"SpotPrices - $market, $marketDay", dr))
  }
}

case class SpotPriceIdentifier(market: String, day: Day) extends PriceIdentifier {
  val curveIdentifier = SpotPricesIdentifier(market)
  val point = day
  override def nullValue(refData: ReferenceData) = {
    val priceUOM = refData.markets.spotMarketOrThrow(market).priceUOM
    Qty("321", priceUOM)
  }
}


