package evcel.curve.curves

import evcel.curve.ReferenceData
import evcel.curve.environment._
import evcel.daterange.Month
import evcel.quantity.Qty

case class FuturesPrices(market: String, marketDay: MarketDay, prices: Map[Month, Qty]) extends Curve {
  def apply(point: Any): Either[AtomicEnvironmentFail, Qty] = {
    point match {
      case m: Month => price(m)
      case _ => Left(GeneralAtomicEnvironmentFail(s"Unexpected point $point"))
    }
  }

  def price(m: Month) = {
    prices.get(m).map(Right(_)).getOrElse(Left(MissingCurveData(s"FuturesPrices - $market, $marketDay", m)))
  }
}

case class FuturesPriceIdentifier(market: String, month: Month) extends PriceIdentifier {
  val curveIdentifier = FuturesPricesIdentifier(market)
  val point = month
  override def nullValue(refData: ReferenceData) = {
    val priceUOM = refData.markets.futuresMarketOrThrow(market).priceUOM
    Qty("123", priceUOM)
  }
}

case class FuturesPricesIdentifier(market: String) extends CurveIdentifier

