package evcel.curve.curves

import evcel.curve.ReferenceData
import evcel.daterange.Day
import evcel.daterange.Month
import evcel.quantity.{UOM, Qty}
import evcel.curve.environment.Curve
import evcel.curve.environment.CurveIdentifier
import evcel.curve.environment.AtomicDatumIdentifier
import evcel.curve.environment.MarketDay

case class FuturesPrices(market: String, marketDay: MarketDay, prices: Map[Month, Qty]) extends Curve {
  def apply(point: Any) = {
    point match {
      case m: Month => price(m)
      case _ => throw new RuntimeException(s"Unexpected point $point")
    }
  }

  def price(m: Month): Qty = {
    prices.getOrElse(m, throw MissingCurveDataException(s"FuturesPrices - $market, $marketDay", m))
  }
}

case class FuturesPriceIdentifier(market: String, month: Month) extends AtomicDatumIdentifier {
  val curveIdentifier = FuturesPricesIdentifier(market)
  val point = month
  override def nullValue(refData: ReferenceData) = {
    val priceUOM = refData.markets.futuresMarketOrThrow(market).priceUOM
    Qty("123", priceUOM)
  }
}

case class FuturesPricesIdentifier(market: String) extends CurveIdentifier

