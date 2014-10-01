package evcel.curve.curves

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.curve.environment._
import evcel.curve.environment.MarketDay._
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

  override def dP(vc: ValuationContext) = {
    val priceUOM = vc.refData.markets.futuresMarketOrThrow(market).priceUOM
    Qty(".25", priceUOM)
  }

  override def forwardStateValue(
    refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay
    ) = {
    val ltd = refData.futuresExpiryRules.expiryRule(market).map(_.futureExpiryDayOrThrow(month)).getOrElse(
      sys.error(s"Invalid market: $market")
    )
    if (forwardMarketDay >= ltd.endOfDay) {
      sys.error(s"$this has expired on $forwardMarketDay")
    }
    original(this)
  }
}


