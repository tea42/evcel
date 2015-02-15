package evcel.curve.curves

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.curve.environment._
import evcel.daterange.Month
import evcel.quantity.Qty
import evcel.utils.EitherUtils._
import scala.util.{Either, Left, Right}
import evcel.referencedata.market.FuturesMarket

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

case class FuturesPriceIdentifier(market: FuturesMarket, month: Month) extends PriceIdentifier with MarketDayPimps{
  val curveIdentifier = FuturesPricesIdentifier(market.name)
  val point = month
  override def nullValue = {
    Qty("123", market.priceUOM)
  }

  override def dP(vc: ValuationContext) = {
    Qty(".25", market.priceUOM)
  }

  override def forwardStateValue(refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay) = {
    for {
      rule <- refData.futuresExpiryRules.expiryRule(market.name)
      ltd <- rule.futureExpiryDay(month)
      value <- original(this)
    } yield {
      if (forwardMarketDay >= ltd.endOfDay) {
        sys.error(s"$this has expired on $forwardMarketDay")
      }
      value
    }
  }
          
}


