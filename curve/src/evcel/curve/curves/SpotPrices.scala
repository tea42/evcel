package evcel.curve.curves

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.curve.environment._
import evcel.curve.environment.MarketDay._
import evcel.daterange._
import evcel.quantity.Qty
import evcel.utils.EitherUtils._
import scala.util.{Either, Left}
import evcel.referencedata.market.SpotMarket

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

case class SpotPriceIdentifier(market: SpotMarket, day: Day) extends PriceIdentifier {
  val curveIdentifier = SpotPricesIdentifier(market.name)
  val point = day
  override def nullValue = {
    Qty("321", market.priceUOM)
  }

  override def dP(vc: ValuationContext) = {
    Qty(".25", market.priceUOM)
  }

  override def forwardStateValue(
    refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay
    ) = {
    if (forwardMarketDay >= day.endOfDay) {
      sys.error(s"$this has expired on $forwardMarketDay")
    }
    original(this)
  }
}


