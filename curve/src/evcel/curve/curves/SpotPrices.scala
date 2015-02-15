package evcel.curve.curves

import evcel.curve.ValuationContext
import evcel.referencedata.ReferenceData
import evcel.curve.environment._
import evcel.daterange._
import evcel.quantity.Qty
import evcel.utils.EitherUtils._
import scala.util.{Either, Left}
import evcel.referencedata.market.SpotMarket
import java.util.{TreeMap => JavaTreeMap}

case class SpotPrices(market: String, marketDay: MarketDay, prices: JavaTreeMap[Day, Qty]) extends Curve {

  private def checkDayIsValid(day : Day) {
    if (day < marketDay.day ||
        day == marketDay.day && marketDay.timeOfDay.fixingsShouldExist)
      throw new IllegalStateException(
        s"Code error: Asked for spot price for day: $day on marketDay: $marketDay"
      )
  }

  def apply(point: Any): Either[AtomicEnvironmentFail, Qty] = {
    point match {
      case day: Day =>
        checkDayIsValid(day)
        if (prices.isEmpty)
          Left(GeneralAtomicEnvironmentFail(s"No prices for market $market on $marketDay"))
        else {
          // Last price on or before day if one exists - otherwise first price.
          val price = Option(prices.headMap(day, /* inclusive = */ true).lastEntry).getOrElse(prices.firstEntry).getValue
          Right(price)
        }
        
      case _ => 
          Left(GeneralAtomicEnvironmentFail(s"Unexpected point $point used to ask prices for market $market on $marketDay"))
    }
  }
}

case class SpotPriceIdentifier(market: SpotMarket, day: Day) extends PriceIdentifier with MarketDayPimps{
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


