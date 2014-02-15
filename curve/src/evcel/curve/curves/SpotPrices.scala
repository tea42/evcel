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
        val maybeValue : Option[Qty] = Option(
          prices.headMap(day, /* inclusive = */ true)
        ).flatMap{
          tailMap => 
            Option(tailMap.lastEntry).map(_.getValue)
        }
        maybeValue.toRight(GeneralAtomicEnvironmentFail(s"No spot prices for $market, $marketDay, day: $day"))
        
      case _ => Left(GeneralAtomicEnvironmentFail(s"Unexpected point $point"))
    }
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


