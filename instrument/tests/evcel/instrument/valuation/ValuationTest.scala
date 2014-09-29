package evcel.instrument.valuation

import evcel.curve.curves.{SpotPriceIdentifier, DiscountRateIdentifier, FuturesPriceIdentifier, FuturesVolIdentifier}
import evcel.curve.environment.MarketDay
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.DateRangeSugar.{Oct, Jan}
import evcel.daterange.{DateRange, Month}
import evcel.instrument.{Future, CommoditySwap}
import evcel.quantity.UOM._
import evcel.quantity.{BDQty, Percent, Qty}
import org.scalatest.{FunSuite, ShouldMatchers}

import scala.language.reflectiveCalls

trait ValuationTest extends FunSuite with ShouldMatchers {
  implicit val valuer = new DefaultValuer
  implicit val position = new Position

  val oct = Oct / 14
  val nov = oct.next
  val dec = nov.next
  val jan = dec.next
  val wti = "Nymex WTI"
  val wti1st = "Nymex WTI nearby 1"
  val sing = "Singapore Gasoil 0.05"

  def createVC(marketDay: MarketDay = (1 / Jan / 2014).endOfDay) = {
    val refData = UnitTestingEnvironment.testRefData

    UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(market, month) =>
        val priceUOM = refData.markets.futuresMarketOrThrow(market).priceUOM
        Qty("100.0", priceUOM) + Qty(month.monthNumber, priceUOM)
      case SpotPriceIdentifier(market, d) =>
        val priceUOM = refData.markets.spotMarketOrThrow(market).priceUOM
        Qty("100.0", priceUOM) + Qty(d.dayNumber, priceUOM)
      case DiscountRateIdentifier(USD, day) => math.exp(-0.05 * Act365.timeBetween(marketDay.day, day))
      case FuturesVolIdentifier(market, month, strike, _) => Percent("20") + Percent((strike.doubleValue % 5).toString)
    })
  }

  def futuresMonthWeightings(vc: ValuationContext, ffpi: String, delivery: DateRange): Map[Month, BigDecimal] = {
    val index = FuturesFrontPeriodIndex.unapply((vc.refData, ffpi)).getOrElse(sys.error(s"Invalid $ffpi"))
    val observedDaysToMonth = index.observedDaysToMonth(vc, delivery)
    val grouped = observedDaysToMonth.groupBy(_._2)
    grouped.mapValues(e => BigDecimal(e.size) / BigDecimal(observedDaysToMonth.size))
  }

  def createSwap(market: String = wti1st, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL)) =
    new CommoditySwap(market, period, strike, volume)

  def createSingSwap(market: String = sing, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT)) =
    new CommoditySwap(market, period, strike, volume)

  def createSingSpreadSwap(market: String = s"$sing vs $wti1st", period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT),
    rule: SwapPricingRule = NonCommonSwapPricingRule) =
    new CommoditySwap(market, period, strike, volume, CommonSwapPricingRule)

  def createFuture(market: String = wti, period: Month = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL)) = {
    new Future(market, period, strike, volume)
  }
}
