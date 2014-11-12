package evcel.instrument.valuation

import evcel.curve.curves._
import evcel.curve.environment.{SpotFXIdentifier, MarketDay}
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.DateRangeSugar.{Oct, Jan}
import evcel.daterange.{Day, DateRange, Month}
import evcel.instrument._
import evcel.quantity.UOM._
import evcel.quantity.{BDQty, Percent, Qty}
import org.scalatest.{FunSuite, ShouldMatchers}
import scala.language.reflectiveCalls
import scalaz.{Show, Equal}

trait ValuationTest extends FunSuite with ShouldMatchers {
  implicit val valuer = new DefaultValuer
  implicit val refData = UnitTestingEnvironment.testRefData

  val oct = Oct / 14
  val nov = oct.next
  val dec = nov.next
  val jan = dec.next
  val wti = "Nymex WTI"
  val nbp = "ICE NBP"
  val nbp1st = "ICE NBP nearby 1"
  val wti1st = "Nymex WTI nearby 1"
  val sing = "Singapore Gasoil 0.05"

  def createVC(marketDay: MarketDay = (1 / Jan / 2014).endOfDay) = {
    UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(market, month) =>
        val priceUOM = refData.markets.futuresMarketOrThrow(market).priceUOM
        Qty("100.0", priceUOM) + Qty(month.monthNumber, priceUOM)
      case SpotPriceIdentifier(market, d) =>
        val priceUOM = refData.markets.spotMarketOrThrow(market).priceUOM
        Qty("100.0", priceUOM) + Qty(d.dayNumber, priceUOM)
      case BaseFXRateKey(USD, GBP) => Qty("1.6", USD/GBP)
      case BaseFXRateKey(USD, EUR) => Qty("1.25", USD/EUR)
      case DiscountRateIdentifier(USD, day) => math.exp(-0.05 * Act365.timeBetween(marketDay.day, day))
      case DiscountRateIdentifier(GBP, day) => math.exp(-0.025 * Act365.timeBetween(marketDay.day, day))
      case DiscountRateIdentifier(EUR, day) => math.exp(-0.015 * Act365.timeBetween(marketDay.day, day))
      case FuturesVolIdentifier(market, month, strike, _) => Percent("20") + Percent((strike.doubleValue % 5).toString)
    })
  }

  def futuresMonthWeightings(vc: ValuationContext, ffpi: String, delivery: DateRange): Map[Month, BigDecimal] = {
    val index = FuturesFrontPeriodIndex.parse(ffpi).getOrElse(sys.error(s"Invalid $ffpi"))
    val observedDaysToMonth = index.observedDaysToMonth(vc, delivery)
    val grouped = observedDaysToMonth.groupBy(_._2)
    grouped.mapValues(e => BigDecimal(e.size) / BigDecimal(observedDaysToMonth.size))
  }
  
  def createLookalike(market: String = wti, month: Month = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL), bizDaysSett: Option[Int] = None) =
    new CommoditySwapLookalike(market, month, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSwap(market: String = wti1st, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(market, period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSwapNBP(market: String = nbp1st, period: DateRange = oct,
    strike: BDQty = Qty("1", GBP / THM), volume: BDQty = Qty(123, THM/DAY), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(market, period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSingSwap(market: String = sing, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(market, period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSingSpreadSwap(market: String = s"$sing vs $wti1st", period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT),
    rule: SwapPricingRule = NonCommonSwapPricingRule, bizDaysSett: Option[Int] = None) =
    new CommoditySwapSpread(market, period, strike, volume, CommonSwapPricingRule, bizDaysToSettlement = bizDaysSett)

  def createFuture(market: String = wti, period: Month = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL)) = {
    new Future(market, period, strike, volume)
  }
  def createFutureNBP(market: String = nbp, period: Month = oct,
    strike: BDQty = Qty("100", PENCE / THM), volume: BDQty = Qty(123, THM/DAY)) = {
    new Future(market, period, strike, volume)
  }

  def fxForward(volume: BDQty = Qty(1013, GBP), strike: BDQty = Qty("1.6", USD / GBP), delivery: Day = oct.lastDay) = {
    new FXForward(volume, strike, delivery)
  }

  implicit def hedgeInfoEqual(implicit q: Equal[Qty]): Equal[HedgeInfo] = Equal.equal[HedgeInfo]{
    case (lhs, rhs) => lhs.market == rhs.market && lhs.period == rhs.period && q.equal(lhs.volume, rhs.volume)
  }
  implicit def hedgeInfoShow(implicit s: Show[Qty]): Show[HedgeInfo] = Show.shows[HedgeInfo]{
    case hi => s"(${hi.market}, ${hi.period}, ${s.shows(hi.volume)})"
  }

}
