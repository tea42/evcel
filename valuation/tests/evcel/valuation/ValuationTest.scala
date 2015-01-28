package evcel.valuation

import evcel.curve.curves._
import evcel.curve.environment.{SpotFXIdentifier, MarketDay}
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.DateRangeSugar._
import evcel.daterange._
import evcel.instrument._
import evcel.quantity.UOM._
import evcel.quantity.{BDQty, Percent, Qty}
import evcel.referencedata.Level
import evcel.referencedata.market.{IndexLabelSpread, IndexLabel}
import org.scalatest.{FunSuite, ShouldMatchers}
import scala.language.reflectiveCalls
import scalaz.{Show, Equal}
import scala.math.BigDecimal
import org.scalatest.matchers.ShouldMatchers
import evcel.utils.EitherTestPimps

trait ValuationTest extends FunSuite with ShouldMatchers 
  with EitherTestPimps
{
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
  val sing = IndexLabel.parse("Singapore Gasoil 0.05")

  def createVC(marketDay: MarketDay = (1 / Jan / 2014).endOfDay) = {
    UnitTestingEnvironment(marketDay, {
      case FuturesPriceIdentifier(market, month) =>
        val priceUOM = RichFuturesMarket(refData, market.name).R.priceUOM
        Qty("100.0", priceUOM) + Qty(month.monthNumber, priceUOM)
      case SpotPriceIdentifier(market, d) =>
        val priceUOM = market.priceUOM
        Qty("100.0", priceUOM) + Qty(d.dayNumber, priceUOM)
      case BaseFXRateKey(USD, GBP) => Qty("1.6", USD/GBP)
      case BaseFXRateKey(USD, EUR) => Qty("1.25", USD/EUR)
      case DiscountRateIdentifier(USD, day) => Qty(math.exp(-0.05 * Act365.timeBetween(marketDay.day, day)), SCALAR)
      case DiscountRateIdentifier(GBP, day) => Qty(math.exp(-0.025 * Act365.timeBetween(marketDay.day, day)), SCALAR)
      case DiscountRateIdentifier(EUR, day) => Qty(math.exp(-0.015 * Act365.timeBetween(marketDay.day, day)), SCALAR)
      case FuturesVolIdentifier(market, month, strike, _) => Percent("20") + Percent((strike.doubleValue % 5).toString)
    })
  }

  def futuresMonthWeightings(vc: ValuationContext, ffpi: String, delivery: DateRange): Map[Month, BigDecimal] = {
    val index = IndexLabel.parse(ffpi)
    val richIndex = RichIndex(vc.refData, index, Level.Close).R.asInstanceOf[RichFuturesBasedIndex]
    val grouped = richIndex.observationDays(delivery).groupBy(richIndex.observedMonth(_).R)
    grouped.mapValues(e => BigDecimal(e.size) / BigDecimal(richIndex.observationDays(delivery).size))
  }
  
  def createLookalike(market: String = wti, month: Month = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL), bizDaysSett: Option[Int] = None) =
    new CommoditySwapLookalike(market, month, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSwap(market: String = wti1st, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / BBL), volume: BDQty = Qty(123, BBL), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(IndexLabel.parse(market), period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSwapNBP(market: String = nbp1st, period: DateRange = oct,
    strike: BDQty = Qty("1", GBP / THM), volume: BDQty = Qty(123, THM/DAY), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(IndexLabel.parse(market), period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSingSwap(market: String = sing.indexName, period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT), bizDaysSett: Option[Int] = None) =
    new CommoditySwap(IndexLabel.parse(market), period, strike, volume, bizDaysToSettlement = bizDaysSett)

  def createSingSpreadSwap(market: String = s"$sing vs $wti1st", period: DateRange = oct,
    strike: BDQty = Qty("100", USD / MT), volume: BDQty = Qty(123, MT),
    rule: SwapSpreadPricingRule = NonCommonSwapPricingRule, bizDaysSett: Option[Int] = None) =
    new CommoditySwapSpread(
      IndexLabelSpread.parse(market).get, period, strike, volume, CommonSwapPricingRule, bizDaysToSettlement = bizDaysSett)

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
}
