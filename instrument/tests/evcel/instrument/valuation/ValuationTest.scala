package evcel.instrument.valuation

import evcel.curve.curves.{SpotPriceIdentifier, DiscountRateIdentifier, FuturesPriceIdentifier, FuturesVolIdentifier}
import evcel.curve.environment.MarketDay
import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata.Act365
import evcel.curve.{UnitTestingEnvironment, ValuationContext}
import evcel.daterange.DateRangeSugar.Jan
import evcel.daterange.{DateRange, Month}
import evcel.quantity.UOM._
import evcel.quantity.{Percent, Qty}
import org.scalatest.{FunSuite, ShouldMatchers}

import scala.language.reflectiveCalls

trait ValuationTest extends FunSuite with ShouldMatchers {
  val valuer = new DefaultValuer

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
}
