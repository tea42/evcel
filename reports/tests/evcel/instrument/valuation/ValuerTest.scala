package evcel.instrument.valuation

import evcel.curve.UnitTestingEnvironment
import evcel.curve.curves.{FuturesVolIdentifier, DiscountRateIdentifier, FuturesPriceIdentifier}
import evcel.daterange.DateRangeSugar.{Oct, Nov}
import evcel.curve.environment.MarketDay._
import evcel.daterange.Month
import evcel.instrument.{CommoditySwap, EuropeanOption, FuturesOption}
import evcel.maths.Call
import evcel.quantity.Qty
import org.scalatest.{ShouldMatchers, FunSuite}
import evcel.quantity.UOM._
import scala.language.reflectiveCalls

class ValuerTest extends FunSuite with ShouldMatchers {

  test("test key recording for futures") {
    val market = "Nymex WTI"
    val month = Nov / 14
    val K = Qty("100", USD / BBL)
    val opt = new FuturesOption(
      market, month, K, Qty("1000", BBL), Call, EuropeanOption, bizDaysAfterExpiryToSettlement = 7
    )

    val vc = UnitTestingEnvironment.Null((1 / Oct / 2014).endOfDay)
    val F = vc.futuresPrice(market, month)
    val expiryDay = vc.optionExpiryDay(market, month).get
    val settlementDay = vc.futuresCalendar(market).map(_.addBusinessDays(expiryDay, 7)).get
    val keys = Valuer.keys(vc, opt)
    val expected = Set(
      FuturesPriceIdentifier(market, month),
      DiscountRateIdentifier(USD, settlementDay),
      FuturesVolIdentifier(market, month, K, F)
    )
    keys shouldEqual expected
  }

  test("test key recording for swaps") {
    val oct = Month(2014, 10)
    val nov = oct.next
    val dec = nov.next
    val market = "Nymex WTI"
    val index = "Nymex WTI nearby 1"
    val K = Qty("91", USD / BBL)
    val swap = new CommoditySwap(
      index, oct, K, Qty("1", BBL)
    )
    val vc = UnitTestingEnvironment.Null((1 / Oct / 2014).endOfDay)
    val keys = Valuer.keys(vc, swap)
    val expected = Set(
      FuturesPriceIdentifier(market, nov),
      FuturesPriceIdentifier(market, dec)
    )
    keys shouldEqual expected
  }

}
