package evcel.valuation

import evcel.curve.UnitTestingEnvironment
import evcel.curve.curves.{FuturesVolIdentifier, DiscountRateIdentifier, FuturesPriceIdentifier}
import evcel.daterange.DateRangeSugar.{Oct, Nov}
import evcel.curve.environment.MarketDay._
import evcel.daterange.Month
import evcel.instrument.{CommoditySwap, FuturesOption, Index}
import evcel.maths.{EuropeanOption, Call}
import evcel.quantity.Qty
import org.scalatest.{ShouldMatchers, FunSuite}
import evcel.quantity.UOM._
import scala.language.reflectiveCalls
import org.scalatest.matchers.ShouldMatchers
import evcel.utils.EitherTestPimps

class ValuerTest extends FunSuite with ShouldMatchers with EitherTestPimps {

  test("test key recording for futures") {
    val vc = UnitTestingEnvironment.Null((1 / Oct / 2014).endOfDay)
    val market = vc.futuresMarket("Nymex WTI").R
    val month = Nov / 14
    val K = Qty("100", USD / BBL)
    val opt = new FuturesOption(
      market.name, month, K, Qty("1000", BBL), Call, EuropeanOption, bizDaysAfterExpiryToSettlement = 7
    )

    val F = vc.futuresPrice(market, month).R
    val richMarket = RichFuturesMarket(vc.refData, market.name).R
    val expiryDay = richMarket.optionExpiryDay(month).R
    val settlementDay = richMarket.calendar.addBusinessDays(expiryDay, 7)
    val keys = new DefaultValuer().keys(vc, opt)
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
    val vc = UnitTestingEnvironment.Null((1 / Oct / 2014).endOfDay)
    val market = vc.futuresMarket("Nymex WTI").R
    val index = Index.parse("Nymex WTI nearby 1")
    val K = Qty("91", USD / BBL)
    val swap = new CommoditySwap(
      index, oct, K, Qty("1", BBL)
    )
    val keys = new DefaultValuer().keys(vc, swap)
    val expected = Set(
      FuturesPriceIdentifier(market, nov),
      FuturesPriceIdentifier(market, dec)
    )
    keys shouldEqual expected
  }

}
