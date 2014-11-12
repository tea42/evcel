package evcel.curve


import evcel.curve.environment.MarketDay._
import evcel.curve.marketdata._
import evcel.daterange.DateRangeSugar._
import evcel.quantity.UOM._
import evcel.quantity.{Percent, Qty}
import evcel.quantity.Qty._
import evcel.referencedata.market.FXPair
import org.scalatest.{FunSuite, FunSpecLike, ShouldMatchers}

import scala.language.reflectiveCalls

class ValuationContextTests extends FunSuite with MarketDataTest with ShouldMatchers{

  val vc = UnitTestingEnvironment.fromMarketData(
    (10 / Sep / 2014).endOfDay,

    "Nymex WTI" -> futuresPrices(Sep / 2014 -> Qty("100", USD / MT)),

    USD -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("5"), (31 / Dec / 2015) -> Percent("8"))),
    GBP -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("1"), (31 / Dec / 2015) -> Percent("2"))),
    EUR -> ZeroRateData(Act365, List((31 / Dec / 2014) -> Percent("3"), (31 / Dec / 2015) -> Percent("4.5"))),

    (USD, GBP) -> SpotFXData(Qty("1.6", USD / GBP)),
    (USD, EUR) -> SpotFXData(Qty("1.25", USD / EUR)),

    "Nymex WTI" -> FuturesVolData(List((Sep / 2014, Percent("20"), Nil)))
  )

  test("spot fx") {
    vc.spotFX(FXPair(GBP, GBP)) shouldEqual Qty(1, SCALAR)
    vc.spotFX(FXPair(GBP, USD)) shouldEqual Qty(1.6, USD/GBP)
    vc.spotFX(FXPair(USD, GBP)) shouldEqual Qty(1.6, USD/GBP).invert
    vc.spotFX(FXPair(GBP, EUR)) shouldEqual vc.spotFX(FXPair(USD, GBP)).invert * vc.spotFX(FXPair(USD, EUR))
    vc.spotFX(FXPair(EUR, GBP)) shouldEqual vc.spotFX(FXPair(GBP, EUR)).invert
  }

  test("today fx") {
    vc.todayFX(FXPair(GBP, GBP)) shouldEqual Qty(1, SCALAR)

    vc.todayFX(FXPair(GBP, USD)) shouldEqual
      Qty(1.6, USD/GBP) *
        (vc.discountRate(GBP, vc.marketDay.day + 2) / vc.discountRate(USD, vc.marketDay.day + 2)).toQty

    vc.todayFX(FXPair(GBP, EUR)) shouldEqual
      vc.spotFX(FXPair(USD, GBP)).invert * vc.spotFX(FXPair(USD, EUR)) *
        (vc.discountRate(GBP, vc.marketDay.day + 2) / vc.discountRate(EUR, vc.marketDay.day + 2)).toQty
  }

  test("forward fx") {
    val day = vc.marketDay.day + 150
    vc.forwardFX(FXPair(GBP, GBP), day) shouldEqual Qty(1, SCALAR)

    vc.forwardFX(FXPair(GBP, USD), day) shouldEqual
      Qty(1.6, USD/GBP) *
        (vc.discountRate(GBP, vc.marketDay.day + 2) / vc.discountRate(USD, vc.marketDay.day + 2)).toQty *
        (vc.discountRate(GBP, day) / vc.discountRate(USD, day)).toQty

    vc.forwardFX(FXPair(GBP, EUR), day) shouldEqual
      vc.spotFX(FXPair(USD, GBP)).invert * vc.spotFX(FXPair(USD, EUR)) *
        (vc.discountRate(GBP, vc.marketDay.day + 2) / vc.discountRate(EUR, vc.marketDay.day + 2)).toQty *
        (vc.discountRate(GBP, day) / vc.discountRate(EUR, day)).toQty
  }

  test("forwardState for discount curve") {
    val forwardDate = vc.marketDay.day + 150
    vc.forwardState(forwardDate.endOfDay).discountRate(USD, forwardDate) shouldEqual 1.0
    vc.forwardState(forwardDate.endOfDay).discountRate(USD, forwardDate + 50) shouldEqual
      vc.discountRate(USD, forwardDate + 50) / vc.discountRate(USD, forwardDate)
  }

  test("forwardstate for spotfx") {
    val pair = FXPair(GBP, USD)
    val forwardDate = vc.marketDay.day + 150
    val forwardSpotDate = vc.refData.fxMarket(pair).spotDate(vc.refData, forwardDate)
    vc.forwardState(forwardDate.endOfDay).spotFX(pair) shouldEqual
      vc.forwardFX(pair, forwardSpotDate)
  }

}
