package evcel.referencedata.market

import evcel.quantity.QtyConversions
import evcel.quantity.UOM._
import evcel.quantity.QtyConversions
import evcel.quantity.UOM._
import evcel.referencedata.calendar.TestCalendars
import evcel.referencedata.{TestFuturesExpiryRules, ReferenceData, Level}

// should read from main refdata for markets
object TestMarkets {
  def testRefData = ReferenceData(
    TestFuturesExpiryRules.Test,
    TestCalendars.Empty,
    TestMarkets.Default
  )

  val conv = new QtyConversions(Map((MT -> BBL) -> 7.5))
  val NYMEX_WTI = new FuturesMarket(
    "Nymex WTI", "NYM", 
    USD/BBL, BBL, 
    VolumeCalcRuleLabel.Default, conversions = conv)
  val ICE_NBP   = new FuturesMarket(
    "ICE NBP", "NBP", 
    PENCE/THM, THM/DAY, 
    VolumeCalcRuleLabel.DailyPower)
  val ICE_BRENT = new FuturesMarket(
    "ICE Brent", "ICE", 
     USD/BBL, BBL, 
     VolumeCalcRuleLabel.Default, conversions = conv)

  val RBOB = new FuturesMarket(
    "RBOB", "NYM", 
    US_CENT / GAL, GAL,
    VolumeCalcRuleLabel.Default, conversions = conv)

  val PORK = new FuturesMarket(
    "PORK BELLIES", "NYM", 
    USD / MT, MT,
    VolumeCalcRuleLabel.Default, conversions = conv)

  val SING_GO = new SpotMarket("Singapore Gasoil 0.05", "Platts Asia", USD/MT, MT, VolumeCalcRuleLabel.Default, conv)

  val NYMEX_WTI_1st_MONTH =
    FuturesFrontPeriodIndex(FuturesFrontPeriodIndexLabel("Nymex WTI", 1, 0), NYMEX_WTI, Level.Close)

  val Default = new Markets(
    Vector(NYMEX_WTI, ICE_NBP, ICE_BRENT, RBOB, PORK).map {
      m => m.name -> m
    }.toMap,
    Vector(SING_GO).map {
      m => m.name -> m
    }.toMap,
  Map(
    USD -> Currency(USD, "USD", 2, "USD", spotDayUsesBothCalendars = false),
    GBP -> Currency(GBP, "GBP", 2, "GBP", spotDayUsesBothCalendars = false),
    EUR -> Currency(EUR, "EUR", 2, "EUR", spotDayUsesBothCalendars = false),
    TRY -> Currency(TRY, "TRY", 1, "TRY", spotDayUsesBothCalendars = false),
    MXN -> Currency(MXN, "MXN", 2, "MXN", spotDayUsesBothCalendars = true)
  )

  )
}
