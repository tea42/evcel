package evcel.referencedata.market

import evcel.quantity.QtyConversions
import evcel.quantity.UOM._
import evcel.quantity.QtyConversions
import evcel.quantity.UOM._

// should read from main refdata for markets
object TestMarkets {
  val conv = new QtyConversions(Map((MT -> BBL) -> 7.5))
  val NYMEX_WTI = new FuturesMarket("Nymex WTI", "NYM", USD/BBL, Some(conv))
  val ICE_NBP   = new FuturesMarket("ICE NBP", "NBP", PENCE/THM, conversions = None, perTimeUnit = Some(DAY))
  val ICE_BRENT = new FuturesMarket("ICE Brent", "ICE", USD/BBL, Some(conv))


  val Default = new Markets(
    Vector(NYMEX_WTI, ICE_NBP, ICE_BRENT).map{
      m => m.name -> m
    }.toMap,
    Map(
      "Singapore Gasoil 0.05" -> new SpotMarket("Singapore Gasoil 0.05", "Platts Asia", USD/MT, Some(conv))
  ),
  Map(
    USD -> Currency(USD, "USD", 2, "USD", spotDayUsesBothCalendars = false),
    GBP -> Currency(GBP, "GBP", 2, "GBP", spotDayUsesBothCalendars = false),
    EUR -> Currency(EUR, "EUR", 2, "EUR", spotDayUsesBothCalendars = false),
    TRY -> Currency(TRY, "TRY", 1, "TRY", spotDayUsesBothCalendars = false),
    MXN -> Currency(MXN, "MXN", 2, "MXN", spotDayUsesBothCalendars = true)
  )

  )
}
