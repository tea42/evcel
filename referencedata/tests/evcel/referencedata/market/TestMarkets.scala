package evcel.referencedata.market

import evcel.quantity.QtyConversions
import evcel.quantity.UOM._

// should read from main refdata for markets
object TestMarkets {
  val conv = new QtyConversions(Map((MT -> BBL) -> 7.5))

  val Default = new Markets(Map(
    "Nymex WTI" -> new FuturesMarket("Nymex WTI", "NYM", USD/BBL, Some(conv))
  ),
    Map(
      "Singapore Gasoil 0.05" -> new SpotMarket("Singapore Gasoil 0.05", "Platts Asia", USD/MT, Some(conv))
  ))
}
