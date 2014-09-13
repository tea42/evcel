package evcel.curve.markets

import evcel.quantity.UOM._

// should read from main refdata for markets
object TestMarkets {
  val Default = new Markets(Map(
    "Nymex WTI" -> new FuturesMarket("Nymex WTI", "NYM", USD/BBL)
  ),
    Map(
      "Singapore Gasoil 0.05" -> new SpotMarket("Singapore Gasoil 0.05", "Platts Asia", USD/MT)
  ))
}
