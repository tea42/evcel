package evcel.curve.markets

import evcel.quantity.UOM._

// should read from main refdata for markets
object TestMarkets {
  val Default = new Markets(Map(
    "Nymex WTI" -> new FuturesMarket("Nymex WTI", "NYM", USD/BBL)
  ),
    Map.empty
  )
}
