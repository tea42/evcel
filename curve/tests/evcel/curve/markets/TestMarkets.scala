package evcel.curve.markets

// should read from main refdata for markets
object TestMarkets {
  val Default = new Markets(Map(
    "Nymex WTI" -> new FuturesMarket("Nymex WTI", "NYM")
  ),
    Map.empty
  )
}
