package evcel.referencedata

import evcel.daterange.DateRangeSugar.{Jan, Sep}
import scala.language.reflectiveCalls

object TestFuturesExpiryRules {
  val Test = new FuturesExpiryRules(
    Map("Nymex WTI" ->
      FuturesExpiryRule("Nymex WTI",
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
      ),
     "ICE NBP" ->
      FuturesExpiryRule("ICE NBP",
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
      ),
     "ICE Brent" ->
      FuturesExpiryRule("ICE Brent",
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
        (Jan / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
      )
    )
  )
}
