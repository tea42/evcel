package evcel.referencedata

import evcel.daterange.DateRangeSugar.Sep
import scala.language.reflectiveCalls

object TestFuturesExpiryRules {
  val Test = new FuturesExpiryRules(
    Map("Nymex WTI" ->
      FuturesExpiryRule("Nymex WTI",
        (Sep / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 9)}.toMap,
        (Sep / 2014 to Sep / 2015).map { m => m -> (m.firstDay - 10)}.toMap
      )
    )
  )
}
