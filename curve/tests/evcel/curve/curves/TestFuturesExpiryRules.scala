package evcel.curve.curves

import evcel.daterange.DateRangeSugar.{Dec, Oct, Nov}
import scala.language.reflectiveCalls

object TestFuturesExpiryRules {
  val Test = new FuturesExpiryRules(
    Map("Nymex WTI" ->
      FuturesExpiryRule("Nymex WTI",
        Map(
          Nov / 2014 -> 14 / Oct / 2014,
          Dec / 2014 -> 14 / Nov / 2014
        )
      )
    )
  )
}
