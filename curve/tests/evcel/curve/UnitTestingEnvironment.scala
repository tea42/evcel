package evcel.curve

import evcel.calendar.TestCalendars
import evcel.curve.curves.TestFuturesExpiryRules
import evcel.curve.environment.{AtomicDatumIdentifier, AtomicEnvironment, MarketDay}
import evcel.curve.markets.TestMarkets
import evcel.daterange.Day

object UnitTestingEnvironment {
  def apply(marketDay_ : MarketDay, data: PartialFunction[AtomicDatumIdentifier, Any]): ValuationContext = {
    val testRefData = ReferenceData(
      TestFuturesExpiryRules.Test,
      TestCalendars.Empty,
      TestMarkets.Default
    )
    new ValuationContext(
      new AtomicEnvironment {
        def marketDay = marketDay_

        def apply(point: AtomicDatumIdentifier): Any = {
          data(point)
        }
      },
      testRefData,
      EnvironmentParams.Default
    )
  }
}
