package evcel.curve

import evcel.calendar.TestCalendars
import evcel.curve.curves.TestFuturesExpiryRules
import evcel.curve.environment.{AtomicDatumIdentifier, AtomicEnvironment, MarketDay}
import evcel.curve.markets.TestMarkets

object UnitTestingEnvironment {
  def testRefData = ReferenceData(
    TestFuturesExpiryRules.Test,
    TestCalendars.Empty,
    TestMarkets.Default
  )

  def apply(marketDay_ : MarketDay, data: PartialFunction[AtomicDatumIdentifier, Any]): ValuationContext = {
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

  def Null(marketDay: MarketDay) = apply(marketDay, {
    case a => a.nullValue(testRefData)
  })
}
