package evcel.curve

import evcel.curve.environment.{ AtomicDatumIdentifier, AtomicEnvironment, MarketDay }
import evcel.daterange.Day

object UnitTestingEnvironment {
  def apply(marketDay_ : MarketDay, data: PartialFunction[AtomicDatumIdentifier, Any]): Environment = {
    new Environment(
      new AtomicEnvironment {
        def marketDay = marketDay_
        def apply(point: AtomicDatumIdentifier): Any = {
          data(point)
        }
      }
    )
  }
}
