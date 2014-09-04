package evcel.curve.environment
import evcel.curve.curves.FuturesExpiryRule

trait AtomicEnvironment {
  def marketDay: MarketDay
  def apply(id: AtomicDatumIdentifier): Any
}
