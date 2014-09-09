package evcel.curve.environment

trait AtomicEnvironment {
  def marketDay: MarketDay
  def apply(id: AtomicDatumIdentifier): Any
}
