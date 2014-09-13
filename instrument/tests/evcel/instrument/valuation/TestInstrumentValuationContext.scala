package evcel.instrument.valuation

object TestInstrumentValuationContext {
  def Test = InstrumentValuationContext(new DefaultValuer)
}
