package evcel.instrument

trait Tradeable{
  def instruments : Seq[Instrument]
  def tradeableType : TradeableType
  @transient lazy val name : String = tradeableType.name
}

trait SingleInstrumentTradeable extends Tradeable with Instrument{
  @transient lazy val instruments = Vector(this)
}

trait TradeableType{
  def name : String
}

