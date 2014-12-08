package evcel.instrument

/**
  * The thing traded in a trade. Commonly will be a single instrument
  */
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
  def samples : Seq[Tradeable]
}

object TradeableType{
  val types = Vector[TradeableType](
    Future, FuturesOption, 
    CommoditySwap, CommoditySwapSpread, CommoditySwapLookalike,
    FXForward,
    Cash
  )
}

