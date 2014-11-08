package evcel.instrument

trait Instrument

object Instrument{
  val FUTURE = "Future"
  val FUTURES_OPTION = "Futures Option"
  val COMMODITY_SWAP = "Commodity Swap"
  val COMMODITY_SWAP_LOOKALIKE = "Commodity Swap Lookalike"
  val COMMODITY_SWAP_SPREAD = "Commodity Swap Spread"
}
trait InstrumentType
