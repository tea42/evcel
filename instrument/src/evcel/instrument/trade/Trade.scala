package evcel.instrument.trade

import evcel.instrument.Instrument
import evcel.daterange.Day
import evcel.instrument.Tradeable

case class Trade(
  id : String,
  tradeDay : Day,
  counterparty : String,
  tradeable : Tradeable,
  meta : Map[String, String]
) {
  def instruments = tradeable.instruments
}

object Trade{

  def apply(
    id : String,
    tradeDay : Day,
    counterparty : String,
    tradeable : Tradeable
  ) : Trade = Trade(id, tradeDay, counterparty, tradeable, meta = Map.empty)
}
