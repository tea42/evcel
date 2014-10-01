package evcel.instrument.trade
import evcel.instrument.Instrument
import evcel.daterange.Day

case class Trade(
  id : String,
  tradeDay : Day,
  counterparty : String,
  instrument : Instrument,
  meta : Map[String, String]
){
}

