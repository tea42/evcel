package evcel.report

import evcel.pivot._
import evcel.instrument.trade.Trade
import java.util.concurrent.ConcurrentHashMap

case class PivotTrade(trade : Trade) extends PivotRow {
  import PivotTrade._
  import trade.{instruments, id, tradeDay, counterparty, tradeable, meta}

  val pivotInstruments : Seq[PivotInstrument] = instruments.map(PivotInstrument(_))
  val instrumentFields = pivotInstruments.flatMap(_.fields).distinct

  // Note that these don't include the values for the tradeable, which may
  // correspond to many pivot rows
  private val tradePivotValues = Map[PivotField, PivotValue](
    ID_FIELD -> ID_FIELD.pivotValue(id),
    TRADE_DAY_FIELD -> TRADE_DAY_FIELD.pivotValue(tradeDay),
    CPTY_FIELD -> CPTY_FIELD.pivotValue(counterparty),
    TRADE_TYPE_FIELD -> TRADE_TYPE_FIELD.pivotValue(tradeable.name)
  ) ++ meta.map{
    case (key, value) => 
      val field = PivotTrade.metaField(key)
      field -> field.pivotValue(value.toString)
  } 
  def pivotValue(field : PivotField) : PivotValue = tradePivotValues.getOrElse(field, NullPivotValue)
  val fields = tradePivotValues.keys.toVector

  val pivotTable = {
    PivotTable.addSingleRow(
      this,
      PivotTable(instrumentFields, pivotInstruments)
    )
  }
}

object PivotTrade{
  val ID_FIELD = StringPivotField("Trade ID")
  val TRADE_DAY_FIELD = DayField("Trade Day")
  val CPTY_FIELD = StringPivotField("Counterparty")
  val TRADE_TYPE_FIELD = StringPivotField("Trade Type")

  val STANDARD_FIELDS = List(ID_FIELD, TRADE_DAY_FIELD, CPTY_FIELD, TRADE_TYPE_FIELD)
  private val metaFields = new ConcurrentHashMap[String, TradeMetaField]()
  def metaField(label : String) : TradeMetaField = {
    if (! metaFields.containsKey(label))
      metaFields.putIfAbsent(label, TradeMetaField(label))
    metaFields.get(label)
  }
  def isTradeField(field : PivotField) = STANDARD_FIELDS.contains(field) || field.isInstanceOf[TradeMetaField]
}
