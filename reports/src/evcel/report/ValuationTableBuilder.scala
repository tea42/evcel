package evcel.report

import evcel.curve.ValuationContext
import evcel.instrument.Instrument
import evcel.instrument.trade.Trade
import evcel.instrument.valuation.HedgeInfo
import evcel.instrument.valuation.Valuer
import evcel.instrument.valuation.Valuer._
import evcel.pivot._
import evcel.quantity.Qty

/**
  * Builds a pivot table of trade, instrument and valuation data.
  * The fields passed in may be any combination of trade, instrument
  * and valuation fields (currently just MTM and positon).
  *
  * The valuation context and valuer are optional as these are not required
  * unless there are valuation fields. This code could also be used for a trade page
  * report.
  *
  * The algorithm to build the table is 
  *
  * 1 Split the trades into groups according to the values of any trade fields used
  * 2 Split these groups by any instrument fields used
  * 3 Calculate any valuations required.
  *
  * This grouping allows a more memory efficient PivotTable implementation to be used,
  * and also lends itself to SVD position calculations at an appropriate level.
  */
case class ValuationTableBuilder(
  fields : Seq[PivotField],
  filters : Map[PivotField, PivotValue => Boolean],
  vc : Option[ValuationContext],     
  valuer : Option[Valuer]
){

  private def fieldsAndFilters(predicate : PivotField => Boolean) = {
    (fields.filter(predicate), filters.filterKeys(predicate))
  }

  val (tradeFields, tradeFilters) = fieldsAndFilters(PivotTrade.isTradeField)
  val (instrumentFields, instrumentFilters) = fieldsAndFilters(PivotInstrument.FIELDS.contains)
  val (positionFields, positionFilters) = fieldsAndFilters(PivotValuer.POSITION_FIELDS.contains)

  def build(trades : Seq[Trade]) : PivotTable = {
    buildTable(
      trades.map(PivotTrade(_)), 
      tradeFields, tradeFilters,
      {tradeGroup : Seq[PivotTrade] => buildInstrumentsTable(tradeGroup.flatMap(_.pivotInstruments))}
    )
  }

  private def buildInstrumentsTable(instruments : Seq[PivotInstrument]) : PivotTable = {
    buildTable(
      instruments,
      instrumentFields, instrumentFilters,
      {
        instrumentsGroup : Seq[PivotInstrument] => 
          // TODO - SVD hedging at this point on the group of instruments
          //        rather than hedge calcs for each instrument
          PivotTable.append(instrumentsGroup.map(buildSingleInstrumentValuationTable))
      }
    )
  }


  private def buildSingleInstrumentValuationTable(pivotInstrument : PivotInstrument) : PivotTable = {
    import pivotInstrument.instrument
    import PivotValuer.MTMField
    
    val positionTable = if (positionFields.nonEmpty){
      implicit val valuer_ = valuer.get
      val hedges = instrument.positions(vc.get).map(PivotHedgeInfo(_)).filter(_.satisfies(positionFilters))
      PivotTable(positionFields, hedges)
    } else 
      PivotTable.Null

    if (fields.contains(MTMField)){
      implicit val valuer_ = valuer.get
      val mtm = MTMField.pivotValue(instrument.mtm(vc.get))
      PivotTable.addSingleRow(
        PivotRow(MTMField, mtm),
        positionTable
      )
    } else
      positionTable
  }

    
  private def buildTable[T <: PivotRow](
    entities : Seq[T], 
    fields : Seq[PivotField],
    filters : Map[PivotField, PivotValue => Boolean],
    subTableBuilder : Seq[T] => PivotTable
  ) : PivotTable = {
    val filteredEntities = entities.filter(_.satisfies(filters))
    val groupedEntities = grouped(fields, filteredEntities)
    val rowBuilder = PivotRow(fields)
    val tables : Seq[PivotTable] = groupedEntities.map{
      case (values, entityGroup) => 
        PivotTable.addSingleRow(
          rowBuilder(values),
          subTableBuilder(entityGroup)
        )
    }(scala.collection.breakOut)
    PivotTable.append(tables)
  }

  private def grouped[T <: PivotRow](
    fields : Seq[PivotField],
    hpvs : Seq[T]
  ) : Map[Seq[PivotValue], Seq[T]] = {
    hpvs.groupBy{
      hpv => 
        fields.map(hpv.pivotValue(_))
    }
  }

}

