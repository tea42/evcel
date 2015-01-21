package evcel.instrument

import evcel.daterange.{DateRange, DateRangePeriodLabel}
import evcel.quantity.{BDQty, Qty}
import evcel.daterange.DateRangeSugar._
import evcel.quantity.Qty._
import evcel.quantity.UOM._

case class CommoditySwap(
  index : Index, averagingPeriod: DateRange, strike: BDQty, volume: BDQty,
  bizDaysToSettlement: Option[Int] = None)
  extends SingleInstrumentTradeable with HedgeInstrument {
  import CommoditySwap._

  def tradeableType = CommoditySwap

  def riskMarketLabel = index.indexName
  def riskPeriod = DateRangePeriodLabel(averagingPeriod)

  def isCleared = bizDaysToSettlement.isEmpty
  def instrumentType = CommoditySwap
}

object CommoditySwap extends TradeableType with InstrumentType{
  val defaultDaysToSettlement = 5
  def name = "Commodity Swap"
    
  def samples = Vector(
    CommoditySwap(
      FuturesFrontPeriodIndex("Nymex WTI", nearby = 1, rollEarlyDays = 0),
      Jun / 2014,
      100(USD/BBL),
      123(BBL)
    ),
    CommoditySwap(
      FuturesFrontPeriodIndex("Nymex WTI", nearby = 1, rollEarlyDays = 0),
      Jun / 2014,
      100(USD/BBL),
      123(BBL),
      bizDaysToSettlement = Some(10)
    )
  )

}



