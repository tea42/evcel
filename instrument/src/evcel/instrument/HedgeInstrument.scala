package evcel.instrument

import evcel.daterange.PeriodLabel
import evcel.quantity.Qty
import java.util.concurrent.ConcurrentHashMap

/**
  * In general, instruments don't have a single volume. E.g. market spreads between MT/BBl volumes of 
  * different commodities. However the standard instruments used to hedge portfolios will need 
  * a volume of some sort that is used to describe the position.
  */
trait HedgeInstrument extends Instrument{
  def riskMarketLabel : String // For pivot report
  def riskPeriod : PeriodLabel
  def volume: Qty
}

object HedgeInstrument{
  private val internedHedgeCache = new ConcurrentHashMap[HedgeInstrument, HedgeInstrument]()
  def intern(inst : HedgeInstrument) : HedgeInstrument = {
    var interned = internedHedgeCache.get(inst)
    if (interned == null){
      internedHedgeCache.putIfAbsent(inst, inst)
      interned = internedHedgeCache.get(inst)
    }
    interned
  }
}

