package evcel.valuation

import evcel.referencedata.Level
import evcel.referencedata.market.{IndexLabelSpread, FuturesDerivedIndexLabel}
import evcel.utils.{EvcelFail, EitherUtils}
import evcel.instrument._
import evcel.curve._
import evcel.daterange.{DateRange, Day, Month}
import evcel.utils.EitherUtils._
import scala.util.{Either, Left, Right}

trait HedgingStrategy{
  def vc : ValuationContext
  def hedgingInstruments(inst : Instrument) : Either[EvcelFail, Seq[HedgeInstrument]] 

  protected def hedgeWithFuture(marketLabel : String, month : Month) = 
    RichFuturesMarket(vc.refData, marketLabel).map{mkt => Vector(mkt.unitHedge(month))}

  /**
    * Approximates the spread with a pair of swaps and returns the combined hedging instruments
    * for those. Non-common pricing rule will be exact, common won't necessarily be.
    */
  protected def hedgeSwapSpreadAsSingleSwaps(cs : CommoditySwapSpread) : Either[EvcelFail, Seq[HedgeInstrument]] = {
    for {
      richIndex1 <- RichIndex(vc.refData, cs.indexSpread.index1, cs.index1Level)
      hedges1 <- hedgingInstruments(richIndex1.unitHedge(cs.averagingPeriod))
      richIndex2 <- RichIndex(vc.refData, cs.indexSpread.index2, cs.index2Level)
      hedges2 <- hedgingInstruments(richIndex2.unitHedge(cs.averagingPeriod))
    } yield
      hedges1 ++ hedges2
  }

  protected def hedgeLookalikeAsSwap(csl : CommoditySwapLookalike) = {
    csl.asCommoditySwap(vc.refData).flatMap(hedgingInstruments(_))
  }

  protected def hedgeSwapWithTenorAppropriateSwaps(cs : CommoditySwap) = {
    val averagingPeriod = cs.averagingPeriod
    for (richIndex <- RichIndex(vc.refData, cs.index, cs.level))
      yield {
        vc.params.tenor match {
          case Some(Month) | Some(Day) => 
            richIndex.observationDays(averagingPeriod).map(richIndex.unitHedge(_))
          case None =>
            Vector(richIndex.unitHedge(averagingPeriod))
          case Some(other) =>
            sys.error(s"Haven't coded for tenor $other")
        }
      }
  }

  protected def hedgeSwapWithFutures(index : FuturesDerivedIndexLabel, averagingPeriod : DateRange) = {
    for {
      futuresMarket <- vc.refData.futuresMarket(index.underlyingMarketName)
      richIndex <- RichFuturesBasedIndex(vc.refData, index, futuresMarket.level)
      months <- EitherUtils.mapOrErrorLeft(
          richIndex.observationDays(averagingPeriod),
          {day : Day => richIndex.observedMonth(day)})
    } yield {
      months.distinct.map(richIndex.market.unitHedge)
    }
  }
}

object HedgingStrategy{
  def apply(vc : ValuationContext) = {
    if (vc.params.showEqFutures)
      EquivalentFuturesHedgingStrategy(vc)
    else 
      DefaultHedgingStrategy(vc)
  }
}

case class EquivalentFuturesHedgingStrategy(vc : ValuationContext) extends HedgingStrategy{
  def hedgingInstruments(inst : Instrument) : Either[EvcelFail, Seq[HedgeInstrument]] = {
    inst match {
      case f : Future => 
        hedgeWithFuture(f.market, f.period)

      case fo : FuturesOption => 
        hedgeWithFuture(fo.market, fo.period)

      case cs @ CommoditySwap(index : FuturesDerivedIndexLabel, averagingPeriod, _, _, _, _) =>
        hedgeSwapWithFutures(index, averagingPeriod)

      case cs : CommoditySwap =>
        hedgeSwapWithTenorAppropriateSwaps(cs)

      case csl : CommoditySwapLookalike => 
        hedgeLookalikeAsSwap(csl)

      case cs : CommoditySwapSpread => 
        hedgeSwapSpreadAsSingleSwaps(cs)

      case _ : Cash | _ : FXForward => // Not implemented yet
        Right(Vector())
    }
  }
}

case class DefaultHedgingStrategy(vc : ValuationContext) extends HedgingStrategy{

  def hedgingInstruments(inst : Instrument) : Either[EvcelFail, Seq[HedgeInstrument]] = {

    inst match {
      case f : Future => 
        hedgeWithFuture(f.market, f.period)

      case fo : FuturesOption => 
        hedgeWithFuture(fo.market, fo.period)

      case cs : CommoditySwap => 
        hedgeSwapWithTenorAppropriateSwaps(cs)

      case csl : CommoditySwapLookalike => 
        hedgeLookalikeAsSwap(csl)

      case cs : CommoditySwapSpread => 
        hedgeSwapSpreadAsSingleSwaps(cs)

      case _ : Cash | _ : FXForward => // Not implemented yet
        Right(Vector())
    }
  }

}
