package evcel.referencedata.market

import evcel.daterange.Day
import evcel.quantity.UOM
import evcel.referencedata.{Level, ReferenceData}
import evcel.utils.EitherUtils._
import evcel.utils.{EvcelFail, GeneralEvcelFail}

trait Index {
  def priceUOM: UOM
  def label: IndexLabel
  def level: Level

  def observable(refData: ReferenceData, observationDay: Day): Either[EvcelFail, Observable]

  override def toString: String = label.indexName
}

/**
 * Not all indexes are valid when looking up a fixing. Indexes that implement this are.
 *
 * For example a futures front period index is not a valid observable, we use futures contract
 * indexes to look up fixed futures prices.
 */
trait Observable extends Index

object Index {
  def apply(refData: ReferenceData, indexLabel: IndexLabel, level: Level): Either[EvcelFail, Index] = {
    indexLabel match {
      case s: SpotMarketIndexLabel => for (sm <- refData.markets.spotMarket(indexLabel.indexName)) yield
        SpotIndex(sm)
      case f: FuturesContractIndexLabel => for (fm <- refData.markets.futuresMarket(f.marketName)) yield
        FuturesContractIndex(f, fm, level)
      case f: FuturesFrontPeriodIndexLabel => for (fm <- refData.markets.futuresMarket(f.marketName)) yield
        FuturesFrontPeriodIndex(f, fm, level)
      case o => Left(GeneralEvcelFail(s"Failed to convert $o to Index"))
    }
  }
}

case class FuturesFrontPeriodIndex(label: FuturesFrontPeriodIndexLabel, market: FuturesMarket, level: Level)
  extends Index {
  override def priceUOM: UOM = market.priceUOM

  def rollEarlyDays = label.rollEarlyDays
  def nearby = label.nearby

  override def observable(refData: ReferenceData, observationDay: Day): Either[EvcelFail, FuturesContractIndex] = {
    refData.futuresExpiryRule(market.name).flatMap {
      expRule =>
        refData.calendar(market.name).flatMap {
          cal =>
            val containingMonth = observationDay.containingMonth
            val months = containingMonth.to(containingMonth + 20).view
            val month = months.find(month => expRule.futureExpiryDay(month) match {
              case Left(_) => true // failed to get expiry day, so stop here
              case Right(day) =>
                val adjustedLTD = cal.addBusinessDays(day, -label.rollEarlyDays)
                adjustedLTD >= observationDay
            }).map(_  + (label.nearby - 1))

            month.toRight(GeneralEvcelFail(s"Failed to convert $this to FuturesContractIndex")).flatMap {
              month => expRule.futureExpiryDay(month).map{
                _ => FuturesContractIndex(FuturesContractIndexLabel(market.name, month), market, level)
              }
            }
        }
    }
  }
}

case class FuturesContractIndex(label: FuturesContractIndexLabel, market: FuturesMarket, level: Level)
  extends Index with Observable {
  override def priceUOM: UOM = market.priceUOM

  override def observable(refData: ReferenceData, observationDay: Day): Either[EvcelFail, Observable] = Right(this)
}

case class SpotIndex(market: SpotMarket) extends Index with Observable {
  override def priceUOM: UOM = market.priceUOM

  override def label: IndexLabel = SpotMarketIndexLabel(market.name)

  override def level = market.level

  override def observable(refData: ReferenceData, observationDay: Day): Either[EvcelFail, Observable] = Right(this)
}

case class IndexSpread(spread: IndexLabelSpread, level1: Level, level2: Level)