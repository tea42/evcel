package evcel.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{PriceIdentifier, AtomicDatumIdentifier}
import evcel.instrument._
import evcel.quantity.{Qty, BDQty, DblQty}
import evcel.referencedata.market.FXPair
import scala.util.{Either, Right}
import evcel.utils.{EvcelFail, GeneralEvcelFail}
import evcel.utils.EitherUtils._
import evcel.curve.curves.DiscountRateIdentifier
import evcel.quantity.Qty._
import evcel.daterange.{Day, DateRangeSugar}
import evcel.referencedata.calendar.Calendar
import evcel.maths.EuropeanOption
import evcel.curve.marketdata.Act365
import evcel.maths.models.BlackScholes
import evcel.daterange.DateRangeSugar._

trait Valuer {
  def value(vc: ValuationContext, instr: Instrument): Either[EvcelFail, Qty]
  def keys(vc: ValuationContext, instr: Instrument): Set[AtomicDatumIdentifier] = {
    val (recordingVC, record) = vc.keyRecordingVC
    value(recordingVC, instr)
    record.keys
  }

  def positions(
    vc: ValuationContext, 
    instr: Instrument
  ): Either[EvcelFail, Map[HedgeInstrument, Either[EvcelFail, Double]]] = {
    HedgingStrategy(vc).hedgingInstruments(instr).map{
      hedgingInstruments => 
        SVDPositions.positions(vc, this, instr, hedgingInstruments)
    }
  }

  def priceKeys(vc: ValuationContext, instr: Instrument): Set[PriceIdentifier] = {
    keys(vc, instr).flatMap{case pi: PriceIdentifier => Some(pi); case _ => None}
  }

  def firstOrderPriceDiff(vc: ValuationContext, instr: Instrument, pi: PriceIdentifier): Either[EvcelFail, Qty] = {
    val dP = pi.dP(vc)
    for {
      mtmUp <- value(vc.shiftPrice(pi, dP), instr)
      mtmDown <- value(vc.shiftPrice(pi, -dP), instr)
    } yield {
      (mtmUp - mtmDown) / (2 * dP)
    }
  }
}

object Valuer {
  implicit class RichValuerInstrument(instr: Instrument) {
    def mtm(vc: ValuationContext)(implicit valuer: Valuer) = valuer.value(vc, instr)
    def keys(vc: ValuationContext)(implicit valuer: Valuer) = valuer.keys(vc, instr)
    def priceKeys(vc: ValuationContext)(implicit valuer: Valuer) = valuer.priceKeys(vc, instr)

    def firstOrderPriceDiff(vc: ValuationContext, pi: PriceIdentifier)(implicit valuer: Valuer) = {
      valuer.firstOrderPriceDiff(vc, instr, pi)
    }
    def positions(vc: ValuationContext)(implicit valuer : Valuer) = valuer.positions(vc, instr)
  }
}

class DefaultValuer extends Valuer{
  def value(vc : ValuationContext, instr : Instrument) : Either[EvcelFail, Qty] = {
    def settlementDiscount(
      calendar : Calendar, 
      bizDaysToSettlement : Option[Int], 
      day : Day
    ) : Either[EvcelFail, Qty] = {
      val d : Either[EvcelFail, Qty] = bizDaysToSettlement match {
        case None => Right(Qty.bdOne)
        case Some(n) => 
          val settlementDay = calendar.addBusinessDays(day, n)
          vc.discountRate(vc.valuationCcy, settlementDay)
      }
      d
    }

    val mtm : Either[EvcelFail, Qty] = instr match {
      case Future(marketLabel, period, strike, quotedVolume) => 
        for {
          market <- RichFuturesMarket(vc.refData, marketLabel)
          price <- vc.futuresPrice(market.market, period)
        } yield {
          val volume = market.volumeCalcRule.volume(quotedVolume, period)
          (price - strike) * volume
        }

      case CommoditySwap(index, averagingPeriod, strike, quotedVolume, bizDaysToSettlement) => 
        for {
          richIndex <- RichIndex(vc.refData, index) 
          discount <- settlementDiscount(richIndex.calendar, bizDaysToSettlement, averagingPeriod.lastDay)
          avePrice <- richIndex.averagePrice(vc, richIndex.observationDays(averagingPeriod))
        } yield {
          val volume = richIndex.volumeCalcRule.volume(quotedVolume, averagingPeriod)
          (avePrice - strike) * volume * discount
        }

      case CommoditySwapSpread(indexSpread, averagingPeriod, strike, volume, pricingRule, bizDaysToSettlement) => 
        for{
          richIndexSpread <- RichIndexSpread(vc.refData, indexSpread)
          discount <- settlementDiscount(richIndexSpread.commonCalendar, bizDaysToSettlement, averagingPeriod.lastDay)
          spreadPrice <- richIndexSpread.spreadPrice(vc, pricingRule, averagingPeriod, strike.uom)
        } yield {
          (spreadPrice - strike) * volume * discount
        }

      case CommoditySwapLookalike(marketLabel, month, strike, quotedVolume, bizDaysToSettlement) => 
        for {
          market <- RichFuturesMarket(vc.refData, marketLabel)
          ltd <- market.expiryRule.futureExpiryDay(month)
          discount <- settlementDiscount(market.calendar, bizDaysToSettlement, ltd)
          price <- vc.futuresPrice(market.market, month)
        } yield {
          val volume = market.volumeCalcRule.volume(quotedVolume, ltd)
          (price - strike) * volume * discount
        }

      case FuturesOption(
        marketLabel, month, strike, quotedVolume, right,
        `EuropeanOption`, isCashSettled, bizDaysAfterExpiryToSettlement, customExpiry
      ) => {
        for {
          market <- RichFuturesMarket(vc.refData, marketLabel)
          F <- vc.futuresPrice(market.market, month)
          vol <- vc.futuresVol(market.market, month, strike)
          expiryDay <- customExpiry match {
            case Some(d) => Right(d)
            case None => market.optionExpiryDay(month)
          }
          time = Act365.timeBetween(vc.marketDay.day, expiryDay)
          discount <- settlementDiscount(market.calendar, Some(bizDaysAfterExpiryToSettlement), expiryDay)
        } yield {
          val value = new BlackScholes(
            right, F.checkedDouble(strike.uom), strike.checkedDouble(strike.uom), vol.checkedPercent,time 
          ).undiscountedValue * discount.doubleValue

          val volume = market.volumeCalcRule.volume(quotedVolume, month)
          Qty(value, strike.uom) * volume
        }
      }


      case FXForward(volume, strike, delivery) => {
        val strike_ = if(strike.uom.numerator == volume.uom)
          strike.invert
        else
          strike

        val paying = new Cash(-volume mult strike_, Some(delivery))
        val receiving = new Cash(volume, Some(delivery))

        for {
          payValue <- value(vc, paying)
          recValue <- value(vc, receiving)
        } yield {
          payValue + recValue
        }
      }


      case Cash(volume, delivery) => {
        val discount : Either[EvcelFail, Qty] = delivery match {
          case None => 
            Right(Qty.bdOne)
          case Some(d) => 
            vc.discountRate(volume.uom, d)
        }
        discount.map{d => volume * d}
      }
        
      case _ => sys.error(s"Instrument $instr has no valuation method")
    }
    mtm.flatMap(_.inBaseCcy).flatMap{
      mtm => 
        if(mtm.uom != vc.valuationCcy) {
          for {rate <- vc.todayFX(FXPair(mtm.uom, vc.valuationCcy))} yield mtm * rate
        } else {
          Right(mtm)
        }
    }
  }
}
