package evcel.referencedata.market

import evcel.daterange.{DateRange, Day}
import scala.util.{Either, Left}
import evcel.utils.EvcelFail
import evcel.quantity.Qty
import evcel.quantity.UOM.DAY
/**
  * Needed to calculate the actual volume for futures contracts
  * that don't specify it directly. E.g. NBP futures - which are traded
  * in Thm/Day  - or power futures, which are traded in MW
  */
trait VolumeCalcRule{
  def volume(quotedVolume : Qty, period : DateRange) : Qty
  /**
    * For grouping hedges within (e.g.) a month. If volume is
    * power rather than actual amount then we need to average
    */
  def combineHedgeVolumes(volumes : Seq[Qty]) : Qty

  def scaleHedgeAmountFromDaily(newPeriod : DateRange) : Double
}

case class VolumeCalcRuleLabel(name : String)

object VolumeCalcRuleLabel{
  val DailyPower = VolumeCalcRuleLabel("DailyPower")
  val Default = VolumeCalcRuleLabel("Default")
}

case object DailyPowerVolumeCalcRule extends VolumeCalcRule{
  def volume(quotedVolume : Qty, period : DateRange) = quotedVolume * Qty(period.size, DAY)
  def combineHedgeVolumes(volumes : Seq[Qty]) : Qty = Qty.average(volumes)
  def scaleHedgeAmountFromDaily(newPeriod : DateRange) : Double = 1.0 / newPeriod.size
}

case object DefaultVolumeCalcRule extends VolumeCalcRule{
  def volume(quotedVolume : Qty, period : DateRange) = quotedVolume
  def combineHedgeVolumes(volumes : Seq[Qty]) : Qty = Qty.sum(volumes)
  def scaleHedgeAmountFromDaily(newPeriod : DateRange) : Double = 1.0 
}
