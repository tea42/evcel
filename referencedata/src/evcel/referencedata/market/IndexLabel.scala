package evcel.referencedata.market

import evcel.daterange.Month
import evcel.referencedata.ReferenceDataIdentifier

trait IndexLabel extends ReferenceDataIdentifier {
  def indexName: String
  override def toString = indexName
}

object IndexLabel {
  val SPOT = "Spot"
  val FUTURES_FRONT_PERIOD = "Futures Front Period"
  val FUTURES_CONTRACT = "Futures Contract"

  def parse(name : String) : IndexLabel = {
    name match {
      case FuturesContractIndexLabel.Parse(market, yy, mm) =>
        FuturesContractIndexLabel(market, Month(yy.toInt, mm.toInt))
      case _ => FuturesFrontPeriodIndexLabel.parse(name) match {
        case Some(ffpi) => ffpi
        case _ => SpotMarketIndexLabel(name)
      }
    }
  }

}


trait FuturesDerivedIndexLabel extends IndexLabel {
  def underlyingMarketName: String

}

case class FuturesContractIndexLabel(marketName: String, month: Month) extends FuturesDerivedIndexLabel {

  override def underlyingMarketName = marketName

  def indexName = s"$marketName ($month)"
}

object FuturesContractIndexLabel{
  val Parse = ("""(.*) \(""" + Month.Format + """+\)""").r
}

case class SpotMarketIndexLabel(marketName: String) extends IndexLabel {
  def indexName = marketName
}

case class FuturesFrontPeriodIndexLabel(marketName: String, nearby: Int, rollEarlyDays: Int) extends FuturesDerivedIndexLabel {
  require(nearby > 0, "nearby: " + nearby)
  require(rollEarlyDays >= 0, "rollEarlyDays: " + rollEarlyDays)

  def indexName = {
    val nearbyString = " nearby " + nearby
    val rollString = if (rollEarlyDays == 0) "" else " roll " + rollEarlyDays
    marketName + nearbyString + rollString
  }

  override def underlyingMarketName = marketName
}

object FuturesFrontPeriodIndexLabel{
  val Parse = """(.+) nearby ([0-9]+)[ ]?(roll )?([0-9]?)""".r

  def parse(name: String) = name match {
    case Parse(market, nearby, _, roll) =>
      val nearbyNum = if (nearby.isEmpty) 1 else nearby.toInt
      val rollNum = if (roll.isEmpty) 0 else roll.toInt
      Some(FuturesFrontPeriodIndexLabel(market, nearbyNum, rollNum))
    case _ => None
  }
}

case class IndexLabelSpread(index1: IndexLabel, index2: IndexLabel) {
  def indexName = s"$index1 vs $index2"
  override def toString =  indexName
}


object IndexLabelSpread {
  val Parse = """(.+?) vs (.+)""".r

  def parse(name: String): Option[IndexLabelSpread] = {
    name match {
      case Parse(i1, i2) =>
        Some(IndexLabelSpread(IndexLabel.parse(i1), IndexLabel.parse(i2)))
      case _ => None
    }
  }
}
