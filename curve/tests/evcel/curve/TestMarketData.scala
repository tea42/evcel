package evcel.curve

import evcel.curve.curves.PriceFixingIdentifier
import evcel.curve.environment.{MarketDay, TimeOfDay}
import evcel.curve.marketdata.{FuturesPriceData, PriceFixingData}
import evcel.daterange.{Day, Month}
import evcel.quantity.Qty
import evcel.referencedata.market._
import evcel.referencedata.{Level, ReferenceData}
import evcel.utils.EitherTestPimps

import scala.io.Source

object TestMarketData extends EitherTestPimps {
  lazy val fixingsSource = Source.fromURL(getClass.getResource("/evcel/curve/fixings.csv")).getLines().toList
  lazy val forwardSource = Source.fromURL(getClass.getResource("/evcel/curve/forwardprices.csv")).getLines().toList

  def valuationContext(refData: ReferenceData = UnitTestingEnvironment.testRefData,
                       md:MarketDay = MarketDay(Day(2014, 12, 31), TimeOfDay.end)) = {
    val fixingsData = fixingsSource.map{
      line =>
        val indexStr :: levelStr :: dayStr :: price :: Nil = line.split('\t').toList

        val label = IndexLabel.parse(indexStr)
        val level = Level.fromNameOrThrow(levelStr)
        val ndx = Index.apply(refData, label, level).right.getOrElse(sys.error(s"index: $label/$level"))
        val day = Day.fromISO(dayStr).getOrElse(sys.error(s"day: $dayStr"))
        val fci = RichIndex(refData, ndx).R.observable(refData, day).R
        PriceFixingIdentifier(fci, day) -> PriceFixingData(Qty(price, ndx.priceUOM))
    }.toList

    val forwardPriceData = forwardSource.map{
      line =>
        val market :: month :: price :: Nil = line.split('\t').toList
        val mkt = refData.futuresMarket(market).right.getOrElse(sys.error(s"market: $market"))
        (mkt, Month.parse(month), Qty(price, mkt.priceUOM))
    }.toList.groupBy(_._1).map{
      case (k, l) => k.name -> FuturesPriceData(l.map(e => (e._2, e._3)))
    }.toList

    UnitTestingEnvironment.fromMarketData(md, fixingsData:::forwardPriceData :_*)
  }
}
