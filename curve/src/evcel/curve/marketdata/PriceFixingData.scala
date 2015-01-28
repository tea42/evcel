package evcel.curve.marketdata

import evcel.curve.curves.PriceFixing
import evcel.curve.marketdata.MarketData.CantBuildCurve
import evcel.quantity.BDQty

case class PriceFixingData(fixing: BDQty) extends MarketData {
  def buildCurve: Either[CantBuildCurve, PriceFixing] = {
    Right(PriceFixing(fixing))
  }
}
