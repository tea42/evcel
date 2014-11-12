package evcel.curve.marketdata

import evcel.curve.curves.SpotFX
import evcel.curve.marketdata.MarketData.CantBuildCurve
import evcel.quantity.{UOM, BDQty}

case class SpotFXData(rate: BDQty)
  extends MarketData
{
  def buildCurve(from: UOM, to: UOM): Either[CantBuildCurve, SpotFX] = {
    Right(new SpotFX(from, to, rate))
  }
}
