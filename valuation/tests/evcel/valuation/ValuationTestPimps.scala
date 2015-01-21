package evcel.valuation

import evcel.instrument.{Index, Instrument, IndexSpread}
import evcel.referencedata.ReferenceData
import evcel.daterange.{Month, Day}
import scala.language.implicitConversions
import evcel.curve.environment.PriceIdentifier
import evcel.curve.ValuationContext
import evcel.referencedata.market.{FXPair, SpotMarket}
import evcel.quantity.UOM
import evcel.utils.EitherTestPimps

trait ValuationTestPimps extends EitherTestPimps{

  implicit class RichValuerInstrument(instr: Instrument) {
    def mtm(vc: ValuationContext)(implicit valuer: Valuer) = valuer.value(vc, instr).R
    def positions(vc: ValuationContext)(implicit valuer : Valuer) = valuer.positions(vc, instr).R
  }

}
