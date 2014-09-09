package evcel.instrument.valuation

import evcel.curve.marketdata.Act365
import evcel.instrument.{ EuropeanOption, FuturesOption }
import evcel.curve.ValuationContext
import evcel.maths.models.BlackScholes
import evcel.quantity.{Qty, UOM}

case class OptionOnFutureValuer(o: FuturesOption) {
  def value(vc: ValuationContext, ccy: UOM) = {
    require(o.optionType == EuropeanOption, "Only EuropeanOption supported")
    require(ccy == o.strike.uom.numerator, "No fx yet: " + (ccy, o.strike))

    val F = vc.futuresPrice(o.market, o.delivery)

    val expiryDay = vc.optionExpiryDay(o.market, o.delivery).getOrElse(
      sys.error("No expiry for " + (o.market, o.delivery))
    )
    val T = Act365.timeBetween(vc.marketDay.day, expiryDay)

    val calendar = vc.futuresCalendar(o.market).getOrElse(sys.error("No calendar for " + o.market))
    val disc = vc.discountRate(ccy, calendar.addBusinessDays(expiryDay, o.bizDaysAfterExpiryToSettlement))
    val vol = vc.futuresVol(o.market, o.delivery, o.strike)
    val value = new BlackScholes(
      o.right, F.checkedDouble(o.strike.uom), o.strike.checkedDouble(o.strike.uom), vol.checkedPercent, T
    ).undiscountedValue * disc

    Qty(value, o.strike.uom) * o.volume
  }
}
