package evcel.eventstore.json

import evcel.curve.marketdata.{ DayCount, FuturesPriceData, FuturesVolData, ZeroRateData }
import evcel.daterange.{ Day, Month }
import evcel.quantity.{ Percent, Qty, UOM, UOMRatio, BDQty }
import spray.json._
import evcel.curve.curves.FuturesExpiryRule

object EventStoreJsonProtocol extends DefaultJsonProtocol {
  implicit object DayJsonFormat extends RootJsonFormat[Day] {
    def write(c: Day) =
      JsArray(JsNumber(c.year), JsNumber(c.month), JsNumber(c.dayNumber))

    def read(value: JsValue) = value match {
      case JsArray(JsNumber(y) :: JsNumber(m) :: JsNumber(d) :: Nil) =>
        Day(y.toInt, m.toInt, d.toInt)
      case _ => deserializationError("Day expected")
    }
  }
  implicit val uomRatioFormat = jsonFormat2(UOMRatio)
  implicit object UOMFormat extends RootJsonFormat[UOM] {
    // We write the string representation for auditing only
    def write(u: UOM) =
      JsArray(u.dimension.toJson, u.secondary.toJson)

    def read(value: JsValue) = value match {
      case JsArray(dimensionJson :: secondaryJson :: Nil) =>
        UOM(dimensionJson.convertTo[UOMRatio], secondaryJson.convertTo[UOMRatio])
      case _ => deserializationError("UOM expected")
    }
  }
  implicit object BDQtyFormat extends RootJsonFormat[BDQty] {
    // We write the string representation for auditing only
    def write(q: BDQty) =
      JsArray(JsNumber(q.bdValue), q.uom.toJson)

    def read(value: JsValue) = value match {
      case JsArray(JsNumber(n) :: uomJson :: Nil) =>
        Qty(n, uomJson.convertTo[UOM])
      case _ => deserializationError("BDQty expected")
    }
  }
  implicit object DayCountFormat extends RootJsonFormat[DayCount] {
    def write(dc: DayCount) = JsString(dc.name)
    def read(value: JsValue) = value match {
      case JsString(name) => DayCount.fromName(name)
      case _ => deserializationError("DayCount expected")
    }
  }
  implicit val monthFormat = jsonFormat2(Month.apply)
  implicit val futuresPricesFormat = jsonFormat3(FuturesPriceData)
  implicit val discountRateFormat = jsonFormat4(ZeroRateData)
  implicit val futuresVolFormat = jsonFormat3(FuturesVolData)
  implicit object MapMonthDayFormat extends RootJsonFormat[Map[Month, Day]] {
    def write(map: Map[Month, Day]) = JsArray(map.toList.map {
      case (k, v) => JsArray(monthFormat.write(k), DayJsonFormat.write(v))
    })
    def read(value: JsValue) = value match {
      case JsArray(pairs) =>
        Map[Month, Day]() ++ pairs.map {
          case JsArray(List(mJson, dJson)) => (mJson.convertTo[Month], dJson.convertTo[Day])
        }
      case other =>
        deserializationError("Map expected")
    }
  }

  implicit val futuresExpiryRuleFormat = jsonFormat2(FuturesExpiryRule.apply)
}
