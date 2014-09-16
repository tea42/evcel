package evcel.eventstore.json

import evcel.curve.curves.FuturesExpiryRule
import evcel.curve.environment._
import evcel.curve.marketdata._
import evcel.daterange.{SimpleDateRange, DateRange, Day, Month}
import evcel.quantity.{BDQty, Qty, UOM, UOMRatio}
import spray.json._

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

  implicit object DateRangeFormat extends RootJsonFormat[DateRange]{
    override def write(dateRange: DateRange): JsValue = {
      dateRange match {
        case day : Day =>
            JsArray(DateRange.DAY.toJson, day.toJson)
        case month : Month =>
          JsArray(DateRange.MONTH.toJson, month.toJson)
        case simple : SimpleDateRange =>
          JsArray(DateRange.SIMPLE.toJson, simple.firstDay.toJson, simple.lastDay.toJson)

        case _                        =>        throw new RuntimeException("Unrecognized date range " + dateRange)
      }
    }

    override def read(json: JsValue): DateRange = {
      json match {
        case JsArray(JsString(DateRange.DAY)::json::Nil) => json.convertTo[Day]
        case JsArray(JsString(DateRange.MONTH)::json::Nil) => json.convertTo[Month]
        case JsArray(JsString(DateRange.SIMPLE)::fromJson::toJson::Nil) => 
          SimpleDateRange(fromJson.convertTo[Day], toJson.convertTo[Day])

        case _                                                => 
          throw new RuntimeException("Unrecognized date range json " + json)
      }
    }
  }
  implicit val zeroRateIdentifierFormat = jsonFormat1(ZeroRatesIdentifier)
  implicit val futuresPricesIdentifierFormat = jsonFormat1(FuturesPricesIdentifier)
  implicit val futuresVolsIdentifierFormat = jsonFormat1(FuturesVolsIdentifier)
  implicit val spotPricesIdentifierFormat = jsonFormat1(SpotPricesIdentifier)
  implicit object MarketDataIdentifierFormat extends RootJsonFormat[MarketDataIdentifier]{
    import MarketData._
    override def write(identifier: MarketDataIdentifier): JsValue = identifier match {
      case id : FuturesPricesIdentifier   => JsArray(JsString(FUTURES_PRICES), id.toJson)
      case id : FuturesVolsIdentifier     => JsArray(JsString(FUTURES_VOLS), id.toJson)
      case id : ZeroRatesIdentifier   => JsArray(JsString(ZERO_RATES), id.toJson)
      case id : SpotPricesIdentifier      => JsArray(JsString(SPOT_PRICES), id.toJson)

      case _                        =>        
        throw new RuntimeException("Unrecognized market data identifier " + identifier)
    }

    override def read(json: JsValue): MarketDataIdentifier = {
      json match {
        case JsArray(JsString(FUTURES_PRICES) ::json_  ::Nil) => json_.convertTo[FuturesPricesIdentifier]
        case JsArray(JsString(ZERO_RATES)     ::json_  ::Nil) => json_.convertTo[ZeroRatesIdentifier]
        case JsArray(JsString(FUTURES_VOLS)   ::json_  ::Nil) => json_.convertTo[FuturesVolsIdentifier]
        case JsArray(JsString(SPOT_PRICES)    ::json_  ::Nil) => json_.convertTo[SpotPricesIdentifier]

        case _                                                => 
          throw new RuntimeException("Unrecognized market data identifier json " + json)
      }
    }
  }

  implicit val futuresPricesFormat = jsonFormat1(FuturesPriceData.apply)
  implicit val discountRateFormat = jsonFormat2(ZeroRateData)
  implicit val futuresVolFormat = jsonFormat1(FuturesVolData)
  implicit val spotPriceDataFormat = jsonFormat1(SpotPriceData)

  implicit object MarketDataFormat extends RootJsonFormat[MarketData]{
    import MarketData._
    override def write(data: MarketData): JsValue = data match {
      case fp : FuturesPriceData    =>        JsArray(JsString(FUTURES_PRICES), fp.toJson)
      case zr : ZeroRateData        =>        JsArray(JsString(ZERO_RATES), zr.toJson)
      case fv : FuturesVolData      =>        JsArray(JsString(FUTURES_VOLS), fv.toJson)
      case sp : SpotPriceData       =>        JsArray(JsString(SPOT_PRICES), sp.toJson)

      case _                        =>        throw new RuntimeException("Unrecognized market data type " + data)
    }

    override def read(json: JsValue): MarketData = json match {
      case JsArray(JsString(FUTURES_PRICES) ::json_  ::Nil) => json_.convertTo[FuturesPriceData]
      case JsArray(JsString(ZERO_RATES)     ::json_  ::Nil) => json_.convertTo[ZeroRateData]
      case JsArray(JsString(FUTURES_VOLS)   ::json_  ::Nil) => json_.convertTo[FuturesVolData]
      case JsArray(JsString(SPOT_PRICES)    ::json_  ::Nil) => json_.convertTo[SpotPriceData]

      case _                                                => 
        throw new RuntimeException("Unrecognized market data json " + json)
    }
  }
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
  implicit val futuresExpiryRuleFormat = jsonFormat3(FuturesExpiryRule.apply)
}
