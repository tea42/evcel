package evcel.eventstore.json

import evcel.curve.environment._
import evcel.curve.marketdata._
import evcel.daterange.{DateRange, Day, Month, SimpleDateRange}
import evcel.instrument._
import evcel.instrument.trade.Trade
import evcel.instrument.valuation.{CommonSwapPricingRule, NonCommonSwapPricingRule, SwapPricingRule}
import evcel.maths.{OptionType, OptionRight}
import evcel.quantity.{BDQty, Qty, QtyConversions, UOM, UOMRatio}
import evcel.referencedata.{CalendarIdentifier, FuturesExpiryRule, FuturesExpiryRuleIdentifier, ReferenceDataIdentifier, ReferenceDataTrait}
import evcel.referencedata.calendar.CalendarData
import evcel.referencedata.market.{Currency, CurrencyIdentifier, FuturesMarket, FuturesMarketIdentifier}
import spray.json._
import evcel.instrument.Tradeable
import evcel.instrument.FXForward

object EventStoreJsonProtocol extends DefaultJsonProtocol {
  abstract class NamedFormat[T] extends RootJsonFormat[T] {
    val name: String
  }
 
  // augments a format with type information, allowing
  // hierarchies to be unmarshalled
  def named[T](n: String, orig: RootJsonFormat[T]) = new NamedFormat[T] {
    val name = n
  
    def write(obj: T): JsValue = JsArray(JsString(name), orig.write(obj))
  
    def read(json: JsValue): T = json match {
      case JsArray(JsString(c) :: json :: Nil) if c == name =>
        orig.read(json)
      case _ => deserializationError(json.toString)
    }
  }

  case class TraitFormat[T](formats : (Class[_], NamedFormat[_ <: T])*) extends RootJsonFormat[T]{
    private val byName : Map[String, NamedFormat[_ <: T]] = formats.toMap.values.toList.map{
      fmt => (fmt.name, fmt)
    }.toMap
    private val byClass = formats.toMap
    
    override def write(obj : T): JsValue = {
      // pure evil which Miles will conquer shortly
      val format = byClass(obj.getClass).asInstanceOf[NamedFormat[T]]
      format.write(obj)
    }
    override def read(json: JsValue): T = {
      json match {
        case JsArray(JsString(name)::_) => byName(name).read(json)
        case _ => deserializationError(s"Trait expected - got $json")
      }
    }
  }

  implicit val dayFormat : NamedFormat[Day] = named(DateRange.DAY, new RootJsonFormat[Day]{
    def write(c: Day) =
      JsArray(JsNumber(c.year), JsNumber(c.month), JsNumber(c.dayNumber))

    def read(value: JsValue) = value match {
      case JsArray(JsNumber(y) :: JsNumber(m) :: JsNumber(d) :: Nil) =>
        Day(y.toInt, m.toInt, d.toInt)
      case _ => deserializationError("Day expected")
    }
  })

  implicit val simpleDateRangeFormat : NamedFormat[SimpleDateRange] = named(DateRange.SIMPLE, jsonFormat2(SimpleDateRange))
  implicit val monthFormat : NamedFormat[Month] = named(DateRange.MONTH, jsonFormat2(Month.apply))

  implicit val dateRangeFormat :TraitFormat[DateRange] = new TraitFormat[DateRange](
    classOf[Day] -> dayFormat,
    classOf[Month] -> monthFormat,
    classOf[SimpleDateRange] -> simpleDateRangeFormat
  )

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


  implicit val futuresPricesIdentifierFormat  = named(MarketData.FUTURES_PRICES, jsonFormat1(FuturesPricesIdentifier))
  implicit val futuresVolsIdentifierFormat    = named(MarketData.FUTURES_VOLS, jsonFormat1(FuturesVolsIdentifier))
  implicit val zeroRateIdentifierFormat       = named(MarketData.ZERO_RATES, jsonFormat1(ZeroRatesIdentifier))
  implicit val spotFXIdentifierFormat       = named(MarketData.SPOT_FX, jsonFormat2(SpotFXIdentifier))
  implicit val spotPricesIdentifierFormat     = named(MarketData.SPOT_PRICES, jsonFormat1(SpotPricesIdentifier))

  implicit val marketDataIdentifierFormat : TraitFormat[MarketDataIdentifier] = 
    new TraitFormat[MarketDataIdentifier]( 
      classOf[FuturesPricesIdentifier] -> futuresPricesIdentifierFormat,
      classOf[FuturesVolsIdentifier] -> futuresVolsIdentifierFormat,
      classOf[ZeroRatesIdentifier] -> zeroRateIdentifierFormat,
      classOf[SpotFXIdentifier] -> spotFXIdentifierFormat,
      classOf[SpotPricesIdentifier] -> spotPricesIdentifierFormat
    )

  implicit val futuresPricesFormat = named(MarketData.FUTURES_PRICES, jsonFormat1(FuturesPriceData.apply))
  implicit val discountRateFormat = named(MarketData.ZERO_RATES, jsonFormat2(ZeroRateData.apply))
  implicit val spotFXFormat = named(MarketData.SPOT_FX, jsonFormat1(SpotFXData.apply))
  implicit val futuresVolFormat = named(MarketData.FUTURES_VOLS, jsonFormat1(FuturesVolData.apply))
  implicit val spotPriceDataFormat = named(MarketData.SPOT_PRICES, jsonFormat1(SpotPriceData))

  implicit val marketDataFormat = new TraitFormat[MarketData](
    classOf[FuturesPriceData] -> futuresPricesFormat,
    classOf[FuturesVolData] -> futuresVolFormat,
    classOf[ZeroRateData] -> discountRateFormat,
    classOf[SpotFXData] -> spotFXFormat,
    classOf[SpotPriceData] -> spotPriceDataFormat
  )

  implicit object MapMonthDayFormat extends RootJsonFormat[Map[Month, Day]] {
    def write(map: Map[Month, Day]) = JsArray(map.toList.map {
      case (k, v) => JsArray(monthFormat.write(k), dayFormat.write(v))
    })
    def read(value: JsValue) = value match {
      case JsArray(pairs) =>
        Map[Month, Day]() ++ pairs.map {
          case JsArray(List(mJson, dJson)) => (mJson.convertTo[Month], dJson.convertTo[Day])
        }
      case _ =>
        deserializationError("Map expected")
    }
  }

  implicit object optionRightFormat extends RootJsonFormat[OptionRight]{
    override def write(right : OptionRight): JsValue = JsString(right.toString)
    override def read(json: JsValue): OptionRight = json match {
      case JsString(name) => OptionRight(name)
      case _ => 
        deserializationError(s"Option right expected - got $json")
    }
  }

  implicit object optionTypeFormat extends RootJsonFormat[OptionType]{
    override def write(right : OptionType) = JsString(right.name)
    override def read(json: JsValue): OptionType = json match {
      case JsString(name) => OptionType(name)
      case _ => 
        deserializationError(s"Option type expected - got $json")
    }
  }

  implicit val swapPricingRuleFormat = new RootJsonFormat[SwapPricingRule]{
    def write(pr : SwapPricingRule) = pr match {
      case CommonSwapPricingRule            => JsString("CommonSwapPricingRule")
      case NonCommonSwapPricingRule         => JsString("NonCommonSwapPricingRule")
    }
    def read(json : JsValue) = json match {
      case JsString("CommonSwapPricingRule")            => CommonSwapPricingRule
      case JsString("NonCommonSwapPricingRule")         => NonCommonSwapPricingRule
      case _ => 
        deserializationError(s"SwapPricingRule expected - got $json")
    }
  }
  implicit val futureFormat = named(Instrument.FUTURE, jsonFormat4(Future.apply))
  implicit val futuresOptionFormat = named(Instrument.FUTURES_OPTION, jsonFormat9(FuturesOption.apply))
  implicit val commoditySwapFormat = named(Instrument.COMMODITY_SWAP, jsonFormat5(CommoditySwap.apply))
  implicit val commoditySwapSpreadFormat = named(
    Instrument.COMMODITY_SWAP_SPREAD, jsonFormat6(CommoditySwapSpread.apply)
  )
  implicit val commoditySwapLookalikeFormat = named(
    Instrument.COMMODITY_SWAP_LOOKALIKE, jsonFormat5(CommoditySwapLookalike.apply)
  )
  implicit val fxForwardFormat = named(Instrument.FX_FORWARD, jsonFormat3(FXForward.apply))
  implicit val cashFormat = named(Instrument.CASH, jsonFormat2(Cash.apply))

  implicit val tradeableFormat = new TraitFormat[Tradeable](
    classOf[Future] -> futureFormat, 
    classOf[FuturesOption] -> futuresOptionFormat,
    classOf[CommoditySwap] -> commoditySwapFormat,
    classOf[CommoditySwapSpread] -> commoditySwapSpreadFormat,
    classOf[CommoditySwapLookalike] -> commoditySwapLookalikeFormat,
    classOf[FXForward] -> fxForwardFormat,
    classOf[Cash] -> cashFormat
  )
  implicit val instrumentFormat = new TraitFormat[Instrument](
    classOf[Future] -> futureFormat, 
    classOf[FuturesOption] -> futuresOptionFormat,
    classOf[CommoditySwap] -> commoditySwapFormat,
    classOf[CommoditySwapSpread] -> commoditySwapSpreadFormat,
    classOf[CommoditySwapLookalike] -> commoditySwapLookalikeFormat,
    classOf[FXForward] -> fxForwardFormat,
    classOf[Cash] -> cashFormat
  )

  implicit object TradeFormat extends RootJsonFormat[Trade]{
    def write(trade : Trade) = JsArray(
      trade.id.toJson, 
      trade.tradeDay.toJson,
      trade.counterparty.toJson, 
      trade.tradeable.toJson,
      trade.meta.toList.toJson
    )
    def read(value: JsValue) = value match{
      case JsArray(idJson :: dayJson :: cptyJson :: instJson :: metaJson :: Nil) =>
        Trade(
          idJson.convertTo[String],
          dayJson.convertTo[Day],
          cptyJson.convertTo[String],
          instJson.convertTo[Tradeable],
          metaJson.convertTo[List[(String, String)]].toMap
        )
      case _ => 
        deserializationError(s"TradeFormat expected - got $value")
    }
  }

  implicit val calendarIdentifierFormat = named(ReferenceDataTrait.CALENDAR, jsonFormat1(CalendarIdentifier))
  implicit val futuresExpiryRuleIdentifierFormat = named(
    ReferenceDataTrait.FUTURES_EXPIRY_RULE, jsonFormat1(FuturesExpiryRuleIdentifier))
  implicit val futuresMarketIdentifierFormat = named(ReferenceDataTrait.FUTURES_MARKET, jsonFormat1(FuturesMarketIdentifier))
  implicit val currencyIdentifierFormat = named(ReferenceDataTrait.CURRENCY, jsonFormat1(CurrencyIdentifier))
  implicit val referenceDataIdentifierFormat = new TraitFormat[ReferenceDataIdentifier](
    classOf[CalendarIdentifier] -> calendarIdentifierFormat,
    classOf[FuturesExpiryRuleIdentifier] -> futuresExpiryRuleIdentifierFormat,
    classOf[CurrencyIdentifier] -> currencyIdentifierFormat,
    classOf[FuturesMarketIdentifier] -> futuresMarketIdentifierFormat
  )

  implicit object QtyConversionsFormat extends RootJsonFormat[QtyConversions]{
    def write(qc : QtyConversions) = qc.rates.toList.toJson
    def read(json : JsValue) = QtyConversions(json.convertTo[List[((UOM, UOM), BigDecimal)]].toMap)
  }
  implicit val futuresExpiryRuleFormat = named(ReferenceDataTrait.FUTURES_EXPIRY_RULE, jsonFormat3(FuturesExpiryRule.apply))
  implicit val futuresMarketRuleFormat = named(ReferenceDataTrait.FUTURES_MARKET, jsonFormat5(FuturesMarket.apply))
  implicit val calendarDataFormat = named(ReferenceDataTrait.CALENDAR, jsonFormat1(CalendarData))
  implicit val currencyFormat = named(ReferenceDataTrait.CURRENCY, jsonFormat5(Currency.apply))
  implicit val referenceDataTraitFormat = new TraitFormat[ReferenceDataTrait](
    classOf[FuturesExpiryRule] -> futuresExpiryRuleFormat,
    classOf[CalendarData] -> calendarDataFormat,
    classOf[Currency] -> currencyFormat,
    classOf[FuturesMarket] -> futuresMarketRuleFormat
  )
}
