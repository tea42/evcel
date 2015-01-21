package evcel.report

import evcel.pivot._
import evcel.instrument._

case class PivotInstrument(instrument : Instrument) extends PivotRow{
  def fields : Seq[PivotField] = PivotInstrument.instrumentTypeFields(instrument.instrumentType)

  // Somewhat ugly that we are relying on the pivot fields to be listed in the same
  // order as the member variables - however should fail immediately if this isn't the case
  @transient private lazy val pivotValuesMap : Map[PivotField, PivotValue] = 
    fields.zip(instrument.productIterator.toList).map{
      case (field, any) => field -> field.pivotValue(any.asInstanceOf[field.GoodType])
    }.toMap
  def pivotValue(field : PivotField) = pivotValuesMap(field)
}

object PivotInstrument{
  

  val MARKET_FIELD = StringPivotField("Market")
  val INDEX_FIELD = StringPivotField("Index")
  val PERIOD_FIELD = PeriodField("Period")
  val STRIKE_FIELD = PriceQtyField("Strike")
  val VOLUME_FIELD = SummingQtyField("Volume")
  val DAYS_TO_SETTLEMENT = OptionIntField("Days to settlement")
  val OPTION_RIGHT_FIELD = StringPivotField("Right")
  val OPTION_TYPE_FIELD = StringPivotField("Option Type")
  val CASH_SETTLED = StringPivotField("Cash Settled")
  val CUSTOM_EXPIRY = OptionDayField("Custom Expiry")
  val PRICING_RULE_FIELD = StringPivotField("Pricing Rule")
  val DELIVERY_FIELD = OptionDayField("Delivery Day")

  val FIELDS = Set[PivotField](
    MARKET_FIELD, INDEX_FIELD, PERIOD_FIELD,
    STRIKE_FIELD, VOLUME_FIELD, DAYS_TO_SETTLEMENT,
    OPTION_RIGHT_FIELD, OPTION_TYPE_FIELD, CASH_SETTLED,
    CUSTOM_EXPIRY, PRICING_RULE_FIELD
  )
  val instrumentTypeFields : Map[InstrumentType, Seq[PivotField]] = Map(
    Future -> 
      Vector(MARKET_FIELD, PERIOD_FIELD, STRIKE_FIELD, VOLUME_FIELD),

    FuturesOption -> 
      Vector[PivotField](
        MARKET_FIELD, PERIOD_FIELD, STRIKE_FIELD, VOLUME_FIELD,
        OPTION_RIGHT_FIELD, OPTION_TYPE_FIELD, CASH_SETTLED,
        DAYS_TO_SETTLEMENT, CUSTOM_EXPIRY
      ),

    CommoditySwap -> 
      Vector[PivotField](INDEX_FIELD, PERIOD_FIELD, STRIKE_FIELD, VOLUME_FIELD, DAYS_TO_SETTLEMENT),

    CommoditySwapSpread ->  
      Vector[PivotField](
        MARKET_FIELD, PERIOD_FIELD, STRIKE_FIELD,
        VOLUME_FIELD, PRICING_RULE_FIELD,
        DAYS_TO_SETTLEMENT
      ),

    CommoditySwapLookalike -> 
      Vector[PivotField](
        MARKET_FIELD, PERIOD_FIELD, STRIKE_FIELD, VOLUME_FIELD,
        DAYS_TO_SETTLEMENT
      ),

    Cash ->
      Vector(VOLUME_FIELD, DELIVERY_FIELD)
  )
}
