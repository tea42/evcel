package evcel.pivot

import evcel.quantity.Qty
import evcel.daterange._
import scala.collection.immutable.{VectorBuilder, Nil}
import java.util.concurrent.atomic.AtomicLong
import evcel.utils.{EvcelFail, GeneralEvcelFail}
import scala.math.Ordered

trait PivotValue {
  def content : Seq[_]
  override def toString = content.size match {
    case 0 => ""
    case 1 => content.head.toString
    case n => n + " values"
  }
}

// Intended for values that simply don't exist - e.g. the exercise day
// of a Future.
object NullPivotValue extends PivotValue {
  def content = Nil
}

/**
  * Each `PivotValue` associated with any `PivotField` must be one of
  * - NullPivotValue
  * - ExceptionPivotValue
  * - That fields `GoodType`
  *
  * rules for sorting and merging are generic for the first two - the field
  * only needs to implement behaviour for handling `GoodType` values
  */
trait PivotField{

  type GoodType 
  case class Value(content : Seq[GoodType]) extends PivotValue{
    override def toString = formatMultipleValues(content)
  }
  def name: String

  def formatSingleValue(g : GoodType) = g.toString
  def formatMultipleValues(gs : Seq[GoodType]) = gs.size match {
      case 0 => ""
      case 1 => formatSingleValue(gs.head)
      case n => n + " values"
  }

  // This is the only public way of constructing `PivotValue` objects.
  def pivotValue(value : => GoodType) : PivotValue = {
    try{
      Value(Vector[GoodType](value))
    } catch {
      case e : Exception => ExceptionPivotValue(GeneralEvcelFail(e.getMessage))
    }
  }

  def pivotValue(valueOrFailure : Either[EvcelFail, GoodType]) : PivotValue = valueOrFailure.fold(
    ExceptionPivotValue(_),
    pivotValue(_)
  )

  // When merging 
  // - Any Exception value found becomes the merged result
  // - Null values are ignored
  // - good values are merged by the field
  protected def mergeGoodValues(seq : Iterable[GoodType]) : PivotValue 
  def merge(values : Iterable[PivotValue]) : PivotValue = {
    values.find(_.isInstanceOf[ExceptionPivotValue]).getOrElse{
      val goodValues = values.filterNot(_ == NullPivotValue).flatMap{
        _.content match {
          case goodValues : Seq[GoodType] => goodValues
          case o => sys.error(s"Unexpected value $o for field $name")
        }
      }
      if (goodValues.isEmpty)
        NullPivotValue
      else
        mergeGoodValues(goodValues)
    }
  }

  def mergedValue(table : PivotTable) = merge(table.pivotRows.map(_.pivotValue(this)))

  // When sorting
  // - Exceptions come first
  // - next are the good values 
  //    - single values are sorted by the field
  //    - multiple values are sorted by size - lexicographically if sizes match
  // - last are null values
  protected def singleValueComparator : PartialFunction[(Any, Any), Int] 
  val ordering = new Ordering[PivotValue](){
    def compare(l : PivotValue, r : PivotValue) = {
      (l, r) match {
        case (l : ExceptionPivotValue, r : ExceptionPivotValue) => l.compare(r)
        case (l : ExceptionPivotValue, _ ) => -1
        case (_, r : ExceptionPivotValue) => 1
        case (NullPivotValue, _) | (_, NullPivotValue) => r.content.size - l.content.size
        case _ => (l.content, r.content) match {
          case (Seq(l), Seq(r)) => 
            try {
              singleValueComparator((l, r))
            } catch {
              case _ : MatchError => 
                sys.error("Unexpected pivot value contents, $l, $r for this field $this")
            }
          case (l, r) => 
            if (l.size == r.size){
              // Collections of same size, so sort lexicographically
              l.view.zip(r.view).map{
                case (l_, r_) => singleValueComparator((l_, r_))
              }.find(_ != 0).getOrElse(0)
            } else {
              l.size - r.size
            }
        }
      }
    }
  }
  def sort(pvs : Seq[PivotValue]) : Seq[PivotValue] = {
    pvs.sorted(ordering)
  }
}

/**
  * Fields that aren't associated with quantities mostly merge values the same
  * way - taking the distinct union of the values
  */
trait DistinctMerge{
  self : PivotField =>
  protected def mergeGoodValues(seq : Iterable[self.GoodType]) : PivotValue = 
    self.Value(seq.toVector.distinct)

}

object QtyField{
  val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (l : Qty, r : Qty) => 
      if (l.uom == r.uom)
        l.compare(r)
      else
        l.uom.toString.compare(r.uom.toString)
  }
  def netByUOM(qs : Iterable[Qty]) = qs.groupBy(_.uom).values.map(Qty.sum)(scala.collection.breakOut)
}
  

case class SummingQtyField(name : String) extends PivotField {
  type GoodType = Qty
  protected val singleValueComparator = QtyField.singleValueComparator

  protected def mergeGoodValues(qs : Iterable[Qty]) = {
    Value(QtyField.netByUOM(qs))
  }
}

/**
  * For quantities associated with a particular id - typically a trade id
  * For any id there can be at most one quantity. 
  * When formatting - id's are dropped and the quentities netted
  * This allows us to (e.g.) repeat a trade's MTM across several rows, while summing
  * correctly
  */
case class IdSummingQtyField(name : String) extends PivotField with DistinctMerge{
  type GoodType = (Long, Qty)
  protected def valueConstructor = Value(_)
  def pivotValue(q : Qty) : PivotValue = pivotValue((IdSummingQtyField.nextID(), q))
  def pivotValueFromEither(e : Either[EvcelFail, Qty]) : PivotValue = e.fold(
    ExceptionPivotValue(_),
    {q : Qty => pivotValue((IdSummingQtyField.nextID(), q))}
  )
  val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case ((lId : Long, lQty : Qty), (rId : Long, rQty : Qty)) => 
      QtyField.singleValueComparator((lQty, rQty))
  }

  override def formatSingleValue(idQty : (Long, Qty)) = idQty._2.toString
  override def formatMultipleValues(qs : Seq[(Long, Qty)]) = QtyField.netByUOM(qs.map(_._2)) match {
    case Seq() => ""
    case Seq(q) => q.toString
    case seq => "${seq.size} values"
  }
}

object IdSummingQtyField{
  private val nextID_ = new AtomicLong(0)
  def nextID() = nextID_.getAndIncrement()
}


/**
  * Intended for quantities that can't be meaningfully summed.
  * Should be renamed if there are examples other than prices, 
  * although at the moment I can't think of any.
  */
case class PriceQtyField(name : String) extends PivotField 
  with DistinctMerge
{
  type GoodType = Qty
  protected val singleValueComparator = QtyField.singleValueComparator
}

case class OptionIntField(name : String) extends PivotField with DistinctMerge {
  type GoodType = Option[Int]
  protected val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (Some(l : Int), Some(r : Int)) => l - r
    case (l : Option[_], r : Option[_]) => r.size - l.size
  }
}

case class OptionDayField(name : String) extends PivotField with DistinctMerge{
  type GoodType = Option[Day]
  protected val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (Some(l : Day), Some(r : Day)) => l - r
    case (l : Option[_], r : Option[_]) => l.size - r.size
  }
}

case class DayField(name : String) extends PivotField with DistinctMerge {
  type GoodType = Day
  protected val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (l : Day, r : Day) => l - r
  }
}

case class PeriodField(name : String) extends PivotField with DistinctMerge{
  type GoodType = PeriodLabel
  protected val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (l : PeriodLabel, r : PeriodLabel) => PeriodLabel.ordering.compare(l, r)
  }
}

trait StringPivotFieldTrait extends PivotField with DistinctMerge{
  type GoodType = String
  protected val singleValueComparator : PartialFunction[(Any, Any), Int] = {
    case (l : String, r : String) => l.compare(r)
  }
}
case class StringPivotField(val name : String) extends StringPivotFieldTrait

// Same as StringPivotField - needed as a marker when collecting trade fields
// during report creation. Users can create arbitrary meta fields, so couldn't
// be known at compile time.
case class TradeMetaField(override val name : String) extends StringPivotFieldTrait

case class ExceptionPivotValue(e : EvcelFail) extends PivotValue with Ordered[ExceptionPivotValue]{
  val content = Vector(e)
  def compare(rhs : ExceptionPivotValue) = e.s.compare(rhs.e.s)
}

