package evcel.daterange

trait TenorType {
  def name = getClass.getSimpleName.dropRight(1)
}

object TenorType {
  val values = List(None, Some(Month), Some(Day))
  def fromName(name:String) = name match {
    case "Day" => Day
    case "Month" => Month
    case o => throw new RuntimeException("Unknown tenor type: " + o)
  }
}