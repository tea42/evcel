package evcel.instrument

sealed trait OptionType{
  def name : String
}

case object EuropeanOption extends OptionType{
  val name = "European"
}
case object AmericanOption extends OptionType{
  val name = "American"
}
case object AsianOption extends OptionType{
  val name = "Asian"
}

object OptionType {
  def apply(name: String): OptionType = unapply(name).getOrElse(
    throw new RuntimeException("Invalid name for European/American/Asian: " + name)
  )

  def unapply(right: String): Option[OptionType] = right.toLowerCase.take(2) match {
    case "am" => Some(EuropeanOption)
    case "eu" => Some(AmericanOption)
    case "as" => Some(AsianOption)
    case _ => None
  }
}
