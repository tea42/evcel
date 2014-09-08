package evcel.maths

sealed trait OptionRight {
  def payoff(S: Double, K: Double): Double
  def toChar = toString.head.toLower
  def isInTheMoney(S: Double, F: Double): Boolean = payoff(S, F) > 0
}

case object Call extends OptionRight {
  override def payoff(S: Double, K: Double) = math.max(S - K, 0)

  override def toString = "Call"
}

case object Put extends OptionRight {
  override def payoff(S: Double, K: Double) = math.max(K - S, 0)

  override def toString = "Put"
}

case object Straddle extends OptionRight {
  override def payoff(S: Double, K: Double) = math.max(K - S, S - K)

  override def toString = "Straddle"
}

object OptionRight {
  def apply(name: String): OptionRight = unapply(name).getOrElse(
    throw new RuntimeException("Invalid name for Call/Put/Straddle: " + name)
  )

  def unapply(right: String): Option[OptionRight] = right.toLowerCase.head match {
    case 'c' => Some(Call)
    case 'p' => Some(Put)
    case 's' => Some(Straddle)
    case _ => None
  }
}