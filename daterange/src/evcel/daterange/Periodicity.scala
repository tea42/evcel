package evcel.daterange

sealed trait Periodicity {
  def years: Double
}

case object Continuously extends Periodicity {
  override def years: Double = 0.0
}

case object Hourly extends Periodicity {
  override def years: Double = 1.0 / (24 * 365)
}

case object Daily extends Periodicity {
  override def years: Double = 1.0 / 365
}

case object Weekly extends Periodicity {
  override def years: Double = 1.0 / 52
}

case object Monthly extends Periodicity {
  override def years: Double = 1.0 / 12
}
