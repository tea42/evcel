package evcel.maths

case class BarrierType(upOrDown: UpOrDown, inOrOut: InOrOut)

object BarrierType {
  val DownAndIn = new BarrierType(BarrierDown, BarrierIn)
  val UpAndIn = new BarrierType(BarrierUp, BarrierIn)
  val DownAndOut = new BarrierType(BarrierDown, BarrierOut)
  val UpAndOut = new BarrierType(BarrierUp, BarrierOut)
}

sealed trait UpOrDown {
  def barrierTriggered(S: Double, H: Double): Boolean
}

case object BarrierUp extends UpOrDown {
  override def barrierTriggered(S: Double, H: Double) = S >= H
}

case object BarrierDown extends UpOrDown {
  override def barrierTriggered(S: Double, H: Double) = S <= H
}

sealed trait InOrOut

case object BarrierIn extends InOrOut

case object BarrierOut extends InOrOut
