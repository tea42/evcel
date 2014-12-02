package evcel.maths.models

import evcel.maths.{BarrierOut, BarrierIn, BarrierType, OptionRight}

import scala.collection.BitSet

object BarrierOptionMC {
  def valuation(right: OptionRight, barrier: BarrierType, s: Double, X: Double, h: Double, rebate: Double) = {
    new MCValuation {
      var barrierTriggered = BitSet()

      override def timeDependent: Boolean = true

      override def stepAndCheckCanTerminate(iteration: Int, timeStep: Int, St: Double) = {
        if (barrier.upOrDown.barrierTriggered(St, h)) {
          barrierTriggered = barrierTriggered + iteration
          true
        } else {
          false
        }
      }

      override def payoff(iteration: Int, St: Double): Double = {
        val triggered = barrierTriggered.contains(iteration)
        if ((triggered && barrier.inOrOut == BarrierIn) || (!triggered && barrier.inOrOut == BarrierOut))
          right.payoff(St, X)
        else
          rebate
      }
    }
  }
}
