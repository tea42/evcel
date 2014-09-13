package evcel.utils

import scala.util.Either.RightProjection
import scala.language.implicitConversions

object EitherUtils {
  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right

  implicit class RightBiasedEitherPimp[L, R](e: Either[L, R]) {
    def getOrThrow(m: String) = if(e.isRight) e.right.get else sys.error(m)
    def getOrErrorLeft(f: L => String): R = if(e.isRight) e.right.get else sys.error(f(e.left.get))
    def recover(f: PartialFunction[L, R]): Either[L, R] = if(e.isLeft) Right(f(e.left.get)) else e
  }
}
