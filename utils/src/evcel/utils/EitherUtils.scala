package evcel.utils

import scala.util.Either.RightProjection
import scala.language.implicitConversions

object EitherUtils {
  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right

  implicit class RightBiasedEitherPimp[L, R](e: Either[L, R]) {
    def getOrThrow(m: String) = if(e.isRight) e.right.get else sys.error(m)
    def getOrErrorLeft(f: L => String): R = if(e.isRight) e.right.get else sys.error(f(e.left.get))
    def recover(f: PartialFunction[L, R]): Either[L, R] = if(e.isLeft) Right(f(e.left.get)) else e
    def orElse(x : Either[L, R]) = if (e.isLeft) x else e
  }

  def mapOrErrorLeft[A, L, R] (seq : Seq[A], fn : A => Either[L, R]) : Either[L, Seq[R]] = {
    val acc = new scala.collection.immutable.VectorBuilder[R]()
    var maybeFailure : Option[L] = None
    seq.foreach{
      a => 
        if (!maybeFailure.isDefined){
          fn(a) match {
            case Left(l) => 
              maybeFailure = Some(l)
            case Right(r) => 
              acc += r
          }
        }
    }
    maybeFailure.toLeft(acc.result)
  }
}
