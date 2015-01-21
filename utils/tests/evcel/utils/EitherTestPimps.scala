package evcel.utils

trait EitherTestPimps{
  implicit class RichEither[A, B](e : Either[A, B]){
    def R : B = e.right.get
  }
}
