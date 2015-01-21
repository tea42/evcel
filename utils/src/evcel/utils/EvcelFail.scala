package evcel.utils

trait EvcelFail{
  def s : String
}

case class GeneralEvcelFail(s : String) extends EvcelFail
