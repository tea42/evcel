package evcel.utils

object StringUtils{
  def centreFill(text : String, width : Int) = {
    val rightFill = " " * ((width - text.size) / 2)
    val leftFill = " " * (width - text.size - rightFill.size)
    val res = leftFill + text + rightFill
    require (res.size == width)
    res
  }

  def rightJustify(text : String, width : Int) = {
    " " * (width - text.size) + text
  }
  def leftJustify(text : String, width : Int) = {
    text + " " * (width - text.size) 
  }
  def maxSize(strings : Seq[String]) = strings.foldLeft(0)(_ max _.size)
}
