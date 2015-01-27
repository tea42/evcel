package evcel.referencedata

import evcel.utils.Enumerate

case class Level(name: String) {
  override def toString = name
}

object Level extends Enumerate[Level](classOf[Level], _.name :: Nil) {
  val Close = Level("Close")
  val Settlement = Level("Settlement")
  val High = Level("High")
}