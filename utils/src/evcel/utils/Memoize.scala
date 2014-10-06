package evcel.utils

import java.util.concurrent.{Callable, ConcurrentHashMap}

trait Cache {
  def name: String
  def memoize[K, V](key: K)(f: => V): V
}

case class CHMCache(name: String) extends Cache {
  private class CachedCallable(f: => AnyRef) extends Callable[AnyRef] {
    lazy val result = f
    override def call() = result
  }

  private val cache = new ConcurrentHashMap[AnyRef, CachedCallable]()

  override def memoize[K, V](key: K)(f:  => V): V = {
    def call = f.asInstanceOf[AnyRef]
    val callable = new CachedCallable(call)
    Option(cache.putIfAbsent(key.asInstanceOf[AnyRef], callable)).map(_.call().asInstanceOf[V]).getOrElse(f)
  }
}

object Cache{
  def createStaticCache(name: String) = {
    new CHMCache(name)
  }
}